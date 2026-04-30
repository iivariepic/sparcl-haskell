module Language.Sparcl.Compiler.Haskell where

import qualified Language.Sparcl.Core.Syntax as Core
import qualified Language.Sparcl.Name as Name
import qualified Language.Sparcl.Literal as Literal
import           Language.Sparcl.Pretty (prettyShow)
import           Data.List
import           Data.Char (toUpper)
import           Language.Sparcl.Typing.Type (Ty(..), QualTy(..), pattern (:-@), PolyTy)

-- Helper function to wrap symboling operators in parentheses
formatName :: String -> String
formatName s
    | not (null s) && all (`elem` "!#$%&*+./<=>?@\\^|-~:") s = "(" ++ s ++ ")"
    | otherwise = s

-- Literals
compileLiteral :: Literal.Literal -> String
compileLiteral l = case l of
    Literal.LitInt i    -> show i
    _                   -> error "compileLiteral: Unhandled literal type"

-- Helper function to check if binding is reversible
isReversible :: Ty -> Bool
isReversible ty = case ty of
    -- Check for -o linear arrow
    (_ :-@ _) -> True
    -- Check for 'rev' keyword
    TyCon c _ | prettyShow c == "rev" -> True
    -- Peel Check inner type for forall/polymorphism wrappers
    TyForAll _ (TyQual _ innerTy) -> isReversible innerTy
    -- Check inner type for type synonyms
    TySyn _ innerTy -> isReversible innerTy
    _ -> False

-- Top-level Bindings
compileBinding :: [(Name.Name, PolyTy)] -> [String] -> (Name.Name, Ty, Core.Exp Name.Name) -> String
compileBinding typeMap revNames (name, ty, expr) =
  let rawName = prettyShow name
      nameStr = formatName rawName
  in if isReversible ty
    then
        let fwdCode = compileForward revNames expr
            bwdCode = compileBackward typeMap revNames expr
        in nameStr ++ " = (" ++ fwdCode ++ ", " ++ bwdCode ++ ")"
    else
        let code = compileForward revNames expr
        in nameStr ++ " = " ++ code

-- Patterns
compilePattern :: Core.Pat Name.Name -> String
compilePattern pat = case pat of
    Core.PVar n -> prettyShow n
    Core.PCon c ps ->
        let cName = translateConName (prettyShow c)
            psCode = map compilePattern ps
        in  if null psCode
            then cName
            else "(" ++ cName ++ " " ++ unwords psCode ++ ")"

-- Helper function to translate internal constructor names
translateConName :: String -> String
translateConName name

    | Just rest <- stripPrefix "<Tup " name =
        let n = read (init rest) :: Int
        in "(" ++ replicate (n - 1) ',' ++ ")"

    | otherwise = name

-- Compiling unidirectional or forward functions
compileForward :: [String] -> Core.Exp Name.Name -> String
compileForward revNames expr = case expr of
    -- Literal values
    Core.Lit l -> compileLiteral l

    -- Variables
    Core.Var n ->
        let rawName = prettyShow n
            nameStr = formatName rawName
        in if nameStr `elem` revNames
            then "(fst " ++ nameStr ++ ")"
            else nameStr

    -- Lambda abstractions
    Core.Abs n e ->
        let varName  = prettyShow n
            bodyCode = compileForward revNames e
        in "(\\" ++ varName ++ " -> " ++ bodyCode ++ ")"

    -- Data constructors
    Core.Con c es -> compileConstructor c es
    Core.RCon c es -> compileConstructor c es

    -- Let bindings
    Core.Let binds body ->
        let compileBind (n, _ty, e) = prettyShow n ++ " = " ++ compileForward revNames e
            bindsCode = map compileBind binds
            bindsString = intercalate "; " bindsCode
        in "(let { " ++ bindsString ++ " } in " ++ compileForward revNames body ++ ")"

    -- Case expressions
    Core.Case e alts   -> compileCase e alts
    Core.RCase e rAlts ->
        let alts = map (\(p, body, _pin) -> (p, body)) rAlts
        in compileCase e alts

    -- RPin
    Core.RPin e1 _e2 ->
        compileForward revNames e1

    -- Lift
    Core.Lift e1 e2 ->
        let fwdCode = compileForward revNames e1
            bwdCode = compileForward revNames e2
        in "(" ++ fwdCode ++ ", " ++ bwdCode ++ ")"

    -- Unlift
    Core.Unlift e -> compileForward revNames e

    -- Recursive compilation of the entire function App
    Core.App e1 e2 ->
        let
            code1 = compileForward revNames e1
            code2 = compileForward revNames e2
        in "(" ++ code1 ++ " " ++ code2 ++ ")"

    where
        -- Shared logic for forward and unidirectional constructors
        compileConstructor c es =
            let cName = translateConName (prettyShow c)
                args = map (compileForward revNames) es
            in if null args
                then cName
                else "(" ++ cName ++ " " ++ unwords args ++ ")"

        -- Shared logic for forward and unidirectional cases
        compileCase e alts =
            let scrutinee = compileForward revNames e
                compileAlt (p, body) =
                    "  " ++ compilePattern p ++ " -> " ++ compileForward revNames body
                altsCode = map compileAlt alts
            in "(case " ++ scrutinee ++ " of {\n" ++ intercalate ";\n" altsCode ++ "})"

-- Helper function to invert a forward expression into a backward pattern
-- Returns the pattern string and a function to wrap the body in let-bindings
invertRHS :: [(Name.Name, PolyTy)] -> [String] -> Core.Exp Name.Name -> (String, String -> String)
invertRHS typeMap revNames expr = case expr of
    Core.Var n -> (formatName (prettyShow n), id)

    Core.RCon c es ->
        let cName = translateConName (prettyShow c)
            invertedArgs = map (invertRHS typeMap revNames) es
            argPats = map fst invertedArgs
            modifier = foldr (((.)) . snd) id invertedArgs
            patStr = if null argPats
                     then cName
                     else "(" ++ cName ++ " " ++ unwords argPats ++ ")"
        in (patStr, modifier)

    Core.RPin e1 _e2 ->
        invertRHS typeMap revNames e1

    Core.Con c es ->
        let cName = translateConName (prettyShow c)
            invertedArgs = map (invertRHS typeMap revNames) es
            argPats = map fst invertedArgs
            modifier = foldr (((.)) . snd) id invertedArgs
            patStr = if null argPats
                     then cName
                     else "(" ++ cName ++ " " ++ unwords argPats ++ ")"

        in (patStr, modifier)

    Core.Let binds body ->
        let (bodyPat, bodyMod) = invertRHS typeMap revNames body
            invertBind (n, _ty, e) =
                let (ePat, eMod) = invertRHS typeMap revNames e
                    nName = prettyShow n
                    bindMod rhs = "(let " ++ ePat ++ " = " ++ nName ++ " in " ++ eMod rhs ++ ")"
                in bindMod
            bindMods = map invertBind (reverse binds)
            totalMod = foldr (.) id (bodyMod : bindMods)
        in (bodyPat, totalMod)

    Core.App _ _ ->
        let
            -- flatten nested apps (apps with multiple inputs)
            unrollApp (Core.App e1 e2) =
                let (innerF, innerArgs) = unrollApp e1
                in  (innerF, innerArgs ++ [e2])
            unrollApp e = (e, [])

        in case unrollApp expr of
            (Core.Var f, args) ->
                let fName = prettyShow f
                    formattedFName = formatName fName
                    yName = "_y_" ++ fName -- safe variable name for the pattern

                    bwdCall = if fName `elem` revNames
                          then "(snd " ++ formattedFName ++ ")"
                          else fName

                    -- lookup function type in typemap
                    fTy = case lookup f typeMap of
                            Just ty -> ty
                            Nothing -> error $ "Type not found for function: " ++ fName

                    -- determine reversibility of each function argument
                    typeStr = prettyShow fTy

                    getArgFlags :: String -> [Bool]
                    getArgFlags [] = []
                    getArgFlags s =
                        let rest = dropWhile (/= '-') s
                        in if "-o" `isPrefixOf` rest
                            then True : getArgFlags (drop 2 rest)
                            else if "->" `isPrefixOf` rest
                            then False : getArgFlags (drop 2 rest)
                            else if null rest then [] else getArgFlags (drop 1 rest)

                    isRevArgs = getArgFlags typeStr

                    -- separate arguments based on their reversibility flag
                    zippedArgs = zip args (isRevArgs ++ repeat False)

                    fwdArgs = [ arg | (arg, isRev) <- zippedArgs, not isRev ]
                    revArgs = [ arg | (arg, isRev) <- zippedArgs, isRev ]

                    -- forward context arguments
                    fwdArgsCode = map (compileForward revNames) fwdArgs
                    callStr = if null fwdArgsCode
                              then bwdCall ++ " " ++ yName
                              else bwdCall ++ " " ++ unwords fwdArgsCode ++ " " ++ yName

                    -- reversible variables
                    getVarName :: Core.Exp Name.Name -> String
                    getVarName (Core.Var n) = formatName (prettyShow n)
                    getVarName _            = "error_expected_var"

                    revArgsNames = map getVarName revArgs
                    recPat = case revArgsNames of
                                 [single] -> single
                                 many -> "(" ++ intercalate ", " many ++ ")"

                    modifier rhs = "(let " ++ recPat ++ " = " ++ callStr ++ " in " ++ rhs ++ ")"

                in (yName, modifier)
            _ -> ("error \"App head is not a variable\"", id)

    _ -> ("error \"Inversion not implemented\"", id)


-- Compiling backward functions
compileBackward :: [(Name.Name, PolyTy)] -> [String] -> Core.Exp Name.Name -> String
compileBackward typeMap revNames expr = case expr of
    -- Literal values
    Core.Lit l -> compileLiteral l

    -- Variables
    Core.Var n ->
        let rawName = prettyShow n
            nameStr = formatName rawName
        in if nameStr `elem` revNames
            then "(snd " ++ nameStr ++ ")"
            else nameStr

    -- Lambda abstractions
    Core.Abs n e ->
        let varName  = prettyShow n
            bodyCode = compileBackward typeMap revNames e
        in "(\\" ++ varName ++ " -> " ++ bodyCode ++ ")"

    -- Data constructors (backwards unimplemented)
    Core.Con c es -> compileConstructor c es
    Core.RCon c es -> compileConstructor c es

    -- Let bindings
    Core.Let binds body ->
        let compileBind (n, _ty, e) = prettyShow n ++ " = " ++ compileBackward typeMap revNames e
            bindsCode = map compileBind binds
            bindsString = intercalate "; " bindsCode
        in "(let { " ++ bindsString ++ " } in " ++ compileBackward typeMap revNames body ++ ")"

    -- Case expressions
    Core.Case e alts   -> compileCase e alts
    Core.RCase e rAlts ->
        let scrutinee = compileBackward typeMap revNames e

            -- Helper function to swap forward output and forward pattern
            compileBwdAlt (fwdPat, fwdBody, _pin) =
                let (patStr, modifier) = invertRHS typeMap revNames fwdBody
                    bwdRhs = modifier (compilePattern fwdPat)
                in "  " ++ patStr ++ " -> " ++ bwdRhs

            altsCode = map compileBwdAlt rAlts
        in "(case " ++ scrutinee ++ " of {\n" ++ intercalate ";\n" altsCode ++ "})"

    -- RPin
    Core.RPin e1 _e2 ->
        compileBackward typeMap revNames e1

    -- Lift (backwards unimplemented)
    Core.Lift e1 e2 ->
        let fwdCode = compileBackward typeMap revNames e1
            bwdCode = compileBackward typeMap revNames e2
        in "(" ++ fwdCode ++ ", " ++ bwdCode ++ ")"

    -- Unlift (backwards unimplemented)
    Core.Unlift e -> compileBackward typeMap revNames e

    -- Recursive compilation of the entire function App (backwards unimplemented)
    Core.App e1 e2 ->
        let
            code1 = compileBackward typeMap revNames e1
            code2 = compileBackward typeMap revNames e2
        in "(" ++ code1 ++ " " ++ code2 ++ ")"

    where
        -- Shared logic for forward and unidirectional constructors
        compileConstructor c es =
            let cName = translateConName (prettyShow c)
                args = map (compileBackward typeMap revNames) es
            in if null args
                then cName
                else "(" ++ cName ++ " " ++ unwords args ++ ")"

        -- Shared logic for forward and unidirectional cases
        compileCase e alts =
            let scrutinee = compileBackward typeMap revNames e
                compileAlt (p, body) =
                    "  " ++ compilePattern p ++ " -> " ++ compileBackward typeMap revNames body
                altsCode = map compileAlt alts
            in "(case " ++ scrutinee ++ " of {\n" ++ intercalate ";\n" altsCode ++ "})"

-- Function to compile data declarations
compileDDecl :: Core.DDecl Name.Name -> String
compileDDecl (Core.DDecl dataName tyVars constructors) =
    let
        -- Data type name and type variables
        nameStr = prettyShow dataName
        tyVarStrs = unwords (map prettyShow tyVars)
        -- Left-hand side of the equals sign
        lhs = if null tyVars
            then "data " ++ nameStr
            else "data " ++ nameStr ++ " " ++ tyVarStrs
        -- Helper function to compile a single constructor
        compileCon (conName, _existentials, _constraints, argTypes) =
            let cNameStr = translateConName (prettyShow conName)
                -- wrap types in parentheses
                formatArg ty =
                    let typeStr = prettyShow ty
                    in if ' ' `elem` typeStr && not ("(" `isPrefixOf` typeStr)
                        then "(" ++ typeStr ++ ")"
                        else typeStr

                argsStr = unwords (map formatArg argTypes)
            in if null argTypes
                then cNameStr
                else cNameStr ++ " " ++ argsStr
        -- Right-hand side of the equals sign
        rhs = intercalate " | " (map compileCon constructors)
    in
        lhs ++ " = " ++ rhs ++ " deriving Show"

-- Helper function to capitalize first character of a string
capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs

generateHaskellModule :: String -> [(Name.Name, PolyTy)] -> [Core.DDecl Name.Name] -> [(Name.Name, Ty, Core.Exp Name.Name)] -> (String, String)
generateHaskellModule modName typeMap ddecls bindings =
    let
        revNames = [ prettyShow n | (n, ty, _) <- bindings, isReversible ty ]
        generatedDecls = map (compileBinding typeMap revNames) bindings
        compiledDDecls = map compileDDecl ddecls
        haskellCode = unlines $
              [ "module " ++ capitalize modName ++ " where"
              , ""
              , "import Prelude hiding (fst, snd, (.))"
              ] ++ compiledDDecls ++
              [ ""
              , "main :: IO ()"
              , "main = putStrLn \"This is placeholder code! I will replace this later!\""
              , ""
              ] ++ generatedDecls
    in (haskellCode, ".hs")