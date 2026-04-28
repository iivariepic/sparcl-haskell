module Language.Sparcl.Compiler.Haskell where

import qualified Language.Sparcl.Core.Syntax as Core
import qualified Language.Sparcl.Name as Name
import qualified Language.Sparcl.Literal as Literal
import           Language.Sparcl.Pretty (prettyShow)
import           Data.List
import           Data.Char (toUpper)
import           Language.Sparcl.Typing.Type (Ty(..), QualTy(..), pattern (:-@))

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
compileBinding :: [String] -> (Name.Name, Ty, Core.Exp Name.Name) -> String
compileBinding revNames (name, ty, expr) =
  let nameStr = prettyShow name
  in if isReversible ty
    then
        let fwdCode = compileForward revNames expr
            bwdCode = "error \"bwd pass not yet implemented for: " ++ nameStr ++ "\""
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
        let nameStr = prettyShow n
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

    -- Recursive compilation of the entire App
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

generateHaskellModule :: String -> [Core.DDecl Name.Name] -> [(Name.Name, Ty, Core.Exp Name.Name)] -> (String, String)
generateHaskellModule modName ddecls bindings =
    let
        revNames = [ prettyShow n | (n, ty, _) <- bindings, isReversible ty ]
        generatedDecls = map (compileBinding revNames) bindings
        compiledDDecls = map compileDDecl ddecls
        haskellCode = unlines $
              [ "module " ++ capitalize modName ++ " where"
              , ""
              ] ++ compiledDDecls ++
              [ ""
              , "main :: IO ()"
              , "main = putStrLn \"This is placeholder code! I will replace this later!\""
              , ""
              ] ++ generatedDecls
    in (haskellCode, ".hs")