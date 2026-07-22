module Language.Sparcl.Compiler.Haskell where

import qualified Language.Sparcl.Core.Syntax as Core
import qualified Language.Sparcl.Name as Name
import qualified Language.Sparcl.Literal as Literal
import           Language.Sparcl.Pretty (prettyShow)
import           Data.List
import           Data.Char (toUpper)
import           Language.Sparcl.Typing.Type (Ty(..), QualTy(..), pattern (:-@), PolyTy)

-- Data type that defines the nature of a bound variable in the current scope
data BindingKind
    = PureCopyable       -- ^ Belongs to \Gamma (can be duplicated/dropped freely)
    | LinearReversible   -- ^ Belongs to \Theta (must be treated as a (fwd, bwd) pair at runtime)
    deriving (Eq, Show)

-- Data type that tells us if we are inside a reversible binding
data ContextMode = Inside | Outside deriving (Eq, Show)

-- Data type for the context of the compiler
data CompilerContext =  CompilerContext
    { ctxTypeMap  :: [(Name.Name, PolyTy)]
    , ctxEnv :: [(Name.Name, BindingKind)]
    , ctxMode :: ContextMode
    }

-- Helper to extract all bound variables from a pattern
patVars :: Core.Pat Name.Name -> [Name.Name]
patVars pat = case pat of
    Core.PVar n      -> [n]
    Core.PCon _ args -> concatMap patVars args

-- Helper function to wrap symboling operators in parentheses
formatName :: String -> String
formatName s
    | not (null s) && all (`elem` "!#$%&*+./<=>?@\\^|-~:") s = "(" ++ s ++ ")"
    | otherwise = s

-- Helper function to translate internal constructor names
translateConName :: String -> String
translateConName name
    | Just rest <- stripPrefix "<Tup " name =
        let n = read (init rest) :: Int
        in "(" ++ replicate (n - 1) ',' ++ ")"
    | otherwise = name

-- Helper to determine if an expression is a reversible pair that needs projection
needsProjection :: CompilerContext -> Core.Exp Name.Name -> Bool
needsProjection ctx (Core.Var n) =
    (ctxMode ctx /= Outside) && (case lookup n (ctxEnv ctx) of
        Just LinearReversible -> True
        Just PureCopyable -> False
        Nothing -> maybe False isReversible (lookup n (ctxTypeMap ctx)))
needsProjection _ (Core.App _ _) = False
needsProjection _ _ = False

-- Literals
compileLiteral :: Literal.Literal -> String
compileLiteral l = case l of
    Literal.LitInt i    -> show i
    _                   -> error "compileLiteral: Unhandled literal type"

-- Helper function to check if binding is reversible structurally
isReversible :: Ty -> Bool
isReversible ty = case ty of
    (_ :-@ _) -> True
    TyCon c _ | prettyShow c `elem` ["rev", "NRev", "(rev)"] -> True
    TyForAll _ (TyQual _ innerTy) -> isReversible innerTy
    TySyn _ innerTy -> isReversible innerTy
    _ -> isReversibleSpine ty

isReversibleSpine :: Ty -> Bool
isReversibleSpine ty = case ty of
    _ :-@ _ -> True
    TyCon _ [TyMult _, _, ret] -> isReversibleSpine ret
    TyCon c _ | prettyShow c `elem` ["rev", "NRev", "(rev)"] -> True
    TyForAll _ (TyQual _ innerTy) -> isReversibleSpine innerTy
    TySyn _ innerTy               -> isReversibleSpine innerTy
    _ -> False

-- Helper to check if an application chain belongs to a reversible function
isReversibleAppChain :: CompilerContext -> Core.Exp Name.Name -> Bool
isReversibleAppChain ctx (Core.Var n) =
    (ctxMode ctx /= Outside) && (case lookup n (ctxEnv ctx) of
        Just LinearReversible -> True
        Just PureCopyable -> False
        Nothing -> maybe False isReversible (lookup n (ctxTypeMap ctx)))
isReversibleAppChain ctx (Core.App e1 _) = isReversibleAppChain ctx e1
isReversibleAppChain ctx (Core.RPin e1 _) = isReversibleAppChain ctx e1
isReversibleAppChain _ _ = False
-- Top-level Bindings
compileBinding :: CompilerContext -> (Name.Name, Ty, Core.Exp Name.Name) -> String
compileBinding ctx (name, ty, expr) =
  let rawName = prettyShow name
      nameStr = formatName rawName

      bKind = if isReversible ty then LinearReversible else PureCopyable
      bMode = if isReversible ty then Inside else Outside
      initBodyCtx = ctx { ctxEnv = (name, bKind) : ctxEnv ctx, ctxMode = bMode }

  in if bKind == LinearReversible
    then
        let fwdCode = compileForward initBodyCtx expr
            bwdCode = compileBackward initBodyCtx expr
        in nameStr ++ " = (" ++ nameStr ++ "_fwd , " ++ nameStr ++ "_bwd)\n\n"
        ++ nameStr ++ "_fwd = " ++ fwdCode ++ "\n\n" ++ nameStr ++ "_bwd = " ++ bwdCode
    else
        let code = compileForward initBodyCtx expr
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

-- Compiling unidirectional or forward functions
compileForward :: CompilerContext -> Core.Exp Name.Name -> String
compileForward ctx expr = case expr of
    -- Literal values
    Core.Lit l -> compileLiteral l

    -- Variables
    Core.Var n -> formatName (prettyShow n)

    -- Lambda abstractions
    Core.Abs n e ->
        let varName  = prettyShow n
            bodyCtx  = ctx { ctxEnv = (n, PureCopyable) : ctxEnv ctx }
            bodyCode = compileForward bodyCtx e
        in "(\\" ++ varName ++ " -> " ++ bodyCode ++ ")"

    -- Data constructors
    Core.Con c es ->
        let cName = translateConName (prettyShow c)
            args = map (compileForward ctx) es
        in if null args
            then cName
            else "(" ++ cName ++ " " ++ unwords args ++ ")"

    Core.RCon c es ->
        let cName = translateConName (prettyShow c)
            compileArg e =
                let code = compileForward ctx e
                in if needsProjection ctx e
                   then code ++ "_fwd"
                   else code
            args = map compileArg es
        in if null args then cName else "(" ++ cName ++ " " ++ unwords args ++ ")"

    -- Let bindings
    Core.Let binds body ->
        let compileBind (n, _ty, e) = prettyShow n ++ " = " ++ compileForward ctx e
            bindsCode = map compileBind binds
            bindsString = intercalate "; " bindsCode

            -- Add local bindings to reversible names
            newEnvBindings = [ (n, if isReversible ty then LinearReversible else PureCopyable)
                             | (n, ty, _) <- binds ]
            bodyCtx = ctx { ctxEnv = newEnvBindings ++ ctxEnv ctx }

        in "(let { " ++ bindsString ++ " } in " ++ compileForward bodyCtx body ++ ")"

    -- Case expressions
    Core.Case e alts ->
        let scrutinee = compileForward ctx e
            compileAlt (p, body) =
                let pVars   = patVars p
                    bodyCtx = ctx { ctxEnv = map (\v -> (v, PureCopyable)) pVars ++ ctxEnv ctx }
                in "  " ++ compilePattern p ++ " -> " ++ compileForward bodyCtx body
            altsCode = map compileAlt alts
        in "(case " ++ scrutinee ++ " of {\n" ++ intercalate ";\n" altsCode ++ "})"

    -- Reversible Case expressions
    Core.RCase e rAlts ->
        let scrutCode = compileForward ctx e
            scrutVal  = if needsProjection ctx e
                        then scrutCode ++ "_fwd"
                        else scrutCode

            compileAlt (p, body, _pin) =
                let pVars   = patVars p
                    bodyCtx = ctx { ctxEnv = map (\v -> (v, PureCopyable)) pVars ++ ctxEnv ctx }
                -- Evaluate the branch body forward under the current environment bindings
                in "  " ++ compilePattern p ++ " -> " ++ compileForward bodyCtx body

            altsCode = map compileAlt rAlts
        in "(case " ++ scrutVal ++ " of {\n" ++ intercalate ";\n" altsCode ++ "})"

    -- RPin
    Core.RPin e1 e2 ->
        let f1Code = compileForward ctx e1
            hCode  = compileForward ctx e2
        in "(let _a = " ++ f1Code ++ " in let _f2 = fst (" ++ hCode ++ " _a) in (_a, _f2))"

    -- Lift
    Core.Lift e1 e2 ->
        let fwdCode = compileForward ctx e1
            bwdCode = compileBackward ctx e2
        in "(" ++ fwdCode ++ ", " ++ bwdCode ++ ")"

    -- Unlift
    Core.Unlift e -> compileForward ctx e

    -- Recursive compilation of the entire function App
    Core.App e1 e2 ->
        let code1 = compileForward ctx e1
            code2 = compileForward ctx e2
        in if needsProjection ctx e1
            then "(" ++ code1 ++ "_fwd " ++ code2 ++ ")"
            else "(" ++ code1 ++ " " ++ code2 ++ ")"

-- Compiling backward functions
compileBackward :: CompilerContext -> Core.Exp Name.Name -> String
compileBackward ctx expr = case expr of

    -- Variables in backward evaluation
    Core.Var n ->
        case lookup n (ctxEnv ctx) of
            Just PureCopyable -> "(\\_v -> _v)"
            Just LinearReversible -> formatName (prettyShow n)
            Nothing ->
                if needsProjection ctx expr then
                    formatName (prettyShow n)
                else
                    "(\\_v -> _v)"

    -- Lambda abstractions
    Core.Abs n e ->
        let varName  = prettyShow n
            bodyCode = compileBackward ctx e
        in "(\\" ++ varName ++ " -> " ++ bodyCode ++ ")"

    -- Data constructors
    Core.RCon c es ->
            let cName = translateConName (prettyShow c)
                vars  = [ "_v" ++ prettyShow i | i <- [1..length es] ]
                pat   = if null vars then cName else "(" ++ cName ++ " " ++ unwords vars ++ ")"

                compileArg e v =
                    let code = compileBackward ctx e
                    in if needsProjection ctx e
                       then "(" ++ code ++ "_bwd " ++ v ++ ")"
                       else "(" ++ code ++ " " ++ v ++ ")"

                results = zipWith compileArg es vars
                body = case results of
                    []  -> "()"
                    [r] -> r
                    _   -> "(" ++ intercalate ", " results ++ ")"
            in "(\\" ++ pat ++ " -> " ++ body ++ ")"

    -- Let bindings
    Core.Let binds body ->
        let compileBind (n, _ty, e) = prettyShow n ++ " = " ++ compileForward ctx e
            bindsCode = map compileBind binds
            bindsString = intercalate "; " bindsCode

            newEnvBindings = [ (n, LinearReversible)
                             | (n, ty, _) <- binds, isReversible ty ]
            bodyCtx = ctx { ctxEnv = newEnvBindings ++ ctxEnv ctx }

        in "(let { " ++ bindsString ++ " } in " ++ compileBackward bodyCtx body ++ ")"

    -- RPin
    Core.RPin e1 e2 ->
        let b1Code = compileBackward ctx e1
            hCode  = compileBackward ctx e2
        in "(\\_tup -> case _tup of (_a, _b) -> let _b2 = (" ++ hCode ++ ") _a in (((" ++ b1Code ++ ") _a), (_b2 _b)))"

    -- Lift
    Core.Lift e1 e2 ->
        let fwdCode = compileForward ctx e1
            bwdCode = compileBackward ctx e2
        in "(" ++ fwdCode ++ ", " ++ bwdCode ++ ")"

    -- Unlift
    Core.Unlift e -> compileForward ctx e

    -- Recursive compilation of the entire function App
    Core.App e1 e2 ->
        case e1 of
            Core.Abs _ _ ->
                let code1 = compileBackward ctx e1
                    code2 = compileForward ctx e2
                in "(" ++ code1 ++ " " ++ code2 ++ ")"
            _ ->
                if isReversibleAppChain ctx e1 then
                    let code1 = compileBackward ctx e1
                        code2 = compileForward ctx e2
                    in if needsProjection ctx e1 then
                        "(" ++ code1 ++ "_bwd " ++ code2 ++ ")"
                    else
                        "(" ++ code1 ++ " " ++ code2 ++ ")"
                else
                    compileForward ctx expr

    -- Case expressions
    Core.Case e alts ->
        let scrutinee = compileBackward ctx e
            compileAlt (p, body) =
                let pVars   = patVars p
                    bodyCtx = ctx { ctxEnv = map (\v -> (v, PureCopyable)) pVars ++ ctxEnv ctx }
                in "  " ++ compilePattern p ++ " -> " ++ compileBackward bodyCtx body
            altsCode = map compileAlt alts
        in "(case " ++ scrutinee ++ " of {\n" ++ intercalate ";\n" altsCode ++ "})"

    -- Reversible Case expressions (Backwards)
    Core.RCase e rAlts ->
        let scrutBwd = compileBackward ctx e
            scrutFwd = let code = compileForward ctx e
                       in if needsProjection ctx e then code ++ "_fwd" else code

            scrutCall bwdVal = if needsProjection ctx e
                               then "(" ++ scrutBwd ++ "_bwd " ++ bwdVal ++ ")"
                               else "(" ++ scrutBwd ++ " " ++ bwdVal ++ ")"

            buildIfChain [] = "error \"No matching pin found during inversion in rcase\""
            buildIfChain ((p, body, pin):rest) =
                let pinCode   = compileForward ctx pin
                    pVars     = patVars p
                    bodyCtx   = ctx { ctxEnv = map (\v -> (v, PureCopyable)) pVars ++ ctxEnv ctx }
                    bodyBwd   = compileBackward bodyCtx body

                    upPat     = buildTuplePat body

                    branchVal = "(case " ++ scrutFwd ++ " of { " ++ compilePattern p ++ " -> " ++
                                "let " ++ upPat ++ " = (" ++ bodyBwd ++ ") _v in " ++
                                compilePattern p ++ "; _ -> error \"unreachable rcase pattern\" })"

                in "if (" ++ pinCode ++ ") _v then " ++ scrutCall branchVal ++ " else " ++ buildIfChain rest
        in "(\\_v -> " ++ buildIfChain rAlts ++ ")"

    _ -> compileForward ctx expr

-- Helper to build a tuple pattern mapping the backward output of an expression
buildTuplePat :: Core.Exp Name.Name -> String
buildTuplePat expr = case expr of
    Core.Var n -> formatName (prettyShow n)
    Core.RCon _ es ->
        let pats = map buildTuplePat es
        in case pats of
            []  -> "()"
            [p] -> p
            _   -> "(" ++ intercalate ", " pats ++ ")"
    Core.App _ e2 -> buildTuplePat e2     -- App backward returns the updated argument (e2)
    Core.RPin e1 _ -> buildTuplePat e1
    Core.Lift _ e2 -> buildTuplePat e2
    Core.Let _ body -> buildTuplePat body
    _ -> "_"

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

-- Helper function to construct the main IO function that logs all bindings
constructPutStrLn :: (Name.Name, Ty, Core.Exp Name.Name) -> String
constructPutStrLn (name, _, _) = "\"\\n" ++ prettyShow name ++ ": \" ++ show " ++ formatName (prettyShow name)

generateHaskellModule :: String -> [(Name.Name, PolyTy)] -> [Core.DDecl Name.Name] -> [(Name.Name, Ty, Core.Exp Name.Name)] -> (String, String)
generateHaskellModule modName typeMap ddecls bindings =
    let
        initCtx = CompilerContext
                    { ctxTypeMap  = typeMap
                    , ctxEnv = []
                    , ctxMode = Outside
                    }
        generatedDecls = map (compileBinding initCtx) bindings
        compiledDDecls = map compileDDecl ddecls
        haskellCode = unlines $
              [ "module " ++ capitalize modName ++ " where"
              , ""
              , "import Prelude hiding (fst, snd, (.))"
              ] ++ compiledDDecls ++
              [ ""
              , "main :: IO ()"
              , "main = putStrLn (" ++ intercalate " ++ " (map constructPutStrLn bindings) ++ ")"
              , ""
              , intercalate "\n\n" generatedDecls]
    in (haskellCode, ".hs")