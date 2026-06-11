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

-- Data type for the context of the compiler
data CompilerContext =  CompilerContext
    { ctxTypeMap  :: [(Name.Name, PolyTy)]
    , ctxEnv :: [(Name.Name, BindingKind)]
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

-- Helper to determine if an expression is a reversible pair that needs projection
needsProjection :: CompilerContext -> Core.Exp Name.Name -> Bool
needsProjection ctx (Core.Var n) = case lookup n (ctxEnv ctx) of
    Just LinearReversible -> True
    _                     -> False
-- We can expand this later if more complex expressions return reversible pairs
needsProjection _ _ = False

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
compileBinding :: CompilerContext -> (Name.Name, Ty, Core.Exp Name.Name) -> String
compileBinding ctx (name, ty, expr) =
  let rawName = prettyShow name
      nameStr = formatName rawName

      bKind = if isReversible ty then LinearReversible else PureCopyable
      initBodyCtx = ctx { ctxEnv = (name, bKind) : ctxEnv ctx }

  in if bKind == LinearReversible
    then
        let fwdCode = compileForward initBodyCtx expr
            bwdCode = compileBackward initBodyCtx expr
        in nameStr ++ " = (" ++ fwdCode ++ ", " ++ bwdCode ++ ")"
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

-- Helper function to translate internal constructor names
translateConName :: String -> String
translateConName name

    | Just rest <- stripPrefix "<Tup " name =
        let n = read (init rest) :: Int
        in "(" ++ replicate (n - 1) ',' ++ ")"

    | otherwise = name

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
    Core.Con c es -> compileConstructor c es
    Core.RCon c es ->
            let cName = translateConName (prettyShow c)
                -- For each subexpression, check if it requires a tuple projection
                compileArg e =
                    let code = compileForward ctx e
                    in if needsProjection ctx e
                       then "((fst " ++ code ++ ") _s)"
                       else "(" ++ code ++ " _s)"
                args = map compileArg es
                body = if null args then cName else "(" ++ cName ++ " " ++ unwords args ++ ")"
            in "(\\_s -> " ++ body ++ ")"

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
    Core.Case e alts -> compileCase e alts

    -- Reversible Case expressions
    Core.RCase e rAlts ->
        let scrutCode = compileForward ctx e
            -- Thread the forward state into the scrutinee
            scrutVal  = if needsProjection ctx e
                        then "((fst " ++ scrutCode ++ ") _s)"
                        else "(" ++ scrutCode ++ " _s)"

            compileAlt (p, body, _pin) =
                let pVars   = patVars p
                    bodyCtx = ctx { ctxEnv = map (\v -> (v, PureCopyable)) pVars ++ ctxEnv ctx }
                -- Evaluate the branch body forward with the same state state
                in "  " ++ compilePattern p ++ " -> (" ++ compileForward bodyCtx body ++ ") _s"

            altsCode = map compileAlt rAlts
        in "(\\_s -> case " ++ scrutVal ++ " of {\n" ++ intercalate ";\n" altsCode ++ "})"

    -- RPin
    Core.RPin e1 e2 ->
        let f1Code = compileForward ctx e1
            hCode  = compileForward ctx e2
        in "(\\_s -> let _a = (" ++ f1Code ++ ") _s; _f2 = (" ++ hCode ++ ") _a in (_a, _f2 _s))"

    -- Lift
    Core.Lift e1 e2 ->
        let fwdCode = compileForward ctx e1
            bwdCode = compileForward ctx e2
        in "(" ++ fwdCode ++ ", " ++ bwdCode ++ ")"

    -- Unlift
    Core.Unlift e -> compileForward ctx e

    -- Recursive compilation of the entire function App
    Core.App e1 e2 ->
        let
            code1 = compileForward ctx e1
            code2 = compileForward ctx e2
        in if needsProjection ctx e1
            then "((fst " ++ code1 ++ ") " ++ code2 ++ ")"
            else "(" ++ code1 ++ " " ++ code2 ++ ")"

    where
        -- Shared logic for forward and unidirectional constructors
        compileConstructor c es =
            let cName = translateConName (prettyShow c)
                args = map (compileForward ctx) es
            in if null args
                then cName
                else "(" ++ cName ++ " " ++ unwords args ++ ")"

        -- Helper function to compile a case
        compileCase e alts =
            let scrutinee = compileForward ctx e
                compileAlt (p, body) =
                    let pVars   = patVars p
                        bodyCtx = ctx { ctxEnv = map (\v -> (v, PureCopyable)) pVars ++ ctxEnv ctx }
                    in "  " ++ compilePattern p ++ " -> " ++ compileForward bodyCtx body
                altsCode = map compileAlt alts
            in "(case " ++ scrutinee ++ " of {\n" ++ intercalate ";\n" altsCode ++ "})"

-- Compiling backward functions
compileBackward :: CompilerContext -> Core.Exp Name.Name -> String
compileBackward ctx expr = case expr of
    -- Lambda abstractions
    Core.Abs n e ->
        let varName  = prettyShow n
            bodyCtx  = ctx { ctxEnv = (n, PureCopyable) : ctxEnv ctx }
            bodyCode = compileBackward bodyCtx e
        in "(\\" ++ varName ++ " -> " ++ bodyCode ++ ")"

    -- Data constructors
    Core.Con c es -> compileConstructor c es
    Core.RCon c es ->
            let cName = translateConName (prettyShow c)
                -- Generate names for the unpacked constructor variables (_v1, _v2, ...)
                vars  = [ "_v" ++ show i | i <- [1..length es] ]
                pat   = if null vars then cName else "(" ++ cName ++ " " ++ unwords vars ++ ")"

                -- Project 'snd' if the argument expression is a linear variable pair
                compileArg e v =
                    let code = compileBackward ctx e
                    in if needsProjection ctx e
                       then "((snd " ++ code ++ ") " ++ v ++ ")"
                       else "(" ++ code ++ " " ++ v ++ ")"

                results = zipWith compileArg es vars
                body = case results of
                    []  -> "()"
                    [r] -> r
                    _   -> "(" ++ intercalate ", " results ++ ")"
            in "(\\" ++ pat ++ " -> " ++ body ++ ")"

    -- Let bindings
    Core.Let binds body ->
        let compileBind (n, _ty, e) = prettyShow n ++ " = " ++ compileBackward ctx e
            bindsCode = map compileBind binds
            bindsString = intercalate "; " bindsCode

            -- Add local bindings to the environment \Gamma and \Theta
            newEnvBindings = [ (n, if isReversible ty then LinearReversible else PureCopyable)
                             | (n, ty, _) <- binds ]
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
        let
            code1 = compileBackward ctx e1
            code2 = compileBackward ctx e2
        in if needsProjection ctx e1
            then "((snd " ++ code1 ++ ") " ++ code2 ++ ")"
            else "(" ++ code1 ++ " " ++ code2 ++ ")"

    -- Case expressions
    Core.Case e alts -> compileCase e alts

    -- Reversible Case expressions (Backwards)
    Core.RCase e rAlts ->
        let scrutBwd = compileBackward ctx e
            -- Feeds the unwound branch result back into the scrutinee's backward pass
            scrutCall bwdVal = if needsProjection ctx e
                               then "((snd " ++ scrutBwd ++ ") " ++ bwdVal ++ ")"
                               else "(" ++ scrutBwd ++ " " ++ bwdVal ++ ")"

            -- Recursively construct the conditional if-then-else chain using the branch pins
            buildIfChain [] = "error \"No matching pin found during inversion in rcase\""
            buildIfChain ((p, body, pin):rest) =
                let pinCode   = compileForward ctx pin -- Pins are pure, forward predicates
                    pVars     = patVars p
                    bodyCtx   = ctx { ctxEnv = map (\v -> (v, PureCopyable)) pVars ++ ctxEnv ctx }
                    bodyBwd   = compileBackward bodyCtx body
                    branchVal = "(" ++ bodyBwd ++ ") _v"
                in "if (" ++ pinCode ++ ") _v then " ++ scrutCall branchVal ++ " else " ++ buildIfChain rest
        in "(\\_v -> " ++ buildIfChain rAlts ++ ")"

    -- If use functionality of compileForward if there is no separate need
    _ -> compileForward ctx expr


    where
        -- Shared logic for forward and unidirectional constructors
        compileConstructor c es =
            let cName = translateConName (prettyShow c)
                args = map (compileBackward ctx) es
            in if null args
                then cName
                else "(" ++ cName ++ " " ++ unwords args ++ ")"

        -- Helper function to compile backward case
        compileCase e alts =
            let scrutinee = compileBackward ctx e
                compileAlt (p, body) =
                    let pVars   = patVars p
                        bodyCtx = ctx { ctxEnv = map (\v -> (v, PureCopyable)) pVars ++ ctxEnv ctx }
                    in "  " ++ compilePattern p ++ " -> " ++ compileBackward bodyCtx body
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
        initCtx = CompilerContext
                    { ctxTypeMap  = typeMap
                    , ctxEnv = []
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
              , "main = putStrLn \"This is placeholder code! I will replace this later!\""
              , ""
              ] ++ generatedDecls
    in (haskellCode, ".hs")