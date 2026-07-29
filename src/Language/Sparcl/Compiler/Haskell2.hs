module Language.Sparcl.Compiler.Haskell2 where

import qualified Language.Sparcl.Core.Syntax as Core
import qualified Language.Sparcl.Name as Name
import qualified Language.Sparcl.Literal as Literal
import           Language.Sparcl.Pretty (prettyShow)
import           Data.List
import           Data.Maybe
import           Data.Char (toUpper)
import           Language.Sparcl.Typing.Type (Ty(..), QualTy(..), pattern (:-@), PolyTy)

-- | Target Haskell Patterns
data HsPat
    = HPVar String
    | HPCon String [HsPat]
    | HPOp String HsPat HsPat
    | HPTuple [HsPat]
    | HPWild
    | HPBang HsPat
    deriving (Eq, Show)

-- | Target Haskell Expressions
data HsExpr
    = HVar String
    | HCon String
    | HLit String
    | HApp HsExpr HsExpr
    | HOp String HsExpr HsExpr
    | HLam [HsPat] HsExpr
    | HLet [(HsPat, HsExpr)] HsExpr
    | HCase HsExpr [(HsPat, HsExpr)]
    | HTuple [HsExpr]
    | HIf HsExpr HsExpr HsExpr
    | HError String
    deriving (Eq, Show)

-- | Top-level Declarations
data HsDecl
    = HBind String HsExpr
    | HData String [String] [(String, [String])]
    deriving (Eq, Show)

-- | Strip the module prefix for checking operator/tuple names
stripBase :: String -> String
stripBase s | "Base." `isPrefixOf` s = drop 5 s
            | otherwise              = s

-- | Check if a string represents an operator
isOperatorName :: String -> Bool
isOperatorName s =
    let s' = stripBase s
    in not (null s') && all (`elem` "!#$%&*+./<=>?@\\^|-~:") s'

-- | Check if a string represents a tuple constructor
isTupleName :: String -> Bool
isTupleName name =
    let name' = stripBase name
    in "<Tup " `isPrefixOf` name' ||
       case stripPrefix "(" name' of
         Just rest ->
           case unsnoc rest of
             Just (middle, ')') -> all (== ',') middle
             _                  -> False
         _ -> False

-- | Helper for precedence-based parenthesization
parensIf :: Bool -> String -> String
parensIf True  s = "(" ++ s ++ ")"
parensIf False s = s

-- | Pretty print target patterns
prettyHsPat :: HsPat -> String
prettyHsPat pat = case pat of
    HPVar v       -> v
    HPCon c []    -> c
    HPCon c ps    -> "(" ++ c ++ " " ++ unwords (map prettyHsPat ps) ++ ")"
    HPOp op p1 p2 -> "(" ++ prettyHsPat p1 ++ " " ++ op ++ " " ++ prettyHsPat p2 ++ ")"
    HPTuple ps    -> "(" ++ intercalate ", " (map prettyHsPat ps) ++ ")"
    HPWild        -> "_"
    HPBang p      -> "!" ++ prettyHsPat p

-- | Pretty print target expressions
prettyHsExpr :: Int -> HsExpr -> String
prettyHsExpr p expr = case expr of
    HVar v        -> v
    HCon c        -> c
    HLit l        -> l
    HApp e1 e2    -> parensIf (p > 10) $ prettyHsExpr 10 e1 ++ " " ++ prettyHsExpr 11 e2
    HOp op e1 e2  -> parensIf (p > 5)  $ prettyHsExpr 5 e1 ++ " " ++ op ++ " " ++ prettyHsExpr 6 e2
    HLam ps e     -> parensIf (p > 0)  $ "\\" ++ unwords (map prettyHsPat ps) ++ " -> " ++ prettyHsExpr 0 e
    HLet binds e  -> parensIf (p > 0)  $ "let { " ++ intercalate "; " [ prettyHsPat pat ++ " = " ++ prettyHsExpr 0 b | (pat, b) <- binds ] ++ " } in " ++ prettyHsExpr 0 e
    HCase e alts  -> parensIf (p > 0)  $ "case " ++ prettyHsExpr 0 e ++ " of {\n" ++ intercalate ";\n" [ "  " ++ prettyHsPat pat ++ " -> " ++ prettyHsExpr 0 body | (pat, body) <- alts ] ++ "\n}"
    HTuple es     -> "(" ++ intercalate ", " (map (prettyHsExpr 0) es) ++ ")"
    HIf c t f     -> parensIf (p > 0)  $ "if " ++ prettyHsExpr 0 c ++ " then " ++ prettyHsExpr 0 t ++ " else " ++ prettyHsExpr 0 f
    HError msg    -> parensIf (p > 10) $ "error " ++ show msg

-- | Pretty print target top-level declarations
prettyHsDecl :: HsDecl -> String
prettyHsDecl decl = case decl of
    HBind name e -> name ++ " = " ++ prettyHsExpr 0 e
    HData dName tyVars cons ->
        let lhs = unwords (dName : tyVars)
            rhs = intercalate " | " [ unwords (c : args) | (c, args) <- cons ]
        in "data " ++ lhs ++ " = " ++ rhs ++ " deriving Show"

data Variable = Variable
    { varName :: Name.Name
    , varKind :: BindingKind
    } deriving (Eq, Show)

type Env = [Variable]

-- | Unified context for the AST-based compiler
data CompileContext = CompileContext
    { ctxTypeMap :: [(Name.Name, PolyTy)]
    , ctxEnv     :: Env
    }

-- | Data type that defines the nature of a bound variable in the current scope
data BindingKind
    = Copy       -- ^ Belongs to \Gamma (can be duplicated/dropped freely)
    | Linear     -- ^ Belongs to \Theta (must be treated as a (fwd, bwd) pair at runtime)
    deriving (Eq, Show)

-- =========================================================================
-- REVERSIBLE ABSTRACTIONS
-- =========================================================================

-- | INVARIANT: A RevExpr is an opaque HsExpr that, at runtime, evaluates to
--   exactly a 2-tuple: `(forward_pass_value, backward_pass_closure)`.
newtype RevExpr = RevExpr { unRevExpr :: HsExpr }
    deriving (Eq, Show)

-- | The Compile Result Representation
data CompileResult
    = ForwardOnly HsExpr
    | Reversible RevExpr
    deriving (Eq, Show)

-- | Construct a RevExpr from a forward and backward expression.
mkRev :: HsExpr -> HsExpr -> RevExpr
mkRev fwd bwd = RevExpr (HTuple [fwd, bwd])

-- | Continuation-passing helper to safely bind the forward and backward
--   components of a RevExpr, ensuring no raw tuple manipulation is exposed.
withRev :: String -> RevExpr -> (HsExpr -> HsExpr -> HsExpr) -> HsExpr
withRev prefix (RevExpr e) mkBody =
    let fName = prefix ++ "_fwd"
        bName = prefix ++ "_bwd"
    in HLet [(HPTuple [HPVar fName, HPVar bName], e)]
            (mkBody (HVar fName) (HVar bName))

-- | Unpack a CompileResult if you explicitly need the raw underlying HsExpr.
revExpr :: CompileResult -> HsExpr
revExpr (Reversible (RevExpr e)) = e
revExpr (ForwardOnly _) = error "Compiler Bug: Expected reversible computation, but got forward-only."

-- | Safely extracts the forward value.
getFwd :: CompileResult -> HsExpr
getFwd (ForwardOnly e) = e
getFwd (Reversible r)  = withRev "_fwd_ext" r const

-- | Safely asserts and extracts the RevExpr.
getReversible :: CompileResult -> RevExpr
getReversible (Reversible r)  = r
getReversible (ForwardOnly _) = error "Compiler Bug: Expected reversible computation, but got forward-only."

-- | Helper to look up a variable's binding kind in the environment
lookupVar :: Name.Name -> Env -> Maybe BindingKind
lookupVar n env = listToMaybe [ kind | Variable vName kind <- env, vName == n ]

-- | Helper function to check if type is reversible structurally
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
    TyCon tc [_, _, ret] | "NArrow" `isInfixOf` show tc -> isReversibleSpine ret
    TyCon c _ | prettyShow c `elem` ["rev", "NRev", "(rev)"] -> True
    TyForAll _ (TyQual _ innerTy) -> isReversibleSpine innerTy
    TySyn _ innerTy               -> isReversibleSpine innerTy
    _ -> False

-- | Helper function to check if expression is reversible
-- NOTE: This implies an assumption that (((f a) b) c) is reversible iff f is.
-- Future improvement: Utilize a type-checker to query `exprType ctx e`.
isReversibleExpr :: CompileContext -> Core.Exp Name.Name -> Bool
isReversibleExpr ctx expr = case expr of
    Core.Var n -> maybe False isReversible (lookup n (ctxTypeMap ctx))
    Core.App e1 _ -> isReversibleExpr ctx e1
    Core.RCon _ _ -> True
    _ -> False

-- | Helper to extract all bound variables from a pattern
patVars :: Core.Pat Name.Name -> [Name.Name]
patVars pat = case pat of
    Core.PVar n      -> [n]
    Core.PCon _ args -> concatMap patVars args

-- | Helper function to format names to valid Haskell
formatName :: String -> String
formatName s
    | "Base." `isPrefixOf` s = formatName (drop 5 s)
    | not (null s) && all (`elem` "!#$%&*+./<=>?@\\^|-~:") s = "(" ++ s ++ ")"
    | otherwise = s

-- | Helper function to translate internal constructor names
translateConName :: String -> String
translateConName name
    | "Base." `isPrefixOf` name = translateConName (drop 5 name)
    | Just rest <- stripPrefix "<Tup " name =
        let n = read (init rest) :: Int
        in "(" ++ replicate (n - 1) ',' ++ ")"
    | otherwise = name

-- | Compile a top-level binding into a list of Haskell AST declarations
compileBinding :: CompileContext -> (Name.Name, Ty, Core.Exp Name.Name) -> [HsDecl]
compileBinding ctx (name, ty, expr) =
    let nameStr = formatName (prettyShow name)
        bKind = if isReversible ty then Linear else Copy
        initBodyCtx = ctx { ctxEnv = Variable name bKind : ctxEnv ctx }
        compiled = compileExpr initBodyCtx expr

    in case (bKind, compiled) of
        (Linear, Reversible r) ->
            [ HBind nameStr (revExpr compiled)
            , HBind (nameStr ++ "_fwd") (withRev "top" r const)
            , HBind (nameStr ++ "_bwd") (withRev "top" r (\_ b -> b))
            ]
        (Linear, ForwardOnly _) ->
            error "Compiler Bug: Expected reversible computation for Linear binding, but got forward-only."
        (Copy, _) ->
            [ HBind nameStr (getFwd compiled) ]

-- | Translate Sparcl Patterns to Haskell Patterns
compilePat :: Core.Pat Name.Name -> HsPat
compilePat pat = case pat of
    Core.PVar n -> HPVar (formatName (prettyShow n))
    Core.PCon c ps ->
        let rawCName = prettyShow c
            psAst    = map compilePat ps
        in if isTupleName rawCName
            then HPTuple psAst
            else HPCon (translateConName rawCName) psAst

-- | Helper for literals
compileLiteral :: Literal.Literal -> HsExpr
compileLiteral (Literal.LitInt i) = HLit (show i)
compileLiteral _ = HError "Unhandled literal type"

-- | The expression compilation pass routing to specialized helpers
compileExpr :: CompileContext -> Core.Exp Name.Name -> CompileResult
compileExpr ctx expr = case expr of
    Core.Lit l          -> ForwardOnly (compileLiteral l)
    Core.Lift e1 e2     -> compileLift ctx e1 e2
    Core.Unlift e       -> ForwardOnly (getFwd (compileExpr ctx e))
    Core.Var n          -> compileVar ctx n
    Core.Abs n body     -> compileAbs ctx n body
    Core.App e1 e2      -> compileApp ctx e1 e2
    Core.Con c es       -> compileCon ctx c es
    Core.RCon c es      -> compileRCon ctx c es
    Core.Case e alts    -> compileCase ctx e alts
    Core.Let binds body -> compileLet ctx binds body
    Core.RPin e1 e2     -> ForwardOnly (HError "Not implemented")
    Core.RCase e rAlts  -> ForwardOnly (HError "Not implemented")

-- -----------------------------------------------------------------------------------------
-- Compiling Specific Expression Node Types
-- -----------------------------------------------------------------------------------------

compileLift :: CompileContext -> Core.Exp Name.Name -> Core.Exp Name.Name -> CompileResult
compileLift ctx e1 e2 =
    let fwdEx = getFwd (compileExpr ctx e1)
        bwdEx = getFwd (compileExpr ctx e2)
    in ForwardOnly (HTuple [fwdEx, bwdEx])

compileVar :: CompileContext -> Name.Name -> CompileResult
compileVar ctx n =
    let v = formatName (prettyShow n)
    in case lookupVar n (ctxEnv ctx) of
        Just Linear -> Reversible (RevExpr (HVar v))
        _           -> ForwardOnly (HVar v)

compileAbs :: CompileContext -> Name.Name -> Core.Exp Name.Name -> CompileResult
compileAbs ctx n body =
    let -- 1. Dynamically look up the variable's type to determine if it is Linear or Copy.
        isLin = maybe False isReversible (lookup n (ctxTypeMap ctx))
        kind  = if isLin then Linear else Copy

        -- 2. Bind it in the local environment and compile the body
        varObj  = Variable n kind
        bodyCtx = ctx { ctxEnv = varObj : ctxEnv ctx }
        compiledBody = compileExpr bodyCtx body
        pat = HPVar (formatName (prettyShow n))
    in
    case kind of
        Copy ->
            -- For static/unrestricted arguments
            -- we keep the forward and backward passes natively coupled in a single lambda.
            case compiledBody of
                Reversible r ->
                    ForwardOnly (HLam [pat] (unRevExpr r))
                ForwardOnly f ->
                    ForwardOnly (HLam [pat] f)

        Linear ->
            -- For first-class reversible functions
            -- we construct a Reversible computation encapsulating the forward and backward closures.
            case compiledBody of
                Reversible r ->
                    let -- fwdFun takes the linear argument and executes the forward pass of the body
                        fwdFun = HLam [pat] (getFwd compiledBody)

                        -- bwdFun extracts the dynamically constructed backward closure from the body.
                        bwdFun = withRev "_bwd_ext" r (\_fwd bwd -> bwd)
                    in
                    -- We must return a Reversible here so that `compileApp` and top-level
                    -- bindings can safely invoke `getReversible` on it!
                    Reversible (mkRev fwdFun bwdFun)

                ForwardOnly _ ->
                    ForwardOnly (HError "Compiler Bug: Linear lambda must wrap a reversible body")

compileApp :: CompileContext -> Core.Exp Name.Name -> Core.Exp Name.Name -> CompileResult
compileApp ctx e1 e2 = case e1 of
    Core.App (Core.Var op) lhs | isOperatorName (stripBase (prettyShow op)) ->
        let l = getFwd (compileExpr ctx lhs)
            r = getFwd (compileExpr ctx e2)
        in ForwardOnly (HOp (stripBase (prettyShow op)) l r)
    _ ->
        let res1 = compileExpr ctx e1
            res2 = compileExpr ctx e2
        in if isReversibleExpr ctx e1
           then
               let wrapArg bodyFn = case res2 of
                       Reversible r     -> withRev "_arg" r bodyFn
                       ForwardOnly expr -> bodyFn expr (HError "Compiler bug: Expected linear argument")
               in Reversible $ RevExpr $
                   wrapArg $ \argFwd argBwd ->
                       withRev "_fn" (getReversible res1) $ \fwdFn bwdFn ->
                           let fwdCall    = HApp fwdFn argFwd
                               callBwdFn  = HApp bwdFn (HVar "_val")
                               bwdClosure = HLam [HPVar "_val"] $
                                                HLet [(HPVar "_dx", callBwdFn)] (HApp argBwd (HVar "_dx"))
                           in unRevExpr (mkRev fwdCall bwdClosure)
           else
               ForwardOnly (HApp (getFwd res1) (getFwd res2))

compileCon :: CompileContext -> Name.Name -> [Core.Exp Name.Name] -> CompileResult
compileCon ctx c es =
    let compiledArgs = map (getFwd . compileExpr ctx) es
        rawCName     = prettyShow c
    in ForwardOnly $
        if isTupleName rawCName
            then HTuple compiledArgs
            else foldl HApp (HCon (translateConName rawCName)) compiledArgs

compileRCon :: CompileContext -> Name.Name -> [Core.Exp Name.Name] -> CompileResult
compileRCon ctx c es =
    let compiledArgs = map (compileExpr ctx) es
        rawCName = prettyShow c
        cName    = translateConName rawCName

        -- Recursively unpacks all RevExpr arguments so we can map them.
        buildArgs :: Int -> [CompileResult] -> ([HsExpr] -> [HsExpr] -> HsExpr) -> HsExpr
        buildArgs _ [] k = k [] []
        buildArgs i (res:rest) k = case res of
            Reversible r ->
                withRev ("_a" ++ show i) r $ \fwd bwd ->
                    buildArgs (i+1) rest $ \fs bs -> k (fwd:fs) (bwd:bs)
            ForwardOnly expr ->
                buildArgs (i+1) rest $ \fs bs -> k (expr:fs) (HError "Compiler Bug":bs)

    in Reversible $ RevExpr $
        buildArgs 1 compiledArgs $ \fwdArgs bwdArgs ->

            -- 1. Forward Constructor Application
            let fwdNode = if isTupleName rawCName
                          then HTuple fwdArgs
                          else foldl HApp (HCon cName) fwdArgs

            -- 2. Backward Reconstructor logic directly using target AST
                argNames = [ "_val" ++ show i | i <- [1..length es] ]
                argPats  = map HPVar argNames
                pat = if isTupleName rawCName then HPTuple argPats else HPCon cName argPats

                reconArgs = zipWith (\bwd argName -> HApp bwd (HVar argName)) bwdArgs argNames
                reconBody = if isTupleName rawCName
                            then HTuple reconArgs
                            else foldl HApp (HCon cName) reconArgs

                bwdClosure = HLam [HPVar "_val"] (HCase (HVar "_val") [(pat, reconBody)])

            in unRevExpr (mkRev fwdNode bwdClosure)

compileCase :: CompileContext -> Core.Exp Name.Name -> [(Core.Pat Name.Name, Core.Exp Name.Name)] -> CompileResult
compileCase ctx e alts =
    let compiledScrut = getFwd (compileExpr ctx e)
        compileAlt (pat, body) =
            let boundVars = map (`Variable` Copy) (patVars pat)
                altCtx    = ctx { ctxEnv = boundVars ++ ctxEnv ctx }
                bodyRes   = compileExpr altCtx body
            in (compilePat pat, getFwd bodyRes)
        compiledAlts = map compileAlt alts
    in ForwardOnly (HCase compiledScrut compiledAlts)

compileLet :: CompileContext -> Core.Bind Name.Name -> Core.Exp Name.Name -> CompileResult
compileLet ctx binds body =
    let -- 1. Determine the BindingKind for each let-bound variable based on its type
        mkVar (n, ty, _) = Variable n (if isReversible ty then Linear else Copy)
        newVars = map mkVar binds

        -- 2. Extend the context with the new bindings.
        bodyCtx = ctx { ctxEnv = newVars ++ ctxEnv ctx }

        -- Helper to safely extract the underlying Haskell AST node
        -- regardless of whether the right-hand side is a forward-only value or a reversible tuple.
        getRawExpr :: CompileResult -> HsExpr
        getRawExpr (ForwardOnly expr) = expr
        getRawExpr (Reversible (RevExpr expr)) = expr

        -- 3. Compile the right-hand side of each binding
        compileBind (n, _ty, e) =
            let pat = HPVar (formatName (prettyShow n))
                compiledE = compileExpr bodyCtx e
            in (pat, getRawExpr compiledE)

        hsBinds = map compileBind binds

        -- 4. Compile the let body
        compiledBody = compileExpr bodyCtx body

    in case compiledBody of
        ForwardOnly expr ->
            ForwardOnly (HLet hsBinds expr)

        Reversible (RevExpr expr) ->
            Reversible (RevExpr (HLet hsBinds expr))

-- | Function to compile data declarations
compileDDecl :: Core.DDecl Name.Name -> String
compileDDecl (Core.DDecl dataName tyVars constructors) =
    let
        nameStr = prettyShow dataName
        tyVarStrs = unwords (map prettyShow tyVars)
        lhs = if null tyVars
            then "data " ++ nameStr
            else "data " ++ nameStr ++ " " ++ tyVarStrs

        compileCons (conName, _existentials, _constraints, argTypes) =
            let cNameStr = translateConName (prettyShow conName)
                formatArg ty =
                    let typeStr = prettyShow ty
                    in if ' ' `elem` typeStr && not ("(" `isPrefixOf` typeStr)
                        then "(" ++ typeStr ++ ")"
                        else typeStr
                argsStr = unwords (map formatArg argTypes)
            in if null argTypes
                then cNameStr
                else cNameStr ++ " " ++ argsStr

        rhs = intercalate " | " (map compileCons constructors)
    in
        lhs ++ " = " ++ rhs ++ " deriving Show"

-- | Helper function to capitalize first character of a string
capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs

isFunctionTy :: Ty -> Bool
isFunctionTy ty = case ty of
    _ :-@ _                                           -> True
    TyCon tc [_, _, _] | "NArrow" `isInfixOf` show tc -> True
    TyForAll _ (TyQual _ innerTy)                     -> isFunctionTy innerTy
    TySyn _ innerTy                                   -> isFunctionTy innerTy
    _                                                 -> False

isShowableTy :: Ty -> Bool
isShowableTy ty = case ty of
    _ | isFunctionTy ty           -> False
    TyVar _                       -> False
    TyForAll _ (TyQual _ innerTy) -> isShowableTy innerTy
    TySyn _ innerTy               -> isShowableTy innerTy
    TyCon _ args                  -> all isShowableTy args
    _                             -> True

-- | Helper function to construct the main IO function that logs all bindings
constructPutStrLn :: (Name.Name, Ty, Core.Exp Name.Name) -> String
constructPutStrLn (name, _, _) = "\"\\n" ++ prettyShow name ++ ": \" ++ show " ++ formatName (prettyShow name)

generateHaskellModule :: String -> [(Name.Name, PolyTy)] -> [Core.DDecl Name.Name] -> [(Name.Name, Ty, Core.Exp Name.Name)] -> (String, String)
generateHaskellModule modName typeMap ddecls bindings =
    let
        initCtx = CompileContext { ctxTypeMap = typeMap, ctxEnv = [] }

        generatedDecls = concatMap (map prettyHsDecl . compileBinding initCtx) bindings
        compiledDDecls = map compileDDecl ddecls
        showableBindings = filter (\(_, ty, _) -> isShowableTy ty) bindings

        haskellCode = unlines $
              ["{-# LANGUAGE BangPatterns #-}"
              ,"module " ++ capitalize modName ++ " where"
              , ""
              , "import Prelude hiding (fst, snd, (.))"
              ] ++ compiledDDecls ++
              [ ""
              , "main :: IO ()"
              , "main = putStrLn (" ++ intercalate " ++ " (map constructPutStrLn showableBindings) ++ ")"
              , ""
              , intercalate "\n\n" generatedDecls]
    in (haskellCode, ".hs")