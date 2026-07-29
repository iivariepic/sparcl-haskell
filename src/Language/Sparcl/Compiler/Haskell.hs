module Language.Sparcl.Compiler.Haskell where

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
    | HPTuple [HsPat]
    | HPWild
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
    HPTuple ps    -> "(" ++ intercalate ", " (map prettyHsPat ps) ++ ")"
    HPWild        -> "_"

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
    = Copy        -- ^ Belongs to \Gamma (can be duplicated/dropped freely)
    | LinearArg   -- ^ A full reversible tuple passed as an argument
    | LinearPat   -- ^ A reversible variable bound inside a forward pattern match
    deriving (Eq, Show)

-- | Helper to safely extract the underlying Haskell AST node
getRawExpr :: CompileResult -> HsExpr
getRawExpr (ForwardOnly expr) = expr
getRawExpr (Reversible (RevExpr expr)) = expr

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

-- | Helper to get a reversible expression from both CompileResult types
toRev :: CompileResult -> RevExpr
toRev (Reversible r) = r
toRev (ForwardOnly e) = RevExpr e

-- | Safely extracts the forward value.
getFwd :: CompileResult -> HsExpr
getFwd (ForwardOnly e) = e
getFwd (Reversible r)  = withRev "_fwd_ext" r const

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

-- | Helper to extract all bound variables from a pattern
patVars :: Core.Pat Name.Name -> [Name.Name]
patVars pat = case pat of
    Core.PVar n      -> [n]
    Core.PCon _ args -> concatMap patVars args

-- | A safe pattern reconstructor that uses "_res" variables
patToExpRes :: Core.Pat Name.Name -> HsExpr
patToExpRes (Core.PVar n) = HVar (formatName (prettyShow n) ++ "_res")
patToExpRes (Core.PCon c ps) =
    let rawCName = prettyShow c
    in if isTupleName rawCName
       then HTuple (map patToExpRes ps)
       else foldl HApp (HCon (translateConName rawCName)) (map patToExpRes ps)

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
compileBinding ctx (name, _, expr) =
    let nameStr = formatName (prettyShow name)
        initBodyCtx = ctx { ctxEnv = Variable name Copy : ctxEnv ctx }
        compiled = compileExpr initBodyCtx expr
    in [ HBind nameStr (getRawExpr compiled) ]

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
    Core.Unlift e       -> compileUnlift ctx e
    Core.Var n          -> compileVar ctx n
    Core.Abs n body     -> compileAbs ctx n body
    Core.App e1 e2      -> compileApp ctx e1 e2
    Core.Con c es       -> compileCon ctx c es
    Core.RCon c es      -> compileRCon ctx c es
    Core.Case e alts    -> compileCase ctx e alts
    Core.Let binds body -> compileLet ctx binds body
    Core.RPin e1 e2     -> compileRPin ctx e1 e2
    Core.RCase e rAlts  -> compileRCase ctx e rAlts

-- -----------------------------------------------------------------------------------------
-- Compiling Specific Expression Node Types
-- -----------------------------------------------------------------------------------------

-- | Compiling Lift
compileLift :: CompileContext -> Core.Exp Name.Name -> Core.Exp Name.Name -> CompileResult
compileLift ctx e1 e2 =
    let fwdEx = getFwd (compileExpr ctx e1)
        bwdEx = getFwd (compileExpr ctx e2)
    in ForwardOnly (HTuple [fwdEx, bwdEx])

-- | Compiling Unlift
compileUnlift :: CompileContext -> Core.Exp Name.Name -> CompileResult
compileUnlift ctx e = case compileExpr ctx e of
    Reversible (RevExpr r) -> ForwardOnly r
    ForwardOnly h ->
        let wrappedAs = HTuple [HVar "_as", HLam [HPVar "_v"] (HVar "_v")]

            fwdFun = HLam [HPVar "_as"] $
                HLet [(HPTuple [HPVar "_fwd_val", HPWild], HApp h wrappedAs)]
                     (HVar "_fwd_val")

            bwdFun = HLam [HPVar "_out"] $
                HLet [ (HPVar "_as", HApp (HVar "_bwd_rec") (HVar "_out"))
                     , (HPTuple [HPWild, HPVar "_bwd_closure"], HApp h wrappedAs)
                     ]
                     (HApp (HVar "_bwd_closure") (HVar "_out"))

        in ForwardOnly $ HLet [(HPVar "_bwd_rec", bwdFun)] (HTuple [fwdFun, HVar "_bwd_rec"])

-- | Compiling Variables
compileVar :: CompileContext -> Name.Name -> CompileResult
compileVar ctx n =
    let v = formatName (prettyShow n)
    in case lookupVar n (ctxEnv ctx) of
        Just LinearArg -> Reversible (RevExpr (HVar v))
        Just LinearPat -> Reversible (mkRev (HVar v) (HLam [HPVar "_v"] (HVar "_v")))
        _              -> ForwardOnly (HVar v)

-- | Compiling Lambda Abstractions
compileAbs :: CompileContext -> Name.Name -> Core.Exp Name.Name -> CompileResult
compileAbs ctx n body =
    let isLin = maybe False isReversible (lookup n (ctxTypeMap ctx))
        kind  = if isLin then LinearArg else Copy
        varObj  = Variable n kind
        bodyCtx = ctx { ctxEnv = varObj : ctxEnv ctx }
        compiledBody = compileExpr bodyCtx body
        pat = HPVar (formatName (prettyShow n))
    in case compiledBody of
        Reversible r  -> ForwardOnly (HLam [pat] (unRevExpr r))
        ForwardOnly f -> ForwardOnly (HLam [pat] f)

-- | Compiling the function app
compileApp :: CompileContext -> Core.Exp Name.Name -> Core.Exp Name.Name -> CompileResult
compileApp ctx e1 e2 = case e1 of
    Core.App (Core.Var op) lhs | isOperatorName (stripBase (prettyShow op)) ->
        let l = getRawExpr (compileExpr ctx lhs)
            r = getRawExpr (compileExpr ctx e2)
        in ForwardOnly (HOp (stripBase (prettyShow op)) l r)
    _ ->
        let f1 = getRawExpr (compileExpr ctx e1)
            f2 = getRawExpr (compileExpr ctx e2)
        in ForwardOnly (HApp f1 f2)

-- | Compiling non-reversible constructors
compileCon :: CompileContext -> Name.Name -> [Core.Exp Name.Name] -> CompileResult
compileCon ctx c es =
    let compiledArgs = map (getFwd . compileExpr ctx) es
        rawCName     = prettyShow c
    in ForwardOnly $
        if isTupleName rawCName
            then HTuple compiledArgs
            else foldl HApp (HCon (translateConName rawCName)) compiledArgs

-- | Compiling reversible constructors
compileRCon :: CompileContext -> Name.Name -> [Core.Exp Name.Name] -> CompileResult
compileRCon ctx c es =
    let compiledArgs = map (compileExpr ctx) es
        rawCName = prettyShow c
        cName    = translateConName rawCName

        buildArgs :: Int -> [CompileResult] -> ([HsExpr] -> [HsExpr] -> HsExpr) -> HsExpr
        buildArgs _ [] k = k [] []
        buildArgs i (res:rest) k =
            let r = toRev res
            in withRev ("_a" ++ show i) r $ \fwd bwd ->
                buildArgs (i+1) rest $ \fs bs -> k (fwd:fs) (bwd:bs)

    in Reversible $ RevExpr $
        buildArgs 1 compiledArgs $ \fwdArgs bwdArgs ->
            let fwdNode = if isTupleName rawCName
                          then HTuple fwdArgs
                          else foldl HApp (HCon cName) fwdArgs

                argNames = [ "_val" ++ show i | i <- [1..length es] ]
                argPats  = map HPVar argNames
                pat = if isTupleName rawCName then HPTuple argPats else HPCon cName argPats

                reconArgs = zipWith (\bwd argName -> HApp bwd (HVar argName)) bwdArgs argNames
                reconBody = if isTupleName rawCName
                            then HTuple reconArgs
                            else foldl HApp (HCon cName) reconArgs

                bwdClosure = HLam [HPVar "_val"] (HCase (HVar "_val") [(pat, reconBody)])

            in unRevExpr (mkRev fwdNode bwdClosure)

-- | Compiling non-reversible cases
compileCase :: CompileContext -> Core.Exp Name.Name -> [(Core.Pat Name.Name, Core.Exp Name.Name)] -> CompileResult
compileCase ctx e alts =
    let compiledScrut = getRawExpr (compileExpr ctx e)
        compileAlt (pat, body) =
            let boundVars = map (`Variable` Copy) (patVars pat)
                altCtx    = ctx { ctxEnv = boundVars ++ ctxEnv ctx }
                bodyRes   = compileExpr altCtx body
            in ((compilePat pat, getRawExpr bodyRes), bodyRes)

        compiledAlts = map compileAlt alts
        hsAlts = map fst compiledAlts

    in case compiledAlts of
         [] -> ForwardOnly $ HCase compiledScrut hsAlts -- Safe fallback for empty case
         ((_, firstRes) : _) -> case firstRes of
             Reversible _  -> Reversible $ RevExpr $ HCase compiledScrut hsAlts
             ForwardOnly _ -> ForwardOnly $ HCase compiledScrut hsAlts

-- | Compiling Let Bindings
compileLet :: CompileContext -> Core.Bind Name.Name -> Core.Exp Name.Name -> CompileResult
compileLet ctx binds body =
    let mkVar (n, ty, _) = Variable n (if isReversible ty then LinearArg else Copy)
        newVars = map mkVar binds
        bodyCtx = ctx { ctxEnv = newVars ++ ctxEnv ctx }
        compileBind (n, _ty, e) = (HPVar (formatName (prettyShow n)), getRawExpr (compileExpr bodyCtx e))
        hsBinds = map compileBind binds
        compiledBody = compileExpr bodyCtx body
    in case compiledBody of
        ForwardOnly expr -> ForwardOnly (HLet hsBinds expr)
        Reversible (RevExpr expr) -> Reversible (RevExpr (HLet hsBinds expr))

-- | Compiling Reversible Let Bindings
compileRPin :: CompileContext -> Core.Exp Name.Name -> Core.Exp Name.Name -> CompileResult
compileRPin ctx e1 e2 =
    let res1 = compileExpr ctx e1
        fwd_e2 = getFwd (compileExpr ctx e2)
        r1 = toRev res1
    in Reversible $ RevExpr $
        withRev "_pin" r1 $ \fwd_e1 bwd_e1 ->
            let fwdNode = fwd_e1
                bwdNode = HLam [HPVar "_out"] $
                    HIf (HApp fwd_e2 (HVar "_out"))
                        (HApp bwd_e1 (HVar "_out"))
                        (HError "Pin predicate failed: Branch mismatch in backward pass")
            in unRevExpr (mkRev fwdNode bwdNode)

-- | Helper function to locate ALL specific linear variables within the AST
findVars :: Core.Exp Name.Name -> [Name.Name] -> [Name.Name]
findVars (Core.Var v) vs | v `elem` vs = [v]
findVars (Core.App e1 e2) vs = findVars e1 vs `union` findVars e2 vs
findVars (Core.Con _ args) vs = nub (concatMap (`findVars` vs) args)
findVars (Core.RCon _ args) vs = nub (concatMap (`findVars` vs) args)
findVars (Core.Lift e1 e2) vs = findVars e1 vs `union` findVars e2 vs
findVars (Core.Let binds body) vs =
    let bindVars = concatMap (\(_, _, e) -> findVars e vs) binds
    in nub (bindVars ++ findVars body vs)
findVars (Core.Case e alts) vs =
    let altVars = concatMap (\(_, body) -> findVars body vs) alts
    in nub (findVars e vs ++ altVars)
findVars (Core.RCase e alts) vs =
    let altVars = concatMap (\(_, body, _) -> findVars body vs) alts
    in nub (findVars e vs ++ altVars)
findVars (Core.RPin e1 e2) vs = findVars e1 vs `union` findVars e2 vs
findVars (Core.Abs _ body) vs = findVars body vs
findVars (Core.Unlift e) vs = findVars e vs
findVars _ _ = []

-- | Dynamically derive the pattern, appending "_res" to avoid shadowing original functions!
deriveBwdPat :: Core.Exp Name.Name -> [Name.Name] -> HsPat
deriveBwdPat (Core.RCase _ _) []  = HPTuple []
deriveBwdPat (Core.RCase _ _) [v] = HPVar (formatName (prettyShow v) ++ "_res")
deriveBwdPat (Core.RCase _ _) vs  = HPTuple (map (\v -> HPVar (formatName (prettyShow v) ++ "_res")) vs)

deriveBwdPat (Core.RPin _ _) []  = HPTuple []
deriveBwdPat (Core.RPin _ _) [v] = HPVar (formatName (prettyShow v) ++ "_res")
deriveBwdPat (Core.RPin _ _) vs  = HPTuple (map (\v -> HPVar (formatName (prettyShow v) ++ "_res")) vs)

deriveBwdPat (Core.RCon c args) vs =
    let cName = prettyShow c
    in if isTupleName cName
       then HPTuple (map (`deriveBwdPat` vs) args)
       else HPCon (translateConName cName) (map (`deriveBwdPat` vs) args)

deriveBwdPat (Core.Con c args) vs =
    let cName = prettyShow c
    in if isTupleName cName
       then HPTuple (map (`deriveBwdPat` vs) args)
       else HPCon (translateConName cName) (map (`deriveBwdPat` vs) args)

deriveBwdPat e vs =
    let found = intersect (findVars e vs) vs
    in case found of
        []  -> HPWild
        [v] -> HPVar (formatName (prettyShow v) ++ "_res")
        _   -> HPTuple (map (\v -> HPVar (formatName (prettyShow v) ++ "_res")) found)

-- | Compiling Reversible Cases
compileRCase :: CompileContext -> Core.Exp Name.Name -> [(Core.Pat Name.Name, Core.Exp Name.Name, Core.Exp Name.Name)] -> CompileResult
compileRCase ctx e alts =
    let resE = compileExpr ctx e
        rE = toRev resE
    in Reversible $ RevExpr $
        withRev "_rcase" rE $ \fwdE bwdE ->

            let compileFwdAlt (pat, body, _cond) =
                    let boundVars = map (`Variable` LinearPat) (patVars pat)
                        altCtx = ctx { ctxEnv = boundVars ++ ctxEnv ctx }
                        resBody = compileExpr altCtx body
                        rBody = toRev resBody
                    -- The forward case only returns the forward value.
                    in (compilePat pat, withRev "_alt" rBody const)

                fwdNode = HCase fwdE (map compileFwdAlt alts)

                buildBwd [] = HError "RCase backward match failed"
                buildBwd ((pat, body, cond):rest) =
                    let boundVars = map (`Variable` LinearPat) (patVars pat)
                        altCtx = ctx { ctxEnv = boundVars ++ ctxEnv ctx }
                        rBody = toRev (compileExpr altCtx body)

                        allLinVars = nub [ v | Variable v kind <- ctxEnv altCtx
                                             , kind `elem` [LinearArg, LinearPat] || prettyShow v == "k" ]
                        bVars = patVars pat
                        fVars = intersect (findVars body allLinVars) allLinVars \\ bVars
                        vars = bVars ++ fVars

                        rhsPat = deriveBwdPat body vars
                        rBodyExpr = unRevExpr rBody

                        bwdScrutinee = HApp bwdE (patToExpRes pat)

                        retExpr = if null fVars
                                  then bwdScrutinee
                                  else HTuple (bwdScrutinee : map (\v -> HVar (formatName (prettyShow v) ++ "_res")) fVars)

                        -- This prevents Type Clashes across branches and solves variable scoping.
                        branchExec =
                            HLet [ (compilePat pat, fwdE) ] $
                                HLet [ (HPTuple [HPVar "_body_fwd", HPVar "_body_bwd"], rBodyExpr) ] $
                                    HLet [ (rhsPat, HApp (HVar "_body_bwd") (HVar "_out")) ]
                                        retExpr

                        condCheck = HApp (getRawExpr (compileExpr ctx cond)) (HVar "_out")

                    in HIf condCheck branchExec (buildBwd rest)

                bwdNode = HLam [HPVar "_out"] (buildBwd alts)

            in unRevExpr (mkRev fwdNode bwdNode)

-- | Function to compile data declarations into the target AST
compileDDecl :: Core.DDecl Name.Name -> HsDecl
compileDDecl (Core.DDecl dataName tyVars constructors) =
    let
        dNameStr = prettyShow dataName
        tyVarStrs = map prettyShow tyVars

        compileCons (conName, _existentials, _constraints, argTypes) =
            let cNameStr = translateConName (prettyShow conName)
                formatArg ty =
                    let typeStr = prettyShow ty
                    in if ' ' `elem` typeStr && not ("(" `isPrefixOf` typeStr)
                        then "(" ++ typeStr ++ ")"
                        else typeStr
            in (cNameStr, map formatArg argTypes)

        consDecls = map compileCons constructors
    in
        HData dNameStr tyVarStrs consDecls

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
        compiledDDecls = map (prettyHsDecl . compileDDecl) ddecls

        showableBindings = filter (\(_, ty, _) -> isShowableTy ty) bindings

        haskellCode = unlines $
              ["module " ++ capitalize modName ++ " where"
              , ""
              , "import Prelude hiding (fst, snd, (.))"
              ] ++ compiledDDecls ++
              [ ""
              , "main :: IO ()"
              , "main = putStrLn (" ++ intercalate " ++ " (map constructPutStrLn showableBindings) ++ ")"
              , ""
              , intercalate "\n\n" generatedDecls]
    in (haskellCode, ".hs")