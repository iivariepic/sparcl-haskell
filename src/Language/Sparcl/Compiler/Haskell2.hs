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
    | HOp String HsExpr HsExpr -- ^ Added for infix operators
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
prettyHsExpr :: HsExpr -> String
prettyHsExpr expr = case expr of
    HVar v        -> v
    HCon c        -> c
    HLit l        -> l
    HApp e1 e2    -> "(" ++ prettyHsExpr e1 ++ " " ++ prettyHsExpr e2 ++ ")"
    HOp op e1 e2  -> "(" ++ prettyHsExpr e1 ++ " " ++ op ++ " " ++ prettyHsExpr e2 ++ ")"
    HLam ps e     -> "(\\" ++ unwords (map prettyHsPat ps) ++ " -> " ++ prettyHsExpr e ++ ")"
    HLet binds e  -> "(let { " ++ intercalate "; " [ prettyHsPat p ++ " = " ++ prettyHsExpr b | (p, b) <- binds ] ++ " } in " ++ prettyHsExpr e ++ ")"
    HCase e alts  -> "(case " ++ prettyHsExpr e ++ " of {\n" ++ intercalate ";\n" [ "  " ++ prettyHsPat p ++ " -> " ++ prettyHsExpr body | (p, body) <- alts ] ++ "\n})"
    HTuple es     -> "(" ++ intercalate ", " (map prettyHsExpr es) ++ ")"
    HIf c t f     -> "if " ++ prettyHsExpr c ++ " then " ++ prettyHsExpr t ++ " else " ++ prettyHsExpr f
    HError msg    -> "error " ++ show msg

-- | Pretty print target top-level declarations
prettyHsDecl :: HsDecl -> String
prettyHsDecl decl = case decl of
    HBind name e -> name ++ " = " ++ prettyHsExpr e
    HData dName tyVars cons ->
        let lhs = unwords (dName : tyVars)
            rhs = intercalate " | " [ unwords (c : args) | (c, args) <- cons ]
        in "data " ++ lhs ++ " = " ++ rhs ++ " deriving Show"

-- | Data type that tells us if we are inside a reversible binding
data ContextMode = Outside | Inside deriving (Eq, Show)

data Variable = Variable
    { varName :: Name.Name
    , varKind :: BindingKind
    } deriving (Eq, Show)

type Env = [Variable]

-- | Unified context for the AST-based compiler
data CompileContext = CompileContext
    { ctxTypeMap :: [(Name.Name, PolyTy)]
    , ctxEnv     :: Env
    , ctxMode    :: ContextMode
    }

-- | Data type that defines the nature of a bound variable in the current scope
data BindingKind
    = Copy       -- ^ Belongs to \Gamma (can be duplicated/dropped freely)
    | Linear     -- ^ Belongs to \Theta (must be treated as a (fwd, bwd) pair at runtime)
    deriving (Eq, Show)

-- | The Backward Pass structure
data BwdResult =
    BwdResult
        { bwdExpr :: HsExpr
        , bwdEnv  :: [Variable]
        }

-- | The Compile Result Representation
data CompileResult =
    CompileResult
        { forward  :: Maybe HsExpr
        , backward :: Maybe BwdResult
        }

-- | Helper to grab just the forward expression, no matter the compilation type
getFwd :: CompileResult -> HsExpr
getFwd (ForwardOnly e)  = e
getFwd (Reversible f _) = f

-- | Helper to look up a variable's binding kind in the environment
lookupVar :: Name.Name -> Env -> Maybe BindingKind
lookupVar n env = listToMaybe [ kind | Variable vName kind <- env, vName == n ]

-- | Helper function to check if binding is reversible structurally
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
        bMode = if isReversible ty then Inside else Outside

        initBodyCtx = ctx { ctxEnv = Variable name bKind : ctxEnv ctx, ctxMode = bMode }
        compiled = compileExpr initBodyCtx expr

    in case bKind of
        Linear -> case compiled of
            Reversible fwdEx (Bwd bwdEx _) ->
                [ HBind nameStr (HTuple [HVar (nameStr ++ "_fwd"), HVar (nameStr ++ "_bwd")])
                , HBind (nameStr ++ "_fwd") fwdEx
                , HBind (nameStr ++ "_bwd") bwdEx
                ]
            ForwardOnly _ ->
                [ HBind nameStr (HError "Compiler bug: Expected reversible computation, got ForwardOnly") ]

        Copy ->
            [ HBind nameStr (getFwd compiled) ]

-- | Translate Sparcl Patterns to Haskell Patterns
compilePat :: Core.Pat Name.Name -> HsPat
compilePat pat = case pat of
    Core.PVar n -> HPVar (formatName (prettyShow n))
    Core.PCon c ps ->
        let cName = translateConName (prettyShow c)
            psAst = map compilePat ps
        in HPCon cName psAst

-- | Helper for literals
compileLiteral :: Literal.Literal -> HsExpr
compileLiteral (Literal.LitInt i) = HLit (show i)
compileLiteral _ = HError "Unhandled literal type"

-- | The expression compilation pass
compileExpr :: CompileContext -> Core.Exp Name.Name -> Compiled
compileExpr ctx expr = case expr of

    Core.Lit l ->
        ForwardOnly (compileLiteral l)

    Core.Lift e1 e2 ->
        -- Lift explicitly pairs a forward expression and a backward expression.
        -- By definition, both sides are evaluated for their forward pass representations.
        let fwdEx = getFwd (compileExpr ctx e1)
            bwdEx = getFwd (compileExpr ctx e2)
        in Reversible
            { cFwd = fwdEx
            , cBwd = Bwd { bCode = bwdEx, bReconstructs = [] }
            }

    Core.Unlift e ->
        -- Consumes a reversible value, extracting its forward computation
        ForwardOnly (getFwd (compileExpr ctx e))

    -- TODO: Add Var, App, Abs, Con, RCon, Let, Case, RCase, RPin
    _ -> ForwardOnly (HError "Not implemented")


-- | Function to compile data declarations
compileDDecl :: Core.DDecl Name.Name -> String
compileDDecl (Core.DDecl dataName tyVars constructors) =
    let
        nameStr = prettyShow dataName
        tyVarStrs = unwords (map prettyShow tyVars)
        lhs = if null tyVars
            then "data " ++ nameStr
            else "data " ++ nameStr ++ " " ++ tyVarStrs

        compileCon (conName, _existentials, _constraints, argTypes) =
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

        rhs = intercalate " | " (map compileCon constructors)
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
        initCtx = CompileContext { ctxTypeMap = typeMap, ctxEnv = [], ctxMode = Outside }

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