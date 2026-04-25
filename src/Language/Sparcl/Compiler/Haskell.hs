module Language.Sparcl.Compiler.Haskell where

import qualified Language.Sparcl.Core.Syntax as Core
import qualified Language.Sparcl.Name as Name
import qualified Language.Sparcl.Literal as Literal
import           Language.Sparcl.Pretty (prettyShow)
import           Data.List
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
compileBinding :: (Name.Name, Ty, Core.Exp Name.Name) -> String
compileBinding (name, ty, expr) =
  let nameStr = prettyShow name
  in if isReversible ty
    then
        let fwdCode = compileForward expr
            bwdCode = "error \"bwd pass not yet implemented for: " ++ nameStr ++ "\""
        in nameStr ++ " = (" ++ fwdCode ++ ", " ++ bwdCode ++ ")"
    else
        let code = compileForward expr
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
compileForward :: Core.Exp Name.Name -> String
compileForward expr = case expr of
    -- Literal values
    Core.Lit l -> compileLiteral l

    -- Variables
    Core.Var n -> prettyShow n

    -- Lambda abstractions
    Core.Abs n e ->
        let varName  = prettyShow n
            bodyCode = compileForward e
        in "(\\" ++ varName ++ " -> " ++ bodyCode ++ ")"

    -- Data constructors
    Core.Con c es -> compileConstructor c es
    Core.RCon c es -> compileConstructor c es

    -- Let bindings
    Core.Let binds body ->
        let compileBind (n, _ty, e) = prettyShow n ++ " = " ++ compileForward e
            bindsCode = map compileBind binds
            bindsString = intercalate "; " bindsCode
        in "(let { " ++ bindsString ++ " } in " ++ compileForward body ++ ")"

    -- Case expressions
    Core.Case e alts   -> compileCase e alts
    Core.RCase e rAlts ->
        let alts = map (\(p, body, _pin) -> (p, body)) rAlts
        in compileCase e alts

    -- RPin
    Core.RPin e1 _e2 ->
        compileForward e1

    -- Lift
    Core.Lift e1 e2 ->
        let fwdCode = compileForward e1
            bwdCode = compileForward e2
        in "(" ++ fwdCode ++ ", " ++ bwdCode ++ ")"

    -- Unlift
    Core.Unlift e -> compileForward e

    -- Recursive compilation of the entire App
    Core.App e1 e2 ->
        let
            code1 = compileForward e1
            code2 = compileForward e2
        in "(" ++ code1 ++ " " ++ code2 ++ ")"

    _ -> error "compileForward: Unimplemented constructor"

    where
        -- Shared logic for forward and unidirectional constructors
        compileConstructor c es =
            let cName = translateConName (prettyShow c)
                args = map compileForward es
            in if null args
                then cName
                else "(" ++ cName ++ " " ++ unwords args ++ ")"

        -- Shared logic for forward and unidirectional cases
        compileCase e alts =
            let scrutinee = compileForward e
                compileAlt (p, body) =
                    "  " ++ compilePattern p ++ " -> " ++ compileForward body
                altsCode = map compileAlt alts
            in "(case " ++ scrutinee ++ " of {\n" ++ unlines altsCode ++ "})"


generateHaskellModule :: String -> [(Name.Name, Ty, Core.Exp Name.Name)] -> (String, String)
generateHaskellModule modName bindings =
    let generatedDecls = map compileBinding bindings
        haskellCode = unlines $
              [ "module " ++ modName ++ " where"
              , ""
              , "main :: IO ()"
              , "main = putStrLn \"This is placeholder code! I will replace this later!\""
              , ""
              ] ++ generatedDecls
    in (haskellCode, ".hs")