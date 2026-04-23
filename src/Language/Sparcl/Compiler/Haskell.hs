module Language.Sparcl.Compiler.Haskell where

import qualified Language.Sparcl.Core.Syntax as Core
import qualified Language.Sparcl.Name as Name
import qualified Language.Sparcl.Literal as Literal
import           Language.Sparcl.Pretty (prettyShow)
import           Data.List
import           Language.Sparcl.Typing.Type (Ty)

-- Literals
compileLiteral :: Literal.Literal -> String
compileLiteral l = case l of
    Literal.LitInt i    -> show i
    _                   -> error "compileLiteral: Unhandled literal type"

-- Top-level Bindings
compileBinding :: (Name.Name, Ty, Core.Exp Name.Name) -> String
compileBinding (name, _ty, expr) =
  let code = compileExpression expr
      nameStr = prettyShow name
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

-- The Main Compiler Function
compileExpression :: Core.Exp Name.Name -> String
compileExpression expr = case expr of
    -- Literal values
    Core.Lit l -> compileLiteral l

    -- Variables
    Core.Var n -> prettyShow n

    -- Lambda abstractions
    Core.Abs n e ->
        let varName  = prettyShow n
            bodyCode = compileExpression e
        in "(\\" ++ varName ++ " -> " ++ bodyCode ++ ")"

    -- Data constructors
    Core.Con c es ->
        let cName = translateConName (prettyShow c)
            args = map compileExpression es
        in if null args
            then cName
            else "(" ++ cName ++ " " ++ unwords args ++ ")"

    -- Let bindings
    Core.Let binds body ->
        let compileBind (n, _ty, e) = prettyShow n ++ " = " ++ compileExpression e
            bindsCode = map compileBind binds
            bindsString = intercalate "; " bindsCode
        in "(let { " ++ bindsString ++ " } in " ++ compileExpression body ++ ")"

    -- Case expressions
    Core.Case e alts ->
        let scrutinee = compileExpression e
            compileAlt (p, body) =
                "  " ++ compilePattern p ++ " -> " ++ compileExpression body
            altsCode = map compileAlt alts
        in "(case " ++ scrutinee ++ " of {\n" ++ unlines altsCode ++ "})"

    -- Recursive compilation of the entire App
    Core.App e1 e2 ->
        let
            code1 = compileExpression e1
            code2 = compileExpression e2
        in "(" ++ code1 ++ " " ++ code2 ++ ")"

    _ -> error "compileExpression: Unimplemented constructor"


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