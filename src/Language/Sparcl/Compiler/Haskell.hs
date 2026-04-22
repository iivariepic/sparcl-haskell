module Language.Sparcl.Compiler.Haskell where

import qualified Language.Sparcl.Core.Syntax as Core
import qualified Language.Sparcl.Name as Name
import qualified Language.Sparcl.Literal as Literal
import           Language.Sparcl.Pretty (prettyShow)

-- Literals
compileLiteral :: Literal.Literal -> String
compileLiteral l = case l of
    Literal.LitInt i    -> show i
    _                   -> error "compileLiteral: Unhandled literal type"

-- Top-level Bindings
compileBinding :: (Name.Name, Core.Exp Name.Name) -> String
compileBinding (name, expr) =
  let code = compileExpression expr
      nameStr = prettyShow name
  in nameStr ++ " = " ++ code

-- The Main Compiler Function
compileExpression :: Core.Exp Name.Name -> String
compileExpression expr = case expr of
    -- Literal values
    Core.Lit l -> compileLiteral l

    -- Variables
    Core.Var n -> prettyShow n

    -- Recursive compilation of the entire App
    Core.App e1 e2 ->
        let
            code1 = compileExpression e1
            code2 = compileExpression e2
        in "(" ++ code1 ++ " " ++ code2 ++ ")"

    _ -> error "compileExpression: Unimplemented constructor"


generateHaskellModule :: String -> [(Name.Name, Core.Exp Name.Name)] -> (String, String)
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