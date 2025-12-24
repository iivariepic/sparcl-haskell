module Language.Sparcl.Compiler.Haskell where

import qualified Language.Sparcl.Core.Syntax as Core
import qualified Language.Sparcl.Name as Name
import qualified Language.Sparcl.Literal as Literal
import           Language.Sparcl.Pretty (prettyShow)

-- The type for the code output
data HaskellCode = HaskellCode
    { hsForward :: String
    , hsBackward :: String
    }
    deriving (Show, Eq)

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
  in nameStr ++ " = " ++ hsForward code

-- The Main Compiler Function
compileExpression :: Core.Exp Name.Name -> HaskellCode
compileExpression expr = case expr of
    -- Literal values
    Core.Lit l ->
        let code = compileLiteral l
        in HaskellCode
            { hsForward = code
            , hsBackward = code
            }

    -- Variables
    Core.Var n ->
        let nameString = prettyShow n
        in HaskellCode
            { hsForward = nameString
            , hsBackward = nameString
            }

    -- Recursive compilation of the entire App
    Core.App e1 e2 ->
        let
            code1 = compileExpression e1
            code2 = compileExpression e2

            fwdApp = "(" ++ hsForward code1 ++ " " ++ hsForward code2 ++ ")"
            bwdApp = "(" ++ hsBackward code1 ++ " " ++ hsBackward code2 ++ ")"
        in HaskellCode
            { hsForward = fwdApp
            , hsBackward = bwdApp
            }

    _ -> error "compileExpression: Unimplemented constructor"


generateHaskellModule :: String -> [(Name.Name, Core.Exp Name.Name)] -> (String, String)
generateHaskellModule modName bindings =
    let generatedDecls = map compileBinding bindings
        haskellCode = unlines $
              [ "module " ++ modName ++ " where"
              , ""
              , "main :: IO ()"
              , "main = putStrLn \"This is placeholder code! I will replace later!\""
              , ""
              ] ++ generatedDecls
    in (haskellCode, ".hs")