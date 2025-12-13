module Language.Sparcl.CompileHaskell where

import qualified Language.Sparcl.Core.Syntax as Core
import qualified Language.Sparcl.Name as Name
import qualified Language.Sparcl.Literal as Literal

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
        let nameString = show n
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

-- Test function for compiling the code with the unimplemented constructors hard coded
compileModule :: HaskellCode -> String
compileModule (HaskellCode fwdCode bwdCode) = unlines
    [     "module Test where"
        , ""
        , "fwd_result :: IO ()"
        , "fwd_result = putStrLn $ show (" ++ fwdCode ++ ")"
        , ""
        , "bwd_result :: IO ()"
        , "bwd_result = putStrLn $ show (" ++ bwdCode ++ ")"
        , ""
        , "main :: IO ()"
        , "main = do"
        , "  putStrLn \"--- Forward Result ---\""
        , "  fwd_result"
        , "  putStrLn \"--- Backward Result ---\""
        , "  bwd_result"
        ]