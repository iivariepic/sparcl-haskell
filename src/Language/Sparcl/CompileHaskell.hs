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
            
    _ -> error "compileExpression: Unimplemented constructor"
