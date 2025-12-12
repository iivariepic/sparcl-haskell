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

-- The Main Compiler Function
compileExpression :: Core.Exp Name.Name -> HaskellCode
compileExpression expr = case expr of
    _ -> error "compileExpression: Unimplemented constructor"
