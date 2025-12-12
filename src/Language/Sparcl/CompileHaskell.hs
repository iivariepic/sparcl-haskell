module Language.Sparcl.CompileHaskell where

import qualified Language.Sparcl.Core.Syntax as Core
import qualified Language.Sparcl.Name as Name
import qualified Language.Sparcl.Literal as Literal

--| The type for the code output
data HaskellCode = HaskellCode
    { hsForward :: String
    , hsBackward :: String
    }
    deriving (Show, Eq)