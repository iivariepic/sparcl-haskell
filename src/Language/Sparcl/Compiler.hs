module Language.Sparcl.Compiler where

import qualified Language.Sparcl.Surface.Parsing as Parser
import qualified Language.Sparcl.CompileHaskell  as Compiler
import qualified Language.Sparcl.Core.Syntax     as Core
import qualified Language.Sparcl.Surface.Syntax  as S
import qualified Control.Monad.Reader            as Rd
import qualified Language.Sparcl.Desugar         as Desugar
import qualified Language.Sparcl.Typing.Typing   as Typing
import qualified Language.Sparcl.Renaming        as Renaming

import System.FilePath                    (takeBaseName)
import Language.Sparcl.Name               (Name)
import Language.Sparcl.Module
import Language.Sparcl.Typing.TCMonad     (runTC, runTCWith, TypingContext, initTypingContext, KeyTC)
import Control.Monad.Catch                (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.IO.Class             (MonadIO)
import Language.Sparcl.DebugPrint         (KeyDebugLevel)
import Language.Sparcl.Class              (Has(..), Local(..))

data CompilerEnv = CompilerEnv
    { envDebugLevel    :: Int
    , envTypingContext :: TypingContext
    }

newtype CompilerM a = CompilerM { runCompilerM :: Rd.ReaderT CompilerEnv IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

instance Has KeyDebugLevel Int CompilerM where
  ask _ = CompilerM $ Rd.asks envDebugLevel

instance Has KeyTC TypingContext CompilerM where
  ask _ = CompilerM $ Rd.asks envTypingContext

instance Local KeyTC TypingContext CompilerM where
  local _ f (CompilerM m) = CompilerM $ Rd.local (\e -> e { envTypingContext = f (envTypingContext e) }) m

desugarModuleToCore :: String -> CompilerM [(Name, Core.Exp Name)]
desugarModuleToCore input = do
    let info = baseModuleInfo

    parsedModule <- case Parser.parseModule "<compiler>" input of
                      Left err -> error $ "Parsing Error:\n" ++ err
                      Right m  -> return m

    let (S.Module modName _ _ topDeclsParsed) = parsedModule

    let (topDeclsRenamed, _dataDecls, _typeDecls, _boundVars, _opTable) =
          case Renaming.runRenaming (miNameTable info) (miOpTable info)
                         (Renaming.renameTopDecls modName topDeclsParsed) of
                     Left err -> error $ "Renaming Error: " ++ show err
                     Right r  -> r

    coreBindingsWithTypes <- runTC $
      runTCWith (miConTable info) (miTypeTable info) (miSynTable info) $ do
        res <- Typing.inferTopDecls topDeclsRenamed [] []
        let (typedDecls, _typeMap, _coreDDecls, _coreTDecls, _cTypeTable, _synTable) = res

        Desugar.runDesugar $ Desugar.desugarTopDecls typedDecls

    return [ (n, e) | (n, _ty, e) <- coreBindingsWithTypes ]

compileFile :: FilePath -> IO ()
compileFile inputFile = do
    putStrLn $ "Compiler: Compiling " ++ inputFile
    fileContent <- readFile inputFile

    tc <- initTypingContext
    let env = CompilerEnv { envDebugLevel = 0, envTypingContext = tc }

    bindings <- Rd.runReaderT (runCompilerM $ desugarModuleToCore fileContent) env

    let generatedDecls = map Compiler.compileBinding bindings

    let moduleName = takeBaseName inputFile
    let haskellCode = unlines $
          [ "module " ++ moduleName ++ " where"
          , ""
          , "main :: IO ()"
          , "main = putStrLn \"This is placeholder code! I will replace later!\""
          , ""
          ] ++ generatedDecls

    let outputFile = moduleName ++ ".hs"
    writeFile outputFile haskellCode

    putStrLn $ "Compiler: Success! Output written to: " ++ outputFile