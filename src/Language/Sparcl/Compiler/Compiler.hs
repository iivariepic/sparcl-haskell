module Language.Sparcl.Compiler.Compiler where

import qualified Language.Sparcl.Surface.Parsing   as Parser
import qualified Language.Sparcl.Compiler.Haskell  as HsCompiler
import qualified Language.Sparcl.Core.Syntax       as Core
import qualified Language.Sparcl.Surface.Syntax    as S
import qualified Control.Monad.Reader              as Rd
import qualified Language.Sparcl.Desugar           as Desugar
import qualified Language.Sparcl.Typing.Typing     as Typing
import qualified Language.Sparcl.Renaming          as Renaming

import System.FilePath                    (takeBaseName)
import Language.Sparcl.Name               (Name)
import Language.Sparcl.Module
import Language.Sparcl.Typing.TCMonad     (runTC, runTCWith, TypingContext, initTypingContext, KeyTC)
import Control.Monad.Catch                (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.IO.Class             (MonadIO)
import Language.Sparcl.DebugPrint         (KeyDebugLevel)
import Language.Sparcl.Class              (Has(..), Local(..))
import Language.Sparcl.Typing.Type        (Ty)

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

desugarModuleToCore :: String -> CompilerM ([Core.DDecl Name], [(Name, Ty, Core.Exp Name)])
desugarModuleToCore input = do
    let info = baseModuleInfo

    let parsedModule = case Parser.parseModule "<compiler>" input of
                        Left err -> error $ "Parsing Error:\n" ++ err
                        Right m  -> m

    let (S.Module modName _ _ topDeclsParsed) = parsedModule

    let (topDeclsRenamed, dataDecls, typeDecls, _boundVars, _opTable) =
          case Renaming.runRenaming (miNameTable info) (miOpTable info)
                         (Renaming.renameTopDecls modName topDeclsParsed) of
                     Left err -> error $ "Renaming Error: " ++ show err
                     Right r  -> r

    runTC $
      runTCWith (miConTable info) (miTypeTable info) (miSynTable info) $ do
        res <- Typing.inferTopDecls topDeclsRenamed dataDecls typeDecls
        let (typedDecls, _typeMap, coreDDecls, _coreTDecls, _cTypeTable, _synTable) = res

        coreBindings <- Desugar.runDesugar $ Desugar.desugarTopDecls typedDecls
        return (coreDDecls, coreBindings)

compileFile :: FilePath -> IO ()
compileFile inputFile = do
    putStrLn $ "Compiler: Compiling " ++ inputFile
    fileContent <- readFile inputFile

    tc <- initTypingContext
    let env = CompilerEnv { envDebugLevel = 0, envTypingContext = tc }

    (ddecls, bindings) <- Rd.runReaderT (runCompilerM $ desugarModuleToCore fileContent) env

    let moduleName = takeBaseName inputFile
    let (code, fileExtension) = HsCompiler.generateHaskellModule moduleName ddecls bindings

    let outputFile = moduleName ++ fileExtension
    writeFile outputFile code

    putStrLn $ "Compiler: Success! Output written to: " ++ outputFile