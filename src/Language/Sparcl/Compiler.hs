module Language.Sparcl.Compiler where

import qualified Language.Sparcl.Surface.Parsing as Parser
import qualified Language.Sparcl.CompileHaskell as Compiler
import qualified Language.Sparcl.Core.Syntax as Core
import qualified Language.Sparcl.Surface.Syntax as S
import           System.FilePath        (takeBaseName)
import           Language.Sparcl.Name   (Name)
import           Language.Sparcl.SrcLoc (Loc(..))
import           Language.Sparcl.Pass   (Pass(Parsing))

unsafeDesugarExp :: S.LExp 'Parsing -> Core.Exp Name
unsafeDesugarExp (Loc _ (S.Lit l)) = Core.Lit l
unsafeDesugarExp _ = error "Cannot safely convert complex expressions without Renaming/Type Checking."

compileFile :: FilePath -> IO ()
compileFile inputFile = do
    putStrLn $ "Compiling " ++ inputFile
    fileContent <- readFile inputFile

    case Parser.parseExp fileContent of
        Left err ->
            putStrLn $ "Parsing Error:\n" ++ err

        Right surfaceExp -> do
            let coreAST = unsafeDesugarExp surfaceExp
            let compiledCode = Compiler.compileModule (Compiler.compileExpression coreAST)
            let outputFile = takeBaseName inputFile ++ ".hs"
            writeFile outputFile compiledCode

            putStrLn $ "Success! Output written to: " ++ outputFile