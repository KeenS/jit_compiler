module Main where

import Parser
import Codegen
import Emit
import JIT

import Control.Monad.Trans

import System.IO
import System.Environment
import System.Console.Haskeline

import qualified LLVM.General.AST as AST

initModule :: AST.Module
initModule = emptyModule "my cool jit"

process :: AST.Module -> String -> IO (Either String AST.Module)
process modo source = do
  let res = parseToplevel source
  case res of
    Left err -> do
      print err
      return $ Left "Error"
    Right ex -> do
      ast <- codegen modo ex
      runJIT ast

processFile :: String -> IO (Either String AST.Module)
processFile fname = readFile fname >>= process initModule

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
    loop mod = do
      minput <- getInputLine "ready> "
      case minput of
        Nothing -> outputStrLn "Goobye."
        Just input -> do
          modn <- liftIO $ process mod input
          case modn of
            Right modn -> loop modn
            Left _ -> loop mod


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    [fname] -> processFile fname >> return ()
