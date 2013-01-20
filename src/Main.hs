
module Main where

import System.Console.Haskeline

import Language.Lambda
import Language.Lambda.Parser

main = do
  putStrLn "Please enter a lambda expression."
  runInputT defaultSettings repl

repl :: InputT IO ()
repl = do
  input <- getInputLine "> "
  case input of
    Nothing -> return ()
    Just "quit" -> return ()
    Just input -> do displayReduction input
                     repl

displayReduction s = 
  case parseLambda s of
    Left err  -> outputStrLn $ show err
    Right lam -> mapM_ (outputStrLn . (" -> "++) . show) (reduce lam)

reduce l = case betaReduce l of
  Nothing  -> l:[]
  Just red -> l:reduce red
