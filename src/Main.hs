
module Main where

import Language.Lambda.Parser

main = do
  putStrLn $ either (show) (show) (parseLambda "(λx. x)")

