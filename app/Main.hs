module Main where

import           Lib
import qualified System.Environment as Env (getArgs)

main :: IO ()
main = do
  args <- Env.getArgs
  putStrLn $ readExpr $ head args
 
