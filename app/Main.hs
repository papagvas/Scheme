module Main where

import           Lib
import qualified System.Environment as Env (getArgs)

main :: IO ()
main = Env.getArgs >>= putStrLn . show . eval . readExpr . head
 
