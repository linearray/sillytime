module Main where

import Sillytime.Parsers

import Control.Monad (when)
import System.Environment
import Text.Megaparsec

main :: IO ()
main = do
    args <- getArgs                  -- IO [String]
    when (length args /= 1) (error "wrong number of arguments")
    c <- readFile $ head args
    case runParser activities (head args) c of
        Left s -> putStrLn $ errorBundlePretty s
        Right s -> print s
