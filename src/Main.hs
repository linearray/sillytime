module Main where

import Sillytime.Parsers

import Control.Category
import Control.Monad (when)
import System.Environment
import Text.Megaparsec

main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) (error "wrong number of arguments")
    c <- readFile $ head args

    case runParser parser (head args) c of
        Left s -> putStrLn $ errorBundlePretty s
        Right (SillyTime fee acts) -> do
            maybe (print "No fee") (print <<< show) fee
            mapM_ print acts
