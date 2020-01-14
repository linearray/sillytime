module Main where

import Sillytime.Parsers

import Control.Category
import Control.Monad (when)
import Data.Fixed (Fixed(..), E2)
import Data.Foldable
import Data.Ratio
import Data.Time.Clock
import System.Environment
import qualified Data.Text as Text
import Data.Time.Format as F
import Text.Megaparsec
import Text.Printf


main :: IO ()
main = do
    args <- getArgs
    when (length args /= 1) (error "wrong number of arguments")
    c <- readFile $ head args

    case runParser parser (head args) c of
        Left s -> putStrLn $ errorBundlePretty s
        Right (SillyTime fee acts) -> do
            maybe (putStrLn "No fee\n") (\f -> print f *> putStr "\n") fee
            let (allActs, total) = foldl' (\(as, total) e -> let (das, t) = calcActivities e in (das ++ as, total+t)) ([],0) acts
            mapM_ putStrLn allActs
            putStrLn $ "\nHours worked: " <> show total
            maybe   (pure ())
                    (\(Fee f) -> do
                        putStr "Currency units due: "
                        print $ f * (read (show total) :: Fixed E2)
                    ) fee

calcActivities :: DayActivities -> ([String],Double)
calcActivities (d, acts) = foldl' (\(texts, amount) e -> let (t,i) = act e in (t:texts, amount+i)) ([],0) acts
  where
    hours t = let (MkFixed i) = t / 3600 in fromInteger i / 1000000000000
    act (time, text) = let h = hours (nominalDiffTimeToSeconds time) in
        (show d <> " " <> printf "%5.2f" h <> " " <> Text.unpack text, h)
