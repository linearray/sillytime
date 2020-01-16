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

        Right (SillyTime fee blocks) -> do
            maybe (putStrLn "No fee") (\f -> print f *> putStr "\n") fee

            let (strs,totalhours) =
                    foldl' (\(strs, total) b ->
                        let (blocktitle, (daytexts, blockhours)) = calcBlock b
                            hours = "Hours worked: " <> show blockhours
                            toadd = maybe
                                        [ hours, "\n" ]
                                        (\(Fee f) -> [ hours
                                                     , "Currency units due: " <> show (f * (read (show blockhours) :: Fixed E2))
                                                     , "\n"
                                                     ]) fee
                        in
                            (strs ++ (blocktitle : daytexts ++ toadd)
                            , total + blockhours))
                        ([],0)
                        blocks

            mapM_ putStrLn strs

            putStrLn $ "Total hours worked: " <> show totalhours

            maybe   (pure ())
                    (\(Fee f) -> do
                        putStr "Currency units due: "
                        print $ f * (read (show totalhours) :: Fixed E2)
                    ) fee

  where
    renderBlockHours :: Double -> String
    renderBlockHours h = "Hours worked: " <> show h


calcBlock :: Block -> (String, ([String], Double))
calcBlock b = ( blockTitle b,
                foldl' (\(texts,runningtotal) day ->
                    let (strs,subtotal) = calcDay day in (texts ++ strs, runningtotal + subtotal))
                    ([],0)
                    (blockActivities b) )

calcDay :: DayActivities -> ([String], Double)
calcDay (d, acts) = foldl' (\(texts, amount) e -> let (t,i) = act e in (t:texts, amount+i)) ([],0) acts
  where
    hours t = let (MkFixed i) = t / 3600 in fromInteger i / 1000000000000
    act (time, text) = let h = hours (nominalDiffTimeToSeconds time) in
        (show d <> " " <> printf "%5.2f" h <> " " <> Text.unpack text, h)
