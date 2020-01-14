module Sillytime.Parsers where

import              Control.Applicative hiding (many, some)
import              Control.Category
import              Control.Monad (void)
import              Data.Char as Char
import              Data.Maybe (fromJust)
import              Data.Text (Text)
import qualified    Data.Text as Text
import              Data.Time.Calendar
import              Data.Time.Clock
import              Data.Time.LocalTime
import              Data.Void
import              Text.Megaparsec
import              Text.Megaparsec.Char
import              Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

type Y  = Integer
type M  = Int
type D  = Int

type H = Int
type Mi = Int

-- I really want to write 24:00 in the input, but `TimeOfDay` from `time`
-- won't let me, so I have to use my own type here
type MyTimeOfDay    = (H, Mi)

type DayActivity    = (Day, [Activity])
type Activity       = (NominalDiffTime, Text)

activities :: Parser [DayActivity]
activities = many activityDayP <?> "Activities"

activityDayP :: Parser DayActivity
activityDayP = do
    d <- dateP <?> "Date"
    space1

    acts <- many (activityP d) <?> "Day Activities"
    void eol <|> eof

    pure (d, acts)


activityP :: Day -> Parser Activity
activityP d = do
    du <- some (durationP d) <?> "Activity Duration"
    ac <- manyTill printChar (choice [ void $ try $ do
                                        space1
                                        lookAhead $ durationP d
                                     , void $ try $ lookAhead eol
                                     , void $ try $ lookAhead eof
                                     ]
                             ) <?> "Description"

    pure (sum du, Text.pack ac)

durationP :: Day -> Parser NominalDiffTime
durationP d = do
    t1@(h1,m1) <- timeP <?> "Start time"
    single '-'
    t2@(h2,m2) <- timeP <?> "End time"
    space1

    let d1                 = LocalTime       d  $ fromJust $ makeTimeOfDayValid h1 m1 0
    let d2  | t2 == (24,0) = LocalTime (succ d) $ fromJust $ makeTimeOfDayValid  0  0 0
            | t2 < t1      = LocalTime (succ d) $ fromJust $ makeTimeOfDayValid h2 m2 0
            | otherwise    = LocalTime       d  $ fromJust $ makeTimeOfDayValid h2 m2 0

    pure $ diffLocalTime d2 d1

dateP :: Parser Day
dateP = do
    y <- yearP <?> "Year"
    single '/'
    m <- monthP <?> "Month"
    single '/'
    d <- dayP <?> "Day"

    pure $ fromGregorian y m d


timeP :: Parser MyTimeOfDay
timeP = do
    h <- hourP <?> "Hour"
    single ':'
    m <- minuteP <?> "Minutes"

    pure (h, m)


hourP :: Parser H
hourP = try multiDigitHour <|> singleDigitHour
  where
    singleDigitHour = c2i <$> digitChar
    multiDigitHour = do
        h1 <- oneOf ['0','1','2']
        h2 <- if h1 == '2' then oneOf ['0','1','2','3','4'] else digitChar

        pure $ c2i h1 * 10 + c2i h2

minuteP :: Parser Mi
minuteP = try multiDigitMinute <|> singleDigitMinute
  where
    singleDigitMinute = c2i <$> digitChar
    multiDigitMinute = do
        m1 <- oneOf ['0','1','2','3','4','5']
        m2 <- digitChar

        pure $ c2i m1 * 10 + c2i m2

yearP :: Parser Y
yearP = do
    y1 <- single '2'
    y2 <- digitChar
    y3 <- digitChar
    y4 <- digitChar

    pure $ toInteger $ c2i y1 * 1000 + c2i y2 * 100 + c2i y3 * 10 + c2i y4

monthP :: Parser M
monthP = try multiDigitMonth <|> singleDigitMonth
  where
    singleDigitMonth = c2i <$> digitChar
    multiDigitMonth = do
        m1 <- oneOf ['0','1']
        m2 <- if m1 == '1' then oneOf ['0','1','2'] else digitChar

        pure $ c2i m1 * 10 + c2i m2

dayP :: Parser D
dayP = try multiDigitDay <|> singleDigitDay
  where
    singleDigitDay = c2i <$> digitChar
    multiDigitDay = do
        d1 <- oneOf ['0','1','2','3']
        d2 <- if d1 == '3' then oneOf ['0','1'] else digitChar

        pure $ c2i d1 * 10 + c2i d2


c2i = fromIntegral <<< Char.digitToInt
