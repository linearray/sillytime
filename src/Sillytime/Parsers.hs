module Sillytime.Parsers where

import              Control.Applicative hiding (many, some)
import              Control.Category
import              Control.Monad (void,mzero)
import              Data.Char as Char
import              Data.Fixed as Fixed
import              Data.Fixed (Fixed, E12)
import              Data.Maybe (fromJust, fromMaybe)
import              Data.Scientific (floatingOrInteger)
import              Data.Text (Text)
import qualified    Data.Text as Text
import              Data.Time.Calendar
import              Data.Time.Clock
import              Data.Time.LocalTime
import              Data.Void
import              Text.Megaparsec
import              Text.Megaparsec.Char
import qualified    Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

type Y  = Integer
type M  = Int
type D  = Int

type H = Int
type Mi = Int

-- I really want to write 24:00 in the input, but `TimeOfDay` from `time`
-- won't let me, so I have to use my own type here
type MyTimeOfDay    = (H, Mi)

type DayActivities  = (Day, [Activity])
type Activity       = (NominalDiffTime, Text)

newtype Fee = Fee (Fixed E2) deriving (Eq, Show)

data Block = Block {
    blockTitle      :: String
  , blockActivities :: [ DayActivities ]
}

data SillyTime = SillyTime {
    fee         :: Maybe Fee
  , blocks      :: [ Block ] -- [activities  :: [ DayActivities ]
}

parser :: Parser SillyTime
parser = do
    fee <- optional feeP
    space -- eat up all whitespace

    mandatoryBlock <- blockP False
    otherBlocks <- many (blockP True)
    eof

    pure $ SillyTime fee (mandatoryBlock : otherBlocks)


lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"

sc :: Parser ()
sc = L.space space1 lineComment blockComment

feeP :: Parser Fee
feeP = do
    string "fee:"
    sc
    fee <- L.scientific
    eol
    sc

    case floatingOrInteger fee :: Either Double Integer of
        Left real -> pure $ Fee $ MkFixed $ round (real * 100.0)
        Right int -> pure $ Fee $ MkFixed $ int * 100

blockP :: Bool -> Parser Block
blockP headerMandatory = do
    t <-  if headerMandatory
        then (++) <$> some (single '#') <*> manyTill printChar eol
        else do
            title <- optional ((++) <$> some (single '#') <*> manyTill printChar eol)
            pure $ fromMaybe "" title
    sc
    as <- activitiesP
    sc

    pure $ Block t as

activitiesP :: Parser [DayActivities]
activitiesP = many activityDayP <?> "Activities"

activityDayP :: Parser DayActivities
activityDayP = do
    d <- dateP <?> "Date"
    space1

    acts <- many (activityP d) <?> "Day Activities"
    sc

    pure (d, acts)

-- FIXME: Beyond ugly
activityP :: Day -> Parser Activity
activityP d = do
    du <- some (durationP d) <?> "Activity Duration"
    ac <- manyTill printChar (choice [ void $ try $ lookAhead eol
                                     , void $ try $ lookAhead eof
                                     , void $ try $ do
                                        space1
                                        lookAhead $ durationP d
                                     ]
                             ) <?> "Description"

    pure (sum du, Text.pack ac)

durationP :: Day -> Parser NominalDiffTime
durationP d = do
    t1@(h1,m1) <- timeP <?> "Start time"
    single '-'
    t2@(h2,m2) <- timeP <?> "End time"
    sc

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
