module Schemas (Normalized, parse2012) where

import Data.ByteString.Lazy qualified as BL
import Data.Csv
import Data.Time
import Data.Vector qualified as V

data Sport
  = Kiteboarding
  | SUP
  | Skiing
  | Snowboarding
  | Surfing
  | WingFoiling
  | Parawinging
  deriving (Show)

data Normalized = Normalized
  { date :: Day,
    sport :: Sport,
    hours :: Double,
    windAvg :: Maybe Int,
    windGust :: Maybe Int,
    kiteSize :: Maybe String,
    wingSize :: Maybe String,
    seshType :: String,
    comments :: String
  }
  deriving (Show)

normalizeSport :: String -> Sport
normalizeSport s = case s of
  "Kiteboarding" -> Kiteboarding
  "SUP" -> SUP
  "Skiing" -> Skiing
  "Snowboarding" -> Snowboarding
  "Surfing" -> Surfing
  "Wing Foiling" -> WingFoiling
  "Parawinging" -> Parawinging
  _ -> error $ "Unknown sport: " ++ s

parseDate :: String -> Parser Day
parseDate dateStr =
  case parseTimeM True defaultTimeLocale "%-m/%-d/%Y" dateStr of
    Just day -> pure day
    Nothing -> error $ "Could not parse date: " ++ dateStr

data CsvRow2012 = CsvRow2012
  { date_2012 :: Day,
    sport_2012 :: String,
    hours_2012 :: Double,
    windAvg_2012 :: Int,
    windGust_2012 :: Int,
    kiteSize_2012 :: String,
    seshType_2012 :: String,
    comments_2012 :: String
  }
  deriving (Show)

instance FromNamedRecord CsvRow2012 where
  parseNamedRecord r =
    CsvRow2012
      <$> (r .: "Date" >>= parseDate)
      <*> r .: "Sport"
      <*> r .: "Hours"
      <*> r .: "Lull (kn)"
      <*> r .: "Gust (kn)"
      <*> r .: "Kite Size"
      <*> r .: "Type"
      <*> r .: "Comments"

normalize2012 :: CsvRow2012 -> Normalized
normalize2012 row =
  Normalized
    { date = date_2012 row,
      sport = normalizeSport $ sport_2012 row,
      hours = hours_2012 row,
      windAvg = Just (windAvg_2012 row),
      windGust = Just (windGust_2012 row),
      kiteSize = Just (kiteSize_2012 row),
      wingSize = Nothing,
      seshType = seshType_2012 row,
      comments = comments_2012 row
    }

readCsvFile :: (FromNamedRecord a) => String -> IO [a]
readCsvFile path = do
  contents <- BL.readFile path
  case decodeByName contents of
    Left err -> error $ "Error parsing CSV: " ++ err
    Right (_, rows) -> pure $ V.toList rows

parseCsvFile :: (FromNamedRecord a) => String -> (a -> Schemas.Normalized) -> IO [Schemas.Normalized]
parseCsvFile path normalizeFn = do
  contents <- readCsvFile path
  pure $ normalizeFn <$> contents

parse2012 :: String -> IO [Normalized]
parse2012 path = parseCsvFile path normalize2012