module Schemas (Normalized, parse2012, parse2013, parse2014) where

import Data.ByteString.Lazy qualified as BL
import Data.Csv
import Data.Time
import Data.Vector qualified as V

data Sport
  = Kiteboarding
  | SUP
  | Skiing
  | Snowboarding
  | MountainBiking
  | Hiking
  | Surfing
  | WingFoiling
  | Parawinging
  deriving (Show)

data SeshType
  = Spot
  | Downwinder
  | Roundwinder
  deriving (Show)

data Normalized = Normalized
  { date :: Day,
    sport :: Sport,
    hours :: Double,
    windAvg :: Maybe Int,
    windGust :: Maybe Int,
    kiteSize :: Maybe String,
    wingSize :: Maybe String,
    seshType :: Maybe SeshType,
    location :: Maybe String,
    comments :: String
  }
  deriving (Show)

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

normalizeSport :: String -> Sport
normalizeSport s = case s of
  "Kiteboarding" -> Kiteboarding
  "SUP" -> SUP
  "Skiing" -> Skiing
  "Snowboarding" -> Snowboarding
  "Mountain Biking" -> MountainBiking
  "Hiking" -> Hiking
  "Surfing" -> Surfing
  "Surf" -> Surfing
  "Wing Foiling" -> WingFoiling
  "Parawinging" -> Parawinging
  _ -> error $ "Unknown sport: " ++ s

normalizeSeshType :: Maybe String -> Maybe SeshType
normalizeSeshType s = case s of
  Nothing -> Nothing
  Just "Spot" -> Just Spot
  Just "Downwinder" -> Just Downwinder
  Just "Roundwinder" -> Just Roundwinder
  Just other -> error $ "Unhandled session type: " ++ other

parseDate :: String -> Day
parseDate dateStr =
  case parseTimeM True defaultTimeLocale "%-m/%-d/%Y" dateStr of
    Just day -> day
    Nothing -> error $ "Could not parse date: " ++ dateStr

-- 2012

data CsvRow2012 = CsvRow2012
  { date_2012 :: String,
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
      <$> r .: "Date"
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
    { date = parseDate $ date_2012 row,
      sport = normalizeSport $ sport_2012 row,
      hours = hours_2012 row,
      windAvg = Just (windAvg_2012 row),
      windGust = Just (windGust_2012 row),
      kiteSize = Just (kiteSize_2012 row),
      wingSize = Nothing,
      seshType = normalizeSeshType $ Just $ seshType_2012 row,
      location = Nothing,
      comments = comments_2012 row
    }

parse2012 :: String -> IO [Normalized]
parse2012 path = parseCsvFile path normalize2012

-- 2013
-- CSV Header: Date,Sport,Hours,Lull,Gust,Kite,Type,Comments

data CsvRow2013 = CsvRow2013
  { date_2013 :: String,
    sport_2013 :: String,
    hours_2013 :: Double,
    windAvg_2013 :: Maybe Int,
    windGust_2013 :: Maybe Int,
    kiteSize_2013 :: Maybe String,
    seshType_2013 :: Maybe String,
    comments_2013 :: String
  }
  deriving (Show)

instance FromNamedRecord CsvRow2013 where
  parseNamedRecord r =
    CsvRow2013
      <$> r .: "Date"
      <*> r .: "Sport"
      <*> r .: "Hours"
      <*> r .: "Lull"
      <*> r .: "Gust"
      <*> r .: "Kite"
      <*> r .: "Type"
      <*> r .: "Comments"

normalize2013 :: CsvRow2013 -> Normalized
normalize2013 row =
  Normalized
    { date = parseDate $ date_2013 row,
      sport = normalizeSport $ sport_2013 row,
      hours = hours_2013 row,
      windAvg = windAvg_2013 row,
      windGust = windGust_2013 row,
      kiteSize = kiteSize_2013 row,
      wingSize = Nothing,
      seshType = normalizeSeshType $ seshType_2013 row,
      location = Nothing,
      comments = comments_2013 row
    }

parse2013 :: String -> IO [Normalized]
parse2013 path = parseCsvFile path normalize2013

-- 2014
-- Headers: Day,Sport,Hours,Lull (kn),Gust (kn),Kite Size,Type,Location,Comments

data CsvRow2014 = CsvRow2014
  { date_2014 :: String,
    sport_2014 :: String,
    hours_2014 :: Double,
    windAvg_2014 :: Maybe Int,
    windGust_2014 :: Maybe Int,
    kiteSize_2014 :: Maybe String,
    seshType_2014 :: Maybe String,
    location_2014 :: Maybe String,
    comments_2014 :: String
  }
  deriving (Show)

instance FromNamedRecord CsvRow2014 where
  parseNamedRecord r =
    CsvRow2014
      <$> r .: "Day"
      <*> r .: "Sport"
      <*> r .: "Hours"
      <*> r .: "Lull (kn)"
      <*> r .: "Gust (kn)"
      <*> r .: "Kite Size"
      <*> r .: "Type"
      <*> r .: "Location"
      <*> r .: "Comments"

normalize2014 :: CsvRow2014 -> Normalized
normalize2014 row =
  Normalized
    { date = parseDate $ date_2014 row,
      sport = normalizeSport $ sport_2014 row,
      hours = hours_2014 row,
      windAvg = windAvg_2014 row,
      windGust = windGust_2014 row,
      kiteSize = kiteSize_2014 row,
      wingSize = Nothing,
      seshType = normalizeSeshType $ seshType_2014 row,
      location = location_2014 row,
      comments = comments_2014 row
    }

parse2014 :: String -> IO [Normalized]
parse2014 path = parseCsvFile path normalize2014