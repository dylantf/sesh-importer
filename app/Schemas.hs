module Schemas (Normalized, parse2012, parse2013, parse2014, parse2015) where

import Data.ByteString.Lazy qualified as BL
import Data.Csv
import Data.Functor ((<&>))
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

newtype Normalized2012 = Normalized2012 {normalize2012 :: Normalized}

instance FromNamedRecord Normalized2012 where
  parseNamedRecord r = do
    normalized <-
      Normalized
        <$> (r .: "Date" <&> parseDate)
        <*> (normalizeSport <$> r .: "Sport")
        <*> r .: "Hours"
        <*> (Just <$> r .: "Lull (kn)")
        <*> (Just <$> r .: "Gust (kn)")
        <*> (Just <$> r .: "Kite Size")
        <*> pure Nothing
        <*> (normalizeSeshType . Just <$> r .: "Type")
        <*> pure Nothing
        <*> r .: "Comments"
    pure $ Normalized2012 normalized

parse2012 :: String -> IO [Normalized]
parse2012 path = map normalize2012 <$> readCsvFile path

-- 2013

newtype Normalized2013 = Normalized2013 {normalize2013 :: Normalized}

instance FromNamedRecord Normalized2013 where
  parseNamedRecord r = do
    normalized <-
      Normalized
        <$> (r .: "Date" <&> parseDate)
        <*> (r .: "Sport" <&> normalizeSport)
        <*> r .: "Hours"
        <*> r .: "Lull"
        <*> r .: "Gust"
        <*> r .: "Kite"
        <*> pure Nothing
        <*> (r .: "Type" <&> normalizeSeshType)
        <*> pure Nothing
        <*> r .: "Comments"
    pure $ Normalized2013 normalized

parse2013 :: String -> IO [Normalized]
parse2013 path = map normalize2013 <$> readCsvFile path

-- 2014

newtype Normalized2014 = Normalized2014 {normalize2014 :: Normalized}

instance FromNamedRecord Normalized2014 where
  parseNamedRecord r = do
    normalized <-
      Normalized
        <$> (r .: "Day" <&> parseDate)
        <*> (r .: "Sport" <&> normalizeSport)
        <*> r .: "Hours"
        <*> r .: "Lull (kn)"
        <*> r .: "Gust (kn)"
        <*> r .: "Kite Size"
        <*> pure Nothing
        <*> (r .: "Type" <&> normalizeSeshType)
        <*> r .: "Location"
        <*> r .: "Comments"
    pure $ Normalized2014 normalized

parse2014 :: String -> IO [Normalized]
parse2014 path = map normalize2014 <$> readCsvFile path

-- 2015

newtype Normalized2015 = Normalized2015 {normalize2015 :: Normalized}

instance FromNamedRecord Normalized2015 where
  parseNamedRecord r = do
    normalized <-
      Normalized
        <$> (r .: "Date" <&> parseDate)
        <*> (r .: "Sport" <&> normalizeSport)
        <*> r .: "Hours"
        <*> r .: "Lull"
        <*> r .: "Gust"
        <*> r .: "Kite"
        <*> pure Nothing
        <*> (r .: "Type" <&> normalizeSeshType)
        <*> r .: "Location"
        <*> r .: "Comments"
    pure $ Normalized2015 normalized

parse2015 :: String -> IO [Normalized]
parse2015 path = map normalize2015 <$> readCsvFile path