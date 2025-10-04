module Parsers
  ( Normalized (..),
    Sport (..),
    SeshType (..),
    BoardType (..),
    parse2012,
    parse2013,
    parse2014,
    parse2015,
    parse2016,
    parse2017,
    parse2018,
    parse2019,
    parse2020,
    parse2021,
    parse2022,
    parse2023,
    parse2024,
    parse2025,
  )
where

import Data.ByteString.Lazy qualified as BL
import Data.Char (isSpace)
import Data.Csv
import Data.Functor ((<&>))
import Data.List
import Data.List.Split
import Data.Time
import Data.Vector qualified as V

data Sport
  = Kiteboarding
  | Sup
  | Skiing
  | Snowboarding
  | MountainBiking
  | Hiking
  | Running
  | Paragliding
  | Surfing
  | WingFoiling
  | Parawinging
  deriving (Show)

data SeshType
  = Spot
  | Downwinder
  | Roundwinder
  deriving (Show)

data BoardType
  = Twintip
  | Hydrofoil
  | Surfboard
  | SupBoard
  | Skis
  | Snowboard
  | Other
  deriving (Show, Eq)

data Normalized = Normalized
  { date :: Day,
    sport :: Sport,
    hours :: Double,
    windAvg :: Maybe Int,
    windGust :: Maybe Int,
    kiteSize :: Maybe [String],
    wingSize :: Maybe [String],
    seshType :: Maybe SeshType,
    boardType :: Maybe [BoardType],
    foil :: Maybe [String],
    board :: Maybe String,
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

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

replace :: String -> String -> String -> String
replace from with = go
  where
    go [] = []
    go s@(x : xs)
      | from `isPrefixOf` s = with ++ go (drop (length from) s)
      | otherwise = x : go xs

maybeString :: String -> Maybe String
maybeString str = case trim str of
  "" -> Nothing
  s -> Just s

parseMany :: String -> Maybe [String]
parseMany str =
  case splitOn "," . trim <$> maybeString str of
    Nothing -> Nothing
    Just [] -> Nothing
    Just xs -> Just xs

normalizeSport :: String -> Sport
normalizeSport s = case s of
  "Kiteboarding" -> Kiteboarding
  "SUP" -> Sup
  "Skiing" -> Skiing
  "Snowboarding" -> Snowboarding
  "Mountain Biking" -> MountainBiking
  "Hiking" -> Hiking
  "Running" -> Running
  "Paragliding" -> Paragliding
  "Surfing" -> Surfing
  "Surf" -> Surfing
  "Wing foiling" -> WingFoiling
  "Parawinging" -> Parawinging
  _ -> error $ "Unhandled sport: `" ++ s ++ "`"

normalizeSeshType :: Maybe String -> Maybe SeshType
normalizeSeshType s = case s of
  Nothing -> Nothing
  Just "Spot" -> Just Spot
  Just "Downwinder" -> Just Downwinder
  Just "Roundwinder" -> Just Roundwinder
  Just other -> error $ "Unhandled session type: " ++ other

normalizeBoardType :: String -> BoardType
normalizeBoardType bt = case trim bt of
  "Twintip" -> Twintip
  "Twintp" -> Twintip
  "Hydrofoil" -> Hydrofoil
  "Surfboard" -> Surfboard
  "Strapless" -> Surfboard
  "SUP" -> SupBoard
  "Skis" -> Skis
  "Snowboard" -> Snowboard
  "Skim" -> Other
  other -> error $ "Unhandled board type `" ++ other ++ "`"

parseDate :: String -> Day
parseDate dateStr =
  case parseTimeM True defaultTimeLocale "%-m/%-d/%Y" dateStr of
    Just day -> day
    Nothing -> error $ "Could not parse date: " ++ dateStr

-- Parse and normalize: some kites contain "m" in the size, but some don't
parseKiteSize :: String -> Maybe [String]
parseKiteSize str = map normalizeSize <$> parseMany str
  where
    normalizeSize = replace "m" ""

-- Parse maybe-CSV list of strings into board types
parseBoardType :: String -> Maybe [BoardType]
parseBoardType bt = map normalizeBoardType <$> parseMany bt

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
        <*> (r .: "Kite Size" <&> parseKiteSize)
        <*> pure Nothing
        <*> (normalizeSeshType . Just <$> r .: "Type")
        <*> pure Nothing
        <*> pure Nothing
        <*> pure Nothing
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
        <*> (r .: "Kite" <&> parseKiteSize)
        <*> pure Nothing
        <*> (r .: "Type" <&> normalizeSeshType)
        <*> pure Nothing
        <*> pure Nothing
        <*> pure Nothing
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
        <*> (r .: "Kite Size" <&> parseKiteSize)
        <*> pure Nothing
        <*> (r .: "Type" <&> normalizeSeshType)
        <*> pure Nothing
        <*> pure Nothing
        <*> pure Nothing
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
        <*> (r .: "Kite" <&> parseKiteSize)
        <*> pure Nothing
        <*> (r .: "Type" <&> normalizeSeshType)
        <*> pure Nothing
        <*> pure Nothing
        <*> pure Nothing
        <*> r .: "Location"
        <*> r .: "Comments"
    pure $ Normalized2015 normalized

parse2015 :: String -> IO [Normalized]
parse2015 path = map normalize2015 <$> readCsvFile path

-- 2016

newtype Normalized2016 = Normalized2016 {normalize2016 :: Normalized}

normalize2016Record :: NamedRecord -> Parser Normalized
normalize2016Record r = do
  Normalized
    <$> (r .: "Date" <&> parseDate)
    <*> (r .: "Sport" <&> normalizeSport)
    <*> r .: "Hours"
    <*> r .: "Lull (kts)"
    <*> r .: "Gust (kts)"
    <*> (r .: "Kite" <&> parseKiteSize)
    <*> pure Nothing
    <*> (r .: "Type" <&> normalizeSeshType)
    <*> (r .: "Board" <&> parseBoardType)
    <*> pure Nothing
    <*> pure Nothing
    <*> r .: "Location"
    <*> r .: "Comments"

instance FromNamedRecord Normalized2016 where
  parseNamedRecord r = Normalized2016 <$> normalize2016Record r

parse2016 :: String -> IO [Normalized]
parse2016 path = map normalize2016 <$> readCsvFile path

parse2017 :: String -> IO [Normalized]
parse2017 path = map normalize2016 <$> readCsvFile path

parse2018 :: String -> IO [Normalized]
parse2018 path = map normalize2016 <$> readCsvFile path

parse2019 :: String -> IO [Normalized]
parse2019 path = map normalize2016 <$> readCsvFile path

parse2020 :: String -> IO [Normalized]
parse2020 path = map normalize2016 <$> readCsvFile path

parse2021 :: String -> IO [Normalized]
parse2021 path = map normalize2016 <$> readCsvFile path

-- 2022 (Addition of foil and foilboard)

newtype Normalized2022 = Normalized2022 {normalize2022 :: Normalized}

instance FromNamedRecord Normalized2022 where
  parseNamedRecord r = do
    normalized <-
      Normalized
        <$> (r .: "Date" <&> parseDate)
        <*> (r .: "Sport" <&> normalizeSport)
        <*> r .: "Hours"
        <*> r .: "Avg (kts)"
        <*> r .: "Gust (kts)"
        <*> (r .: "Kite" <&> parseKiteSize)
        <*> pure Nothing
        <*> (r .: "Type" <&> normalizeSeshType)
        <*> (r .: "Board Type" <&> parseBoardType)
        <*> (r .: "Foil" <&> parseMany)
        <*> r .: "Foil Board" -- I only tracked foilboards for 2022
        <*> r .: "Location"
        <*> r .: "Comments"
    pure $ Normalized2022 normalized

parse2022 :: String -> IO [Normalized]
parse2022 path = map normalize2022 <$> readCsvFile path

-- 2023
-- The column order is different but have the same names as 2022

parse2023 :: String -> IO [Normalized]
parse2023 path = map normalize2022 <$> readCsvFile path

-- 2024 (Addition of wing !)

newtype Normalized2024 = Normalized2024 {normalize2024 :: Normalized}

instance FromNamedRecord Normalized2024 where
  parseNamedRecord r = do
    normalized <-
      Normalized
        <$> (r .: "Date" <&> parseDate)
        <*> (r .: "Sport" <&> normalizeSport)
        <*> r .: "Hours"
        <*> r .: "Avg (kts)"
        <*> r .: "Gust (kts)"
        <*> (r .: "Kite" <&> parseKiteSize)
        <*> (r .: "Wing" <&> parseMany)
        <*> (r .: "Type" <&> normalizeSeshType)
        <*> (r .: "Board Type" <&> parseBoardType)
        <*> (r .: "Foil" <&> parseMany)
        <*> r .: "Foil Board" -- I only tracked foilboards for 2024
        <*> r .: "Location"
        <*> r .: "Comments"
    pure $ Normalized2024 normalized

parse2024 :: String -> IO [Normalized]
parse2024 path = map normalize2024 <$> readCsvFile path

-- 2025
-- ,Date,Sport,Hours,Avg (kts),Gust (kts),Kite,Wing,Board Type,Foil,Board,Location,Type,Comments
newtype Normalized2025 = Normalized2025 {normalize2025 :: Normalized}

instance FromNamedRecord Normalized2025 where
  parseNamedRecord r = do
    normalized <-
      Normalized
        <$> (r .: "Date" <&> parseDate)
        <*> (r .: "Sport" <&> normalizeSport)
        <*> r .: "Hours"
        <*> r .: "Avg (kts)"
        <*> r .: "Gust (kts)"
        <*> (r .: "Kite" <&> parseKiteSize)
        <*> (r .: "Wing" <&> parseMany)
        <*> (r .: "Type" <&> normalizeSeshType)
        <*> (r .: "Board Type" <&> parseBoardType)
        <*> (r .: "Foil" <&> parseMany)
        <*> r .: "Board"
        <*> r .: "Location"
        <*> r .: "Comments"
    pure $ Normalized2025 normalized

parse2025 :: String -> IO [Normalized]
parse2025 path = map normalize2025 <$> readCsvFile path
