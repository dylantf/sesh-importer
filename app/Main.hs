module Main where

import Control.Monad (forM_)
import Data.ByteString.Lazy qualified as BL
import Data.Csv
import Data.Time
import Data.Vector qualified as V

data CsvRow2012 = CsvRow2012
  { date :: Day,
    sport :: String,
    hours :: Double,
    windAvg :: Int,
    windGust :: Int,
    locationName :: String,
    seshType :: String,
    comments :: String
  }
  deriving (Show)

parseDate :: String -> Parser Day
parseDate dateStr =
  case parseTimeM True defaultTimeLocale "%-m/%-d/%Y" dateStr of
    Just day -> pure day
    Nothing -> fail $ "Could not parse date: " ++ dateStr

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

readCsvFile :: String -> IO (Either String [CsvRow2012])
readCsvFile path = do
  contents <- BL.readFile path
  case decodeByName contents :: Either String (Header, V.Vector CsvRow2012) of
    Left err -> pure $ Left err
    Right (_, rows) -> pure $ Right (V.toList rows)

main :: IO ()
main = do
  contents <- readCsvFile "/home/dylan/Desktop/Sesh Import/Sesh Log - 2012.csv"
  case contents of
    Left err -> putStrLn err
    Right rows -> forM_ rows $ \row -> do
      print row
