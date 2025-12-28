module Db (insertData) where

import Control.Monad (unless)
import Database.PostgreSQL.Simple
import Gear
import Parsers (Normalized (..), SeshType (..), Sport (..))

userId :: Int
userId = 1

data SeshesRow = SeshesRow
  { seshesUserId :: Int,
    seshesDate :: String,
    seshesSport :: String,
    seshesDurationSeconds :: Int,
    seshesLocationName :: Maybe String,
    seshesComments :: String
  }

data WindSeshDetailsRow = WindSeshDetailsRow
  { wsdWindAvg :: Int,
    wsdWindGust :: Int,
    wsdSeshType :: String
  }

data Prepped = Prepped
  { preppedSesh :: SeshesRow,
    preppedWindSeshDetails :: Maybe WindSeshDetailsRow,
    preppedGearIds :: [Int]
  }

sportToString :: Sport -> String
sportToString sport = case sport of
  Kiteboarding -> "kiteboarding"
  Sup -> "sup"
  Skiing -> "skiing"
  Snowboarding -> "snowboarding"
  MountainBiking -> "mountain biking"
  Hiking -> "hiking"
  Running -> "running"
  Paragliding -> "paragliding"
  Surfing -> "surfing"
  WingFoiling -> "wing foiling"
  Parawinging -> "parawinging"

seshTypeToString :: SeshType -> String
seshTypeToString seshType = case seshType of
  Spot -> "spot"
  Downwinder -> "downwinder"
  Roundwinder -> "roundwinder"

isWindSesh :: Normalized -> Bool
isWindSesh normalized = sport normalized `elem` [Kiteboarding, WingFoiling, Parawinging]

prepSesh :: Normalized -> SeshesRow
prepSesh normalized =
  SeshesRow
    { seshesUserId = userId,
      seshesDate = show $ date normalized,
      seshesSport = sportToString $ sport normalized,
      seshesLocationName = location normalized,
      seshesDurationSeconds = round $ hours normalized * 3600 :: Int,
      seshesComments = comments normalized
    }

prepWindSeshDetails :: Normalized -> Maybe WindSeshDetailsRow
prepWindSeshDetails normalized | not (isWindSesh normalized) = Nothing
prepWindSeshDetails Normalized {windAvg = Just avg, windGust = Just gust, seshType = Just st} =
  Just
    WindSeshDetailsRow
      { wsdWindAvg = avg,
        wsdWindGust = gust,
        wsdSeshType = seshTypeToString st
      }
prepWindSeshDetails _ = Nothing

prepGearIds :: Normalized -> [Int]
prepGearIds normalized = concatMap ($ normalized) [kiteIds, hydrofoilIds, boardIds, wingIds]

prepNormalizedData :: Normalized -> Prepped
prepNormalizedData normalized =
  Prepped
    { preppedSesh = prepSesh normalized,
      preppedWindSeshDetails = prepWindSeshDetails normalized,
      preppedGearIds = prepGearIds normalized
    }

seshSql :: Query
seshSql =
  "insert into seshes (user_id, date, sport, duration_seconds, location_name, comments) values (?, ?, ?, ?, ?, ?) returning id"

seshVariables :: Prepped -> (Int, String, String, Int, Maybe String, String)
seshVariables prepped =
  (seshesUserId sesh, seshesDate sesh, seshesSport sesh, seshesDurationSeconds sesh, seshesLocationName sesh, seshesComments sesh)
  where
    sesh = preppedSesh prepped

insertSeshes :: Connection -> [Prepped] -> IO [(Int, Prepped)]
insertSeshes conn prepped = do
  xs :: [Only Int] <- returning conn seshSql (map seshVariables prepped)
  let ids = zip [i | Only i <- xs] prepped
  putStrLn $ "Inserted " ++ show (length xs) ++ " seshes."
  pure ids

windSeshDetailsSql :: Query
windSeshDetailsSql =
  "insert into wind_sesh_details (sesh_id, wind_avg, wind_gust, sesh_type) values (?, ?, ?, ?)"

insertWindSeshDetails :: Connection -> [(Int, Prepped)] -> IO ()
insertWindSeshDetails conn records = do
  let toInsert = [(seshId, wsdWindAvg wsd, wsdWindGust wsd, wsdSeshType wsd) | (seshId, p) <- records, Just wsd <- [preppedWindSeshDetails p]]
  unless (null toInsert) $ do
    _ <- executeMany conn windSeshDetailsSql toInsert
    putStrLn $ "Inserted " ++ show (length toInsert) ++ " wind sesh details"

seshGearSql :: Query
seshGearSql = "insert into sesh_gear (sesh_id, gear_id) values (?, ?)"

insertSeshGear :: Connection -> [(Int, Prepped)] -> IO ()
insertSeshGear conn records = do
  let toInsert = [(seshId, gearId) | (seshId, p) <- records, gearId <- preppedGearIds p]
  unless (null toInsert) $ do
    _ <- executeMany conn seshGearSql toInsert
    putStrLn $ "Inserted " ++ show (length toInsert) ++ " sesh gear records"

insertData :: [Normalized] -> IO ()
insertData rows = do
  conn <- connectPostgreSQL "host=localhost port=5432 dbname=seshtracker_dev user=dylan password=dylan"
  let prepped = map prepNormalizedData rows
  seshesWithIds <- insertSeshes conn prepped
  insertWindSeshDetails conn seshesWithIds
  insertSeshGear conn seshesWithIds
