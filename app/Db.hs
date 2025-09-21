module Db (insertData) where

import Database.PostgreSQL.Simple
import Gear
import Parsers (Normalized (..), SeshType (..), Sport (..))

userId :: Int
userId = 1

sportToString :: Sport -> String
sportToString sport = case sport of
  Kiteboarding -> "kiteboarding"
  SUP -> "sup"
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

seshSql :: String
seshSql =
  "insert into seshes"
    ++ " (user_id, date, sport, duration_seconds, location_name, comments)"
    ++ " values (?, ?, ?, ?, ?, ?) returning id"

seshVariables :: Normalized -> (Int, String, String, Int, Maybe String, String)
seshVariables sesh =
  ( userId,
    show $ date sesh,
    sportToString (sport sesh),
    round (hours sesh * 3600) :: Int,
    location sesh,
    comments sesh
  )

kiteboardingSeshSql :: String
kiteboardingSeshSql =
  "insert into kiteboarding_seshes"
    ++ " (sesh_id, wind_avg, wind_gust, sesh_type) values (?, ?, ?, ?)"

kiteboardingSeshVariables :: Normalized -> (Maybe Int, Maybe Int, Maybe String)
kiteboardingSeshVariables sesh =
  ( windAvg sesh,
    windGust sesh,
    seshType sesh >>= Just . seshTypeToString
  )

wingFoilingSeshSql :: String
wingFoilingSeshSql =
  "insert into wing_foiling_seshes"
    ++ " (sesh_id, wind_avg, wind_gust, sesh_type) values (?, ?, ?, ?)"

seshGearSql :: Int -> [Int] -> String
seshGearSql seshId gearIds =
  "insert into sesh_gear (sesh_id, gear_id) values (?, ?);"

seshGearIds :: Normalized -> [Int]
seshGearIds sesh = concatMap ($ sesh) [kiteIds, hydrofoilIds, boardIds, wingIds]

seshGearVariables :: Int -> [Int] -> [(Int, Int)]
seshGearVariables seshId gearIds = map (seshId,) gearIds

insertData :: [Normalized] -> IO ()
insertData rows = do
  conn <- connectPostgreSQL "host=localhost port=5432 dbname=seshtracker_dev user=dylan password=dylan"
  [Only res] <- query conn "select ? + ?" (1 :: Int, 2 :: Int) :: IO [Only Int]
  putStrLn $ "Result is: " ++ show res
  putStrLn "Ok"
