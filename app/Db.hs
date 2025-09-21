module Db (insertData) where

import Control.Monad (forM_)
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

seshSql :: Query
seshSql =
  "insert into seshes (user_id, date, sport, duration_seconds, location_name, comments) values (?, ?, ?, ?, ?, ?) returning id"

seshVariables :: Normalized -> (Int, String, String, Int, Maybe String, String)
seshVariables sesh =
  ( userId,
    show $ date sesh,
    sportToString (sport sesh),
    round (hours sesh * 3600) :: Int,
    location sesh,
    comments sesh
  )

kiteboardingSeshSql :: Query
kiteboardingSeshSql =
  "insert into kiteboarding_seshes (sesh_id, wind_avg, wind_gust, sesh_type) values (?, ?, ?, ?) returning id"

kiteboardingSeshVars :: Normalized -> Int -> (Int, Maybe Int, Maybe Int, Maybe String)
kiteboardingSeshVars sesh seshId =
  ( seshId,
    windAvg sesh,
    windGust sesh,
    seshType sesh >>= Just . seshTypeToString
  )

wingFoilingSeshSql :: Query
wingFoilingSeshSql =
  "insert into wing_foiling_seshes (sesh_id, wind_avg, wind_gust, sesh_type) values (?, ?, ?, ?) returning id"

seshGearSql :: Query
seshGearSql = "insert into sesh_gear (sesh_id, gear_id) values (?, ?);"

seshGearIds :: Normalized -> [Int]
seshGearIds sesh = concatMap ($ sesh) [kiteIds, hydrofoilIds, boardIds, wingIds]

seshGearVars :: Int -> [Int] -> [(Int, Int)]
seshGearVars seshId gearIds = map (seshId,) gearIds

insertRelatedSeshData :: Connection -> Int -> Normalized -> IO ()
insertRelatedSeshData conn seshId sesh = do
  case sport sesh of
    Kiteboarding -> do
      [Only kbSeshId] <- query conn kiteboardingSeshSql $ kiteboardingSeshVars sesh seshId :: IO [Only Int]
      putStrLn $ "-- Inserted kiteboarding sesh with ID: " ++ show kbSeshId
    WingFoiling -> do
      [Only wfSeshId] <- query conn wingFoilingSeshSql $ kiteboardingSeshVars sesh seshId :: IO [Only Int]
      putStrLn $ "-- Inserted wing foiling sesh with ID: " ++ show wfSeshId
    _other -> pure ()

  case seshGearIds sesh of
    [] -> putStrLn "-- (no gear to insert)"
    gearIds -> do
      let vars = seshGearVars seshId gearIds
      putStrLn $ "-- Insterting gear: " ++ show vars
      _ <- executeMany conn seshGearSql vars
      pure ()

insertSesh :: Connection -> Normalized -> IO Int
insertSesh conn sesh = do
  [Only seshId] <- query conn seshSql (seshVariables sesh)
  putStrLn $ "Inserted sesh with id: " ++ show seshId
  _ <- insertRelatedSeshData conn seshId sesh
  pure seshId

insertData :: [Normalized] -> IO ()
insertData rows = do
  conn <- connectPostgreSQL "host=localhost port=5432 dbname=seshtracker_dev user=dylan password=dylan"
  forM_ rows $ insertSesh conn
