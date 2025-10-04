module Db (insertData) where

import Control.Monad (forM_, when)
import Database.PostgreSQL.Simple
import Gear
import Parsers (Normalized (..), SeshType (..), Sport (..))

userId :: Int
userId = 1

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

windSeshDetailsSql :: Query
windSeshDetailsSql =
  "insert into wind_sesh_details (sesh_id, wind_avg, wind_gust, sesh_type) values (?, ?, ?, ?) returning id"

windSeshDetailsVars :: Normalized -> Int -> (Int, Maybe Int, Maybe Int, Maybe String)
windSeshDetailsVars sesh seshId =
  ( seshId,
    windAvg sesh,
    windGust sesh,
    seshType sesh >>= Just . seshTypeToString
  )

seshGearSql :: Query
seshGearSql = "insert into sesh_gear (sesh_id, gear_id) values (?, ?);"

seshGearIds :: Normalized -> [Int]
seshGearIds sesh = concatMap ($ sesh) [kiteIds, hydrofoilIds, boardIds, wingIds]

seshGearVars :: Int -> [Int] -> [(Int, Int)]
seshGearVars seshId gearIds = map (seshId,) gearIds

isWindSesh :: Normalized -> Bool
isWindSesh normalized = case sport normalized of
  Kiteboarding -> True
  WingFoiling -> True
  Parawinging -> True
  _ -> False

insertSeshDetails :: Connection -> Int -> Normalized -> IO ()
insertSeshDetails conn seshId sesh = do
  when (isWindSesh sesh) $ do
    [Only kbSeshId] <- query conn windSeshDetailsSql $ windSeshDetailsVars sesh seshId :: IO [Only Int]
    putStrLn $ "-- Inserted wind sesh details with ID: " ++ show kbSeshId

insertSeshGear :: Connection -> Int -> Normalized -> IO ()
insertSeshGear conn seshId sesh =
  case seshGearIds sesh of
    [] -> pure ()
    gearIds -> do
      let vars = seshGearVars seshId gearIds
      putStrLn $ "-- Insterting gear: " ++ show vars
      _ <- executeMany conn seshGearSql vars
      pure ()

insertSesh :: Connection -> Normalized -> IO Int
insertSesh conn sesh = do
  [Only seshId] <- query conn seshSql (seshVariables sesh)
  putStrLn $ "Inserted sesh with id: " ++ show seshId
  _ <- insertSeshDetails conn seshId sesh
  _ <- insertSeshGear conn seshId sesh
  pure seshId

insertData :: [Normalized] -> IO ()
insertData rows = do
  conn <- connectPostgreSQL "host=localhost port=5432 dbname=seshtracker_dev user=dylan password=dylan"
  forM_ rows $ insertSesh conn
