module Db (testInsert, determineKiteIds) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.List.Split
import Data.Maybe (mapMaybe)
import Data.Time (Day)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Parsers (Normalized (..), Sport (..))

-- { id: 17, user_id: dylan, type: "hydrofoil", name: "Slingshot Hover Glide NF2", size: "707", active: false }
-- { id: 18, user_id: dylan, type: "hydrofoil", name: "Liquid Force Thruster", size: "650", active: false }
-- { id: 19, user_id: dylan, type: "hydrofoil", name: "Axis ART 999", size: "999", active: false }
-- { id: 20, user_id: dylan, type: "hydrofoil", name: "Axis ART 799", size: "799", active: false }
-- { id: 21, user_id: dylan, type: "hydrofoil", name: "F-One Seven Seas", size: "1200", active: false }
-- { id: 22, user_id: dylan, type: "hydrofoil", name: "F-One Phantom-S", size: "840", active: true }
-- { id: 23, user_id: dylan, type: "hydrofoil", name: "Gong Ypra-S v2", size: "780", active: true }
-- { id: 24, user_id: dylan, type: "hydrofoil", name: "Gong Ypra-S v2", size: "1000", active: true }
-- { id: 25, user_id: dylan, type: "hydrofoil", name: "Gong Veloce v3", size: "890", active: true }

-- { id: 26, user_id: dylan, type: "foilboard", name: "Slingshot Alien Air", size: "40L", active: false }
-- { id: 27, user_id: dylan, type: "foilboard", name: "Liquid Force Galaxy", size: "4'2", active: false }
-- { id: 28, user_id: dylan, type: "foilboard", name: "Groove Skate", size: "120", active: true }
-- { id: 29, user_id: dylan, type: "foilboard", name: "Rocket Wing v2", size: "85L", active: false }
-- { id: 30, user_id: dylan, type: "foilboard", name: "Flying Fish", size: "40L", active: false }
-- { id: 31, user_id: dylan, type: "foilboard", name: "Rocket Wing v2", size: "60L", active: true }

-- { id: 32, user_id: dylan, type: "wing", name: "Cabrinha Crosswing X2", size: "6m", active: false }
-- { id: 33, user_id: dylan, type: "wing", name: "Ozone WASP v2", size: "5m", active: false }
-- { id: 34, user_id: dylan, type: "wing", name: "F-One Strike v2", size: "4m", active: false }
-- { id: 35, user_id: dylan, type: "wing", name: "Duotone Unit 2022", size: "5.5m", active: false }
-- { id: 36, user_id: dylan, type: "wing", name: "Gong Neutra 2024", size: "5m", active: true }
-- { id: 37, user_id: dylan, type: "wing", name: "Gong Neutra 2024", size: "4m", active: true }
-- { id: 38, user_id: dylan, type: "wing", name: "Gong Droid 2024", size: "3m", active: true }

mkPersist
  sqlSettings
  [persistLowerCase|
Sesh sql="seshes"
    userId Int sql="user_id"
    date Day sql="date"
    sport String sql="sport"
    durationSeconds Int sql="duration_seconds"
    locationName String Maybe sql="location_name"
    comments String Maybe sql="comments"
    deriving Show

KiteboardingSesh sql="kiteboarding_seshes"
    seshId Int sql="sesh_id"
    windAvg Int sql="wind_avg"
    windGust Int sql="wind_gust"
    seshType String sql="sesh_type"
    deriving Show

WingFoilingSesh sql="wing_foiling_seshes"
    seshId Int sql="sesh_id"
    windAvg Int sql="wind_avg"
    windGust Int sql="wind_gust"
    seshType String sql="sesh_type"
    deriving Show

SeshGear sql="sesh_gear"
    seshId Int sql="sesh_id"
    gearId Int sql="gear_id"
    deriving Show
|]

-- Date range helpers

before :: String -> Day -> Bool
before dateStr d = d <= read dateStr

after :: String -> Day -> Bool
after dateStr d = d >= read dateStr

between :: (String, String) -> Day -> Bool
between (start, end) d = d >= read start && d <= read end

-- Heuristics to determine which gear IDs were used based on date and sizes

determineKiteId :: (Day, String) -> Maybe Int
determineKiteId (seshDate, kiteSize) = case (seshDate, kiteSize) of
  (d, "12m") | before "2013-01-15" d -> Just 1
  (d, "8m") | before "2013-01-15" d -> Just 2
  (d, "12m") | between ("2013-01-16", "2013-12-07") d -> Just 3
  (d, "9m") | between ("2013-01-16", "2013-12-07") d -> Just 4
  (d, "13m") | between ("2013-12-08", "2014-12-31") d -> Just 6
  (d, "9m") | between ("2013-12-08", "2014-12-31") d -> Just 7
  (d, "7m") | between ("2013-12-08", "2014-12-31") d -> Just 8
  (d, "12m") | between ("2015-01-01", "2017-02-05") d -> Just 9
  (d, "10m") | between ("2016-10-09", "2018-01-24") d -> Just 10
  (d, "7m") | between ("2016-10-09", "2019-10-04") d -> Just 11
  (d, "12m") | between ("2017-02-06", "2022-08-10") d -> Just 12
  (d, "10m") | between ("2018-01-25", "2020-12-31") d -> Just 13
  (d, "7m") | between ("2019-10-05", "2020-12-31") d -> Just 14
  (d, "9m") | after "2021-01-01" d -> Just 15
  (d, "7m") | after "2021-01-01" d -> Just 16
  _ -> Nothing

determineKiteIds :: Normalized -> [Int]
determineKiteIds row =
  case (sport row, kiteSize row) of
    (Kiteboarding, Just kiteSizes) ->
      let sizes = splitOn "," kiteSizes
       in mapMaybe (\size -> determineKiteId (date row, size)) sizes
    _ -> []

testInsert :: IO ()
testInsert = do
  let connStr = "host=localhost dbname=seshtracker_dev user=dylan password=dylan port=5432"
  runStderrLoggingT $ withPostgresqlConn connStr $ \backend -> liftIO $ do
    flip runSqlConn backend $ do
      _ <- insert $ Sesh 1 (read "2023-10-01") "Kiteboarding" 3600 (Just "Test Location") (Just "Test Comments")
      return ()
    putStrLn "Insert completed"