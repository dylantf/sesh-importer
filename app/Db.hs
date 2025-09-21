module Db (testInsert) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.Time (Day)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

-- { id: 1, user_id: dylan, type: "kite", name: "Mutiny P-Series", size: "12m", active: false }
-- { id: 2, user_id: dylan, type: "kite", name: "Mutiny P-Series", size: "8m", active: false }
-- { id: 3, user_id: dylan, type: "kite", name: "Liquid Force Envy 2013", size: "12m", active: false }
-- { id: 4, user_id: dylan, type: "kite", name: "Liquid Force Envy 2013", size: "9m", active: false }
-- { id: 6, user_id: dylan, type: "kite", name: "Slingshot Fuel 2013", size: "13m", active: false }
-- { id: 7, user_id: dylan, type: "kite", name: "Slingshot Fuel 2013", size: "9m", active: false }
-- { id: 8, user_id: dylan, type: "kite", name: "Slingshot Fuel 2013", size: "7m", active: false }
-- { id: 9, user_id: dylan, type: "kite", name: "Switch Nitro v4", size: "12m", active: false }
-- { id: 10, user_id: dylan, type: "kite", name: "Best TS 2015", size: "10m", active: false }
-- { id: 11, user_id: dylan, type: "kite", name: "Best TS 2015", size: "7m", active: false }
-- { id: 12, user_id: dylan, type: "kite", name: "Liquid Force Envy 2016", size: "12m", active: false }
-- { id: 13, user_id: dylan, type: "kite", name: "Best Roca 2016", size: "10m", active: false }
-- { id: 14, user_id: dylan, type: "kite", name: "Liquid Force Wow 2018", size: "7m", active: false }
-- { id: 15, user_id: dylan, type: "kite", name: "Ozone Enduro v2", size: "9m", active: true }
-- { id: 16, user_id: dylan, type: "kite", name: "Ozone Enduro v2", size: "7m", active: true }

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

testInsert :: IO ()
testInsert = do
  let connStr = "host=localhost dbname=seshtracker_dev user=dylan password=dylan port=5432"
  runStderrLoggingT $ withPostgresqlConn connStr $ \backend -> liftIO $ do
    flip runSqlConn backend $ do
      _ <- insert $ Sesh 1 (read "2023-10-01") "Kiteboarding" 3600 (Just "Test Location") (Just "Test Comments")
      return ()
    putStrLn "Insert completed"