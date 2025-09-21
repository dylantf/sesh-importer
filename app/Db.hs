module Db (determineHydrofoilIds) where

import Data.List.Split
import Data.Maybe (mapMaybe, maybeToList)
import Data.Time (Day)
import Parsers (Normalized (..), Sport (..))

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

splitValues :: String -> [String]
splitValues = splitOn ","

determineKiteIds :: Normalized -> [Int]
determineKiteIds row =
  case (sport row, kiteSize row) of
    (Kiteboarding, Just kiteSizes) ->
      let sizes = splitValues kiteSizes
       in mapMaybe (\size -> determineKiteId (date row, size)) sizes
    _ -> []

determineHydrofoilId :: (Day, Maybe String) -> Maybe Int
determineHydrofoilId (day, foilName) = case (day, foilName) of
  (d, Nothing) | before "2019-05-26" d -> Just 17
  (d, Nothing) | after "2019-05-27" d -> Just 18
  (_, Just "Thruster") -> Just 18
  (_, Just "ART 999") -> Just 19
  (_, Just "ART 799") -> Just 20
  (_, Just "Phantom 1480") -> Just 21
  (_, Just "Seven Seas 1200") -> Just 22
  (_, Just "Phantom-S 840") -> Just 23
  (_, Just "Eagle 890") -> Just 24
  (_, Just "Eagle 990") -> Just 25
  (_, Just "Ypra-S 785") -> Just 26
  (_, Just "Ypra-S 1000") -> Just 27
  (_, Just "Veloce 890") -> Just 28
  _ -> Nothing

determineHydrofoilIds :: Normalized -> [Int]
determineHydrofoilIds row =
  case (usesFoil, date row, foil row) of
    (False, _, _) -> []
    (True, d, f) -> maybeToList $ determineHydrofoilId (d, f)
  where
    usesFoil = case boardType row of
      Just bt -> "Hydrofoil" `elem` splitValues bt
      Nothing -> False
