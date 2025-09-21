module Db (determineWingIds) where

import Data.List.Split
import Data.Maybe (mapMaybe, maybeToList)
import Data.Time (Day)
import Parsers (Normalized (..), Sport (..))

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

usesFoil :: Normalized -> Bool
usesFoil row = case boardType row of
  Just bt -> "Hydrofoil" `elem` splitValues bt
  Nothing -> False

determineHydrofoilIds :: Normalized -> [Int]
determineHydrofoilIds row =
  case (usesFoil row, date row, foil row) of
    (False, _, _) -> []
    (True, d, f) -> maybeToList $ determineHydrofoilId (d, f)

determineBoardId :: (Day, Maybe String) -> Maybe Int
determineBoardId (day, boardName) = case (day, boardName) of
  (_, Just "Groove Skate") -> Just 28
  (d, Nothing) | after "2022-08-10" d -> Just 28
  (_, Just "Rocket v2 85L") -> Just 29
  (_, Just "Rocket v2 60L") -> Just 31
  (_, Just "Rocket 60L") -> Just 31
  (_, Just "Flying Fish 40L") -> Just 30
  (_, Just "LF Galaxy") -> Just 27
  (d, Nothing) | between ("2017-06-24", "2022-08-09") d -> Just 27
  (d, Nothing) | before "2017-06-23" d -> Just 26
  _ -> Nothing

-- I only care about foilboards, throw away others
determineBoardIds :: Normalized -> [Int]
determineBoardIds row
  | usesFoil row = maybeToList $ determineBoardId (date row, board row)
  | otherwise = []

determineWingId :: (Day, String) -> Maybe Int
determineWingId (day, size) = case (day, size) of
  (_, "6m") -> Just 32
  (d, "5m")
    | before "2024-01-01" d -> Just 33
    | after "2024-01-01" d -> Just 36
  (d, "4m")
    | before "2024-01-01" d -> Just 34
    | after "2024-01-01" d -> Just 37
  (_, "5.5m") -> Just 35
  (_, "3m") -> Just 38
  _ -> Nothing

determineWingIds :: Normalized -> [Int]
determineWingIds row =
  let usesWing = case sport row of
        WingFoiling -> True
        Parawinging -> True
        _ -> False
   in case (usesWing, wingSize row) of
        (True, Just wingSizes) ->
          let sizes = splitValues wingSizes
           in mapMaybe (\size -> determineWingId (date row, size)) sizes
        _ -> []