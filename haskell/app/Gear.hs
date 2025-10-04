module Gear
  ( kiteIds,
    hydrofoilIds,
    boardIds,
    wingIds,
  )
where

import Data.Maybe (mapMaybe, maybeToList)
import Data.Time (Day)
import Parsers (BoardType (..), Normalized (..), Sport (..))

before :: String -> Day -> Bool
before dateStr d = d <= read dateStr

after :: String -> Day -> Bool
after dateStr d = d >= read dateStr

between :: (String, String) -> Day -> Bool
between (start, end) d = d >= read start && d <= read end

kiteId :: (Day, String) -> Maybe Int
kiteId (d, kiteSize) =
  case kiteSize of
    "12" | before "2013-01-15" d -> Just 1
    "8" | before "2013-01-15" d -> Just 2
    "12" | between ("2013-01-16", "2013-12-07") d -> Just 3
    "9" | between ("2013-01-16", "2013-12-07") d -> Just 4
    "13" | between ("2013-12-08", "2014-12-31") d -> Just 6
    "9" | between ("2013-12-08", "2014-12-31") d -> Just 7
    "7" | between ("2013-12-08", "2014-12-31") d -> Just 8
    "12" | between ("2015-01-01", "2017-02-05") d -> Just 9
    "10" | between ("2016-10-09", "2018-01-24") d -> Just 10
    "7" | between ("2016-10-09", "2019-10-04") d -> Just 11
    "12" | between ("2017-02-06", "2022-08-10") d -> Just 12
    "10" | between ("2018-01-25", "2020-12-31") d -> Just 13
    "7" | between ("2019-10-05", "2020-12-31") d -> Just 14
    "9" | after "2021-01-01" d -> Just 15
    "7" | after "2021-01-01" d -> Just 16
    _ -> Nothing

kiteIds :: Normalized -> [Int]
kiteIds row =
  case (sport row, kiteSize row) of
    (Kiteboarding, Just kiteSizes) ->
      mapMaybe (\sz -> kiteId (date row, sz)) kiteSizes
    _ -> []

hydrofoilId :: Day -> Maybe String -> Maybe Int
hydrofoilId d foilName = case foilName of
  Nothing | before "2019-05-26" d -> Just 17
  Nothing | after "2019-05-27" d -> Just 18
  Just "Thruster" -> Just 18
  Just "ART 999" -> Just 19
  Just "ART 799" -> Just 20
  Just "Phantom 1480" -> Just 21
  Just "Seven Seas 1200" -> Just 22
  Just "Phantom-S 840" -> Just 23
  Just "Eagle 890" -> Just 24
  Just "Eagle 990" -> Just 25
  Just "Ypra-S 785" -> Just 26
  Just "Ypra-S 1000" -> Just 27
  Just "Veloce 890" -> Just 28
  _ -> Nothing

isFoilSesh :: Normalized -> Bool
isFoilSesh row = case boardType row of
  Just types -> Hydrofoil `elem` types
  Nothing -> False

hydrofoilIds :: Normalized -> [Int]
hydrofoilIds row =
  case (isFoilSesh row, foil row) of
    (False, _) -> []
    (True, Nothing) -> maybeToList $ hydrofoilId seshDate Nothing
    (True, Just foils) -> mapMaybe (hydrofoilId seshDate . Just) foils
  where
    seshDate = date row

boardId :: Day -> Maybe String -> BoardType -> Maybe Int
boardId d boardName bt =
  case bt of
    Hydrofoil ->
      case boardName of
        Just "Groove Skate" -> Just 31
        Nothing | after "2022-08-10" d -> Just 31
        Just "Rocket v2 85L" -> Just 32
        Just "Rocket v2 60L" -> Just 34
        Just "Rocket 60L" -> Just 34
        Just "Flying Fish 40L" -> Just 33
        Just "LF Galaxy" -> Just 30
        Nothing | between ("2017-06-24", "2022-08-09") d -> Just 30
        Nothing | before "2017-06-23" d -> Just 17
        _ -> Nothing
    Surfboard -> Just 43
    Twintip -> Just 44
    Skis -> Just 45
    Snowboard -> Just 46
    _ -> Nothing

boardIds :: Normalized -> [Int]
boardIds row = case boardType row of
  Just boardTypes -> mapMaybe (boardId seshDate boardName) boardTypes
  Nothing -> []
  where
    seshDate = date row
    boardName = board row

wingId :: (Sport, Day, String) -> Maybe Int
wingId (sport, day, size) = case (sport, size) of
  (_, "6m") -> Just 35
  (_, "5m")
    | before "2024-01-01" day -> Just 36
    | after "2024-01-01" day -> Just 39
  (WingFoiling, "4m")
    | before "2024-01-01" day -> Just 37
    | after "2024-01-01" day -> Just 40
  (Parawinging, "4m") -> Just 42
  (_, "5.5m") -> Just 38
  (_, "3m") -> Just 41
  _ -> Nothing

wingIds :: Normalized -> [Int]
wingIds row =
  let usesWing = case sport row of
        WingFoiling -> True
        Parawinging -> True
        _ -> False
   in case (usesWing, wingSize row) of
        (True, Just wingSizes) ->
          mapMaybe (\size -> wingId (sport row, date row, size)) wingSizes
        _ -> []