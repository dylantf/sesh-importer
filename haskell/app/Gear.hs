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

kiteId :: Day -> String -> Maybe Int
kiteId d kiteSize =
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
    _ -> error $ "Missing gear ID for `" ++ kiteSize ++ "` on date: " ++ show d

kiteIds :: Normalized -> [Int]
kiteIds row
  | sport row /= Kiteboarding = []
  | otherwise = foldMap (mapMaybe (kiteId (date row))) (kiteSize row)

hydrofoilId :: Day -> Maybe String -> Maybe Int
hydrofoilId d Nothing
  | before "2019-05-26" d = Just 17
  | otherwise = Just 18
hydrofoilId _ (Just foilName) = case foilName of
  "Thruster" -> Just 18
  "ART 999" -> Just 19
  "ART 799" -> Just 20
  "Phantom 1480" -> Just 21
  "Seven Seas 1200" -> Just 22
  "Phantom-S 840" -> Just 23
  "Eagle 890" -> Just 24
  "Eagle 990" -> Just 25
  "Ypra-S 785" -> Just 26
  "Ypra-S 1000" -> Just 27
  "Veloce 890" -> Just 28
  _ -> Nothing

isFoilSesh :: Normalized -> Bool
isFoilSesh row = case boardType row of
  Just types -> Hydrofoil `elem` types
  Nothing -> False

hydrofoilIds :: Normalized -> [Int]
hydrofoilIds row
  | not $ isFoilSesh row = []
  | otherwise = case foil row of
      Nothing -> maybeToList $ hydrofoilId seshDate Nothing
      Just foils -> mapMaybe (hydrofoilId seshDate . Just) foils
  where
    seshDate = date row

boardId :: Day -> Maybe String -> BoardType -> Maybe Int
boardId date boardName Hydrofoil =
  case boardName of
    Nothing
      | between ("2017-06-24", "2022-08-09") date -> Just 30
      | before "2017-06-23" date -> Just 17
      | after "2022-08-10" date -> Just 31
    Just "Groove Skate" -> Just 31
    Just "Rocket v2 85L" -> Just 32
    Just "Rocket v2 60L" -> Just 34
    Just "Rocket 60L" -> Just 34
    Just "Flying Fish 40L" -> Just 33
    Just "LF Galaxy" -> Just 30
    _ -> Nothing
boardId _ _ bt =
  case bt of
    Surfboard -> Just 43
    Twintip -> Just 44
    Skis -> Just 45
    Snowboard -> Just 46
    _ -> Nothing

boardIds :: Normalized -> [Int]
boardIds row =
  foldMap (mapMaybe mapBoardId) (boardType row)
  where
    mapBoardId = boardId (date row) (board row)

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

usesWing :: Sport -> Bool
usesWing sport
  | sport `elem` [WingFoiling, Parawinging] = True
  | otherwise = False

wingIds :: Normalized -> [Int]
wingIds row
  | not $ usesWing $ sport row = []
  | otherwise = foldMap (mapMaybe (wingId . (sport row,date row,))) (wingSize row)