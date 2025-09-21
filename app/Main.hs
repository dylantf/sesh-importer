module Main where

import Db (insertData)
import Parsers qualified

importDir :: String -> String
importDir filename = "/home/dylan/Desktop/Sesh Import/" ++ filename

main :: IO ()
main = do
  let parsers =
        [ Parsers.parse2012 (importDir "2012.csv"),
          Parsers.parse2013 (importDir "2013.csv"),
          Parsers.parse2014 (importDir "2014.csv"),
          Parsers.parse2015 (importDir "2015.csv"),
          Parsers.parse2016 (importDir "2016.csv"),
          Parsers.parse2017 (importDir "2017.csv"),
          Parsers.parse2018 (importDir "2018.csv"),
          Parsers.parse2019 (importDir "2019.csv"),
          Parsers.parse2020 (importDir "2020.csv"),
          Parsers.parse2021 (importDir "2021.csv"),
          Parsers.parse2022 (importDir "2022.csv"),
          Parsers.parse2023 (importDir "2023.csv"),
          Parsers.parse2024 (importDir "2024.csv"),
          Parsers.parse2025 (importDir "2025.csv")
        ]
  csvData <- concat <$> sequence parsers
  _ <- insertData csvData
  putStrLn "Done"
