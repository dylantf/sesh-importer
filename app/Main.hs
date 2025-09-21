module Main where

import Control.Monad (forM_)
import Schemas qualified

importDir :: String -> String
importDir filename = "/home/dylan/Desktop/Sesh Import/" ++ filename

main :: IO ()
main = do
  let parsers =
        [ Schemas.parse2012 (importDir "2012.csv"),
          Schemas.parse2013 (importDir "2013.csv"),
          Schemas.parse2014 (importDir "2014.csv"),
          Schemas.parse2015 (importDir "2015.csv"),
          Schemas.parse2016 (importDir "2016.csv"),
          Schemas.parse2017 (importDir "2017.csv"),
          Schemas.parse2018 (importDir "2018.csv"),
          Schemas.parse2019 (importDir "2019.csv"),
          Schemas.parse2020 (importDir "2020.csv"),
          Schemas.parse2021 (importDir "2021.csv"),
          Schemas.parse2022 (importDir "2022.csv"),
          Schemas.parse2023 (importDir "2023.csv"),
          Schemas.parse2024 (importDir "2024.csv"),
          Schemas.parse2025 (importDir "2025.csv")
        ]
  csvData <- concat <$> sequence parsers
  forM_ csvData $ \row -> print row
