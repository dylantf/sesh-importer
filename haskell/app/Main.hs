module Main where

import           Db      (insertData)
import           Parsers (parseFile)

filePath :: Int -> String
filePath year = "/home/dylan/Desktop/Sesh Import/" ++ show year ++ ".csv"

main :: IO ()
main = do
  csvData <- concat <$> mapM runParser [2012 .. 2025]
  insertData csvData
  where
    runParser year = parseFile (filePath year) year
