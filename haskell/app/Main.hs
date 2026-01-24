module Main where

import Db (insertData)
import Parsers (parseFile)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

filePath :: Int -> IO FilePath
filePath year = do
  home <- getHomeDirectory
  return $ home </> "Desktop" </> "Sesh Import" </> show year ++ ".csv"

main :: IO ()
main = do
  csvData <- concat <$> mapM runParser [2012 .. 2026]
  insertData csvData
  where
    runParser year = do
      path <- filePath year
      parseFile path year
