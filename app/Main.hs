module Main where

import Control.Monad (forM_)
import Schemas qualified

fullPath :: String -> String
fullPath filename = "/home/dylan/Desktop/Sesh Import/" ++ filename

main :: IO ()
main = do
  contents2012 <- Schemas.parse2012 (fullPath "2012.csv")
  forM_ contents2012 $ \row -> print row
