module Main where

import Control.Monad (forM_)
import Schemas qualified

fullPath :: String -> String
fullPath filename = "/home/dylan/Desktop/Sesh Import/" ++ filename

main :: IO ()
main = do
  -- contents2012 <- Schemas.parse2012 (fullPath "2012.csv")
  -- contents2013 <- Schemas.parse2013 (fullPath "2013.csv")
  -- contents2014 <- Schemas.parse2014 (fullPath "2014.csv")
  -- contents2015 <- Schemas.parse2015 (fullPath "2015.csv")
  -- contents2016 <- Schemas.parse2016 (fullPath "2016.csv")
  contents2017 <- Schemas.parse2016 (fullPath "2017.csv")
  forM_ contents2017 $ \row -> print row
