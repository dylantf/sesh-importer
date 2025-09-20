module Main where

import Control.Monad (forM_)
import Schemas qualified

fullPath :: String -> String
fullPath filename = "/home/dylan/Desktop/Sesh Import/" ++ filename

main :: IO ()
main = do
  -- contents2012 <- Schemas.parse2012 (fullPath "2012.csv")
  contents2013 <- Schemas.parse2013 (fullPath "2013.csv")
  -- forM_ contents2012 $ \row -> print row
  forM_ contents2013 $ \row -> print row
