module Time where

import Data.Maybe
import Data.Time
import Data.Time.Format.ISO8601

-- April 5, 2063
day :: Day
day = fromJust (fromGregorianValid year month day)
  where
    year = 2063
    month = 4
    day = 5

printing :: IO ()
printing = do
  t <- getCurrentTime
  zt <- getZonedTime
  print (iso8601Show day)
  print (iso8601Show t)
  print (iso8601Show zt)

parsing :: IO ()
parsing = do
  d <- iso8601ParseM "2063-04-05" :: IO Day
  t <- iso8601ParseM "2020-01-29T15:03:43.013033515Z" :: IO UTCTime
  zt <- iso8601ParseM "2020-01-29T15:03:43.013040029+00:00" :: IO ZonedTime
  print d
  print t
  print zt
