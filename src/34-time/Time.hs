module Time where

import Data.Maybe
import Data.Time

-- Example date:
-- April 5, 2063
day :: Day
day = fromJust $ fromGregorianValid year month day
  where
    year = 2063
    month = 4
    day = 5

-- Adding day deltas to dates
delta :: Day
delta = 3 `addDays` day

-- Adding month deltas to dates
deltaMo :: Day
deltaMo = 8 `addGregorianMonthsClip` day

-- Number of days between two dates
diff :: Integer
diff = delta `diffDays` day

-- Example time
time :: IO UTCTime
time = getCurrentTime

-- Add NominalDiffTime (i.e. picoseconds) to the time
-- Add 5 minutes.
-- Num instance converts from integral seconds to picoseconds
tdelta :: IO UTCTime
tdelta = do
  time <- getCurrentTime
  pure (300 `addUTCTime` time)

-- Get the current time zone
zone :: IO TimeZone
zone = getCurrentTimeZone

-- Get current time with timezone attached
zonetime :: IO ZonedTime
zonetime = getZonedTime

timer :: IO NominalDiffTime
timer = do
  start <- getCurrentTime
  end <- getCurrentTime
  pure (diffUTCTime end start)
