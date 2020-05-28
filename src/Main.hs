{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores  #-}

module Main where

import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Format
import Data.Time.Clock

import System.Posix.Signals

import Control.Concurrent (threadDelay)
import Control.Monad

import System.IO

import Data.List
import Data.Maybe
import Data.Char

import Lib.DataTypes
import Lib.Time
import Lib.Scrape
import Lib.Util
import Lib.UI

main :: IO ()
main = do
  let barHeight = 32 -- TODO get from argument

  hSetBuffering stdout NoBuffering
  tz <- getCurrentTimeZone

  loop $ do
      today    <- localDay . (utcToLocalTime tz) <$> getCurrentTime
      periods  <- getPeriods

      let upcoming = upcomingEntry today =<< periods

      let upcomingStr = maybe "" showShortEntry upcoming
      let periodStr   = maybe "" show $ do
                            ps            <- periods
                            (Entry i _ _) <- upcoming
                            pure $ ps !! i

      let handler = void $ installHandler
                              sigUSR1
                              (CatchOnce $ hello periodStr >> handler)
                              Nothing
      handler

      putStrLn upcomingStr
      threadDelay $ 3600 * 1_000_000

upcomingEntry :: Day -> [Period] -> Maybe Entry
upcomingEntry today ps = safeHead $ filter isUpcoming entries
  where
    isUpcoming (Entry _ date _) = d > 0 && d <= 7 where d = diffDays date today
    entries                     = concatMap periodEntries ps
