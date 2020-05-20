{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores  #-}

module Main where

import System.Posix.Signals

import Control.Applicative
import Control.Monad.State
import Control.Concurrent (threadDelay)

import Data.Char
import Data.List
import Text.Read (readMaybe)
import Data.Maybe

import Text.HTML.Scalpel

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime

data Period = Period { periodName    :: String
                     , periodEntries :: [Entry]
                     }

data Entry = Entry { entryNum     :: Int
                   , entryDate    :: Day
                   , entryOpening :: String
                   }

instance Show Period where
  show (Period p es) = unlines $ (p <> ":") : [ "    " <> show e | e <- es ]

instance Show Entry where
  show (Entry num dat opening) = formatDay dat <> ": " <> (map toLower opening)

formatDay :: Day -> String
formatDay = formatTime sweTimeLocale "%a %d/%m"

main :: IO ()
main = do
  tz       <- getCurrentTimeZone
  today    <- localDay . (utcToLocalTime tz) <$> getCurrentTime
  periods  <- getPeriods

  let upcoming = upcomingEntry today =<< periods

  let printUpcoming = putStrLn $ maybe "" show upcoming
  let printPeriod   = putStr   $ maybe "" show $ do
                        ps            <- periods
                        (Entry i _ _) <- upcoming
                        pure $ ps !! i


  void $ installHandler sigUSR1 (Catch printUpcoming) Nothing
  void $ installHandler sigUSR2 (Catch printPeriod  ) Nothing

  forever (threadDelay $ 3600 * 1_000_000)


upcomingEntry :: Day -> [Period] -> Maybe Entry
upcomingEntry today ps = safeHead $ filter isUpcoming entries
  where
    isUpcoming (Entry _ date _) = d > 0 && d <= 7 where d = diffDays date today
    entries                     = concatMap periodEntries ps

url = "https://www.systembolaget.se/butiker-ombud/oppettider-helgdagar/"

-- | Scrapes Systembolagets website for the opening hours
getPeriods :: IO (Maybe [Period])
getPeriods = do
  tags <- fetchTags url
  pure $ evalState (scrapeT container tags) 0
  where
    container :: ScraperT String (State Int) [Period]
    container =
      chroot ("div" @: [hasClass "editorial-page-content"]) $ inSerial periods

    periods :: SerialScraperT String (State Int) [Period]
    periods = many $ do
      name    <- seekNext $ text "h2"
      entries <- untilNext (matches "h2") (seekNext $ chroot "ul" $ texts "li")

      let year = read $ filter isDigit name :: Integer
      num <- get
      modify (+1)
      pure $ Period name (map (mkEntry num year) entries)


-- | Returns the year (if specified), month and day from the scraped date string
parseEntryDate :: String -> (Maybe Integer, Int, Int)
parseEntryDate s = (year, readMonth $ map toLower month, read day)
  where
    ws           = tail $ words (takeWhile okChar s)
    [day, month] = take 2 $ ws
    year         = ws .!!. 2 >>= readMaybe

    okChar c | isAlphaNum c = True
             | c == ' '     = True
             | otherwise    = False

    readMonth m =
      1 + (fromJust $ findIndex ((== m) . fst) $ months sweTimeLocale)

-- | Creates an Entry from the scraped string
mkEntry :: Int -> Integer -> String -> Entry
mkEntry name year s = Entry name date opening
  where
    dateStr    = takeWhile (/= ':') s
    opening    = dropWhile (not . isAlphaNum) $ drop (length dateStr + 1) s
    (y', m, d) = parseEntryDate dateStr
    date       = fromGregorian (maybe year id y') m d


(.!!.) :: [a] -> Int -> Maybe a
xs .!!. i
  | i >= 0 && i < length xs = Just $ xs !! i
  | otherwise               = Nothing

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

sweTimeLocale :: TimeLocale
sweTimeLocale =
    TimeLocale
        { wDays =
              [ ("söndag", "sön")
              , ("måndag", "mån")
              , ("tisdag", "tis")
              , ("onsdag", "ons")
              , ("torsdag", "tor")
              , ("fredag", "fre")
              , ("lördag", "lör")
              ]
        , months =
              [ ("januari", "jan")
              , ("februari", "feb")
              , ("mars", "mar")
              , ("april", "apr")
              , ("maj", "maj")
              , ("juni", "jun")
              , ("juli", "jul")
              , ("augusti", "aug")
              , ("september", "sep")
              , ("oktober", "okt")
              , ("november", "nov")
              , ("december", "dec")
              ]
        , amPm = ("fm", "em")
        , dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y"
        , dateFmt = "%d-%m-%y"
        , timeFmt = "%H:%M:%S"
        , time12Fmt = "%I:%M:%S %p"
        , knownTimeZones =
              [ TimeZone (1 * 60) False "CET"
              , TimeZone (2 * 60) True "CEST"
              ]
        }
