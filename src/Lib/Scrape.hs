{-# LANGUAGE OverloadedStrings #-}

module Lib.Scrape where

import           Text.HTML.Scalpel

import           Control.Applicative
import           Control.Monad.State

import           Data.Maybe
import           Data.List
import           Text.Read                      ( readMaybe )

import           Lib.Util
import           Lib.DataTypes
import           Lib.Time

import           Data.Char
import           Data.Time.Format
import           Data.Time.Calendar
import           Data.Time.LocalTime

url = "https://www.systembolaget.se/butiker-ombud/oppettider-helgdagar/"

-- | Scrapes Systembolagets website for the opening hours
getPeriods :: IO (Maybe [Period])
getPeriods = do
  tags <- fetchTags url
  pure $ evalState (scrapeT container tags) 0
 where
  container :: ScraperT String (State Int) [Period]
  container =
    chroot ("div" @: [ hasClass "editorial-page-content" ] ) $ inSerial periods

  periods :: SerialScraperT String (State Int) [Period]
  periods = many $ do
    name    <- seekNext $ text "h2"
    entries <- untilNext (matches "h2") (seekNext $ chroot "ul" $ texts "li")

    let year = read $ filter isDigit name :: Integer
    num <- get
    modify (+ 1)
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
