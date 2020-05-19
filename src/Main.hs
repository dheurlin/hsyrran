{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Data.Char
import Data.List
import Text.Read (readMaybe)

import Text.HTML.Scalpel
import Data.Time.Calendar

main :: IO ()
main = putStr =<< maybe "Failed" (concatMap show) <$> getPeriods

data Period = Period { periodName    :: String
                     , periodEntries :: [Entry]
                     }

data Entry = Entry { entryPeriodName :: String
                   , entryDate       :: Day
                   , entryOpening    :: String
                   }

instance Show Period where
  show (Period p es) = unlines $ (p <> ":") : [ "    " <> show e | e <- es ]

instance Show Entry where
  show (Entry _ dat op) = show dat <> ": " <> op


url = "https://www.systembolaget.se/butiker-ombud/oppettider-helgdagar/"

-- | Scrapes Systembolagets website for the opening hours
getPeriods :: IO (Maybe [Period])
getPeriods = scrapeURL url container
  where
    container :: Scraper String [Period]
    container =
      chroot ("div" @: [hasClass "editorial-page-content"]) $ inSerial periods

    periods :: SerialScraper String [Period]
    periods = many $ do
      name    <- seekNext $ text "h2"
      entries <- untilNext (matches "h2") (seekNext $ chroot "ul" $ texts "li")

      let year = read $ filter isDigit name :: Integer
      pure $ Period name (map (mkEntry name year) entries)


-- | Returns the year (if specified), month and day from the scraped date string
parseEntryDate :: String -> (Maybe Integer, Int, Int)
parseEntryDate s = (year, readMonth $ map toLower month, read day)
  where
    ws           = tail $ words (takeWhile okChar s)
    [day, month] = take 2 $ ws
    year         = ws .!!. 2 >>= readMaybe :: Maybe Integer

    okChar c | isAlphaNum c = True
             | c == ' '     = True
             | otherwise    = False

    readMonth "januari"   = 1
    readMonth "februari"  = 2
    readMonth "mars"      = 3
    readMonth "april"     = 4
    readMonth "maj"       = 5
    readMonth "juni"      = 6
    readMonth "juli"      = 7
    readMonth "augusti"   = 8
    readMonth "september" = 9
    readMonth "oktober"   = 10
    readMonth "november"  = 11
    readMonth "december"  = 12

-- | Creates an Entry from the scraped string
mkEntry :: String -> Integer -> String -> Entry
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
