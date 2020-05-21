module Lib.DataTypes where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime

import Data.Char

import Lib.Time

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

showShortEntry :: Entry -> String
showShortEntry e = formatDay (entryDate e) <> ": !"

formatDay :: Day -> String
formatDay = formatTime sweTimeLocale "%a %d/%m"
