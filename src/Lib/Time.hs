module Lib.Time where

import Data.Time.Format
import Data.Time.LocalTime

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
