module Lib.Util where

import System.Process
import System.Posix.Signals
import Control.Monad (void)

(.!!.) :: [a] -> Int -> Maybe a
xs .!!. i
  | i >= 0 && i < length xs = Just $ xs !! i
  | otherwise               = Nothing

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

loop :: IO () -> IO ()
loop m = m >> (loop m)

notify :: String -> IO ()
notify s = void $ system $ "notify-send \"" <> s <> "\""

waitForSignal :: Signal -> IO ()
waitForSignal s = do
  waitProcess <- spawnCommand "sleep infinity"
  installHandler s (CatchOnce $ terminateProcess waitProcess) Nothing
  void $ waitForProcess waitProcess

