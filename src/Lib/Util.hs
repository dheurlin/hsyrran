module Lib.Util where

import System.Process
import System.Posix.Signals
import Control.Monad (void, unless)
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
import Data.IORef

(.!!.) :: [a] -> Int -> Maybe a
xs .!!. i
  | i >= 0 && i < length xs = Just $ xs !! i
  | otherwise               = Nothing

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

loop :: Monad m => m () -> m ()
loop m = m >> (loop m)

notify :: String -> IO ()
notify s = void $ system $ "notify-send \"" <> s <> "\""

getMouseCoords :: IO (Int, Int)
getMouseCoords = do
  [xStr, yStr] <- words <$> readCreateProcess (shell cmdStr) ""
  pure (read xStr, read yStr)
  where
    cmdStr = "eval $(xdotool getmouselocation --shell); echo $X $Y"

waitForSignal :: Signal -> IO ()
waitForSignal s = do
  done <- newIORef False
  installHandler s (CatchOnce $ writeIORef done True) Nothing
  wait done
  where
    wait done = do
      d <- readIORef done
      unless d $ threadDelay 1000 >> wait done
