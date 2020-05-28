{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE LambdaCase        #-}

module Lib.UI where

import           Data.Function                  ( (&) )
import           Data.Maybe
import           Data.Functor                   ( ($>) )
import           Data.Text                      ( Text
                                                , pack
                                                , unpack
                                                )
import           Data.ByteString                ( ByteString )
import           Pipes
import qualified Pipes.Extras                  as Pipes
import           Control.Monad

import           Data.GI.Base.ShortPrelude      ( Int32 )
import           GI.Gtk                         ( Label(..)
                                                , Window(..)
                                                )
import qualified GI.Gtk                        as Gtk
import qualified GI.Gdk                        as Gdk

import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

import           System.Posix.Signals
import           Control.Concurrent.Async      ( async )
import           Control.Concurrent            ( threadDelay )

import           Lib.Util
import           Lib.UIHelpers


type State = ()

data Event = Closed

view' :: Text -> State -> AppView Window Event
view' txt _ =
  bin
      Window
      [ #title          := "HSyrran"
      -- , #windowPosition := Gtk.WindowPositionMouse
      , #typeHint       := Gdk.WindowTypeHintNotification
      , on #deleteEvent (const (False, Closed))
      ] $ widget Label [#label := txt]


update' :: State -> Event -> Transition State Event
update' _ Closed        = Exit

app c = App { view = view' (pack c)
            , update       = update'
            , inputs       = [waitSigUsr]
            , initialState = ()
            }
  where
    waitSigUsr :: Producer Event IO ()
    waitSigUsr = do
      lift $ waitForSignal sigUSR1
      yield $ Closed

showUI :: Int -> String -> IO ()
showUI yOffset contents = do
  void $ Gtk.init Nothing

  void . async $ do
    void $ runLoop (app contents)
    -- Hack to force the window to close
    mapM Gtk.windowClose =<< windowListToplevels
    Gtk.mainQuit

  -- Move the window to sit below taskbar after spawning, HACK
  (x, _) <- getMouseCoords
  async $ moveWindows (fromIntegral x - 50) (fromIntegral yOffset)
  Gtk.main

  where
    moveWindows :: Int32 -> Int32 -> IO ()
    moveWindows x y = windowListToplevels >>= \case
                         [] -> threadDelay 1000 >> moveWindows x y
                         xs -> mapM_ (\w -> Gtk.windowMove w x y) xs
