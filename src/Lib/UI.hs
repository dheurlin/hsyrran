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
                                                , Button(..)
                                                , Box(..)
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


data State = State { contents :: Text
                   , yOffset  :: Int
                   }

data Event = Closed | Hidden | UnHidden | ShowText Text

view' :: State -> AppView Window Event
view' s =
  bin
      Window
      [ #title          := "HSyrran"
      , #typeHint       := Gdk.WindowTypeHintNotification
      , on #deleteEvent (const (False, Closed))
      ] $ widget Label [#label := contents s]


update' :: State -> Event -> Transition State Event
update' _ Closed         = Exit
update' s (ShowText txt) = Transition s { contents = txt } (pure Nothing)
update' s Hidden =
  Transition s $ Nothing <$ (mapM_ Gtk.widgetHide =<< Gtk.windowListToplevels)
update' s UnHidden =
  Transition s $ do
    moveWindowsToTaskBar (yOffset s)
    mapM_ Gtk.widgetShow =<< Gtk.windowListToplevels
    pure Nothing


app txtSource yOffset =
  App { view         = view'
      , update       = update'
      , inputs       = [toggleHidden, for txtSource (yield . ShowText . pack)]
      , initialState = State "" yOffset
      }
  where
    toggleHidden :: Producer Event IO ()
    toggleHidden = do
      yield Hidden
      lift $ waitForSignal sigUSR1
      yield UnHidden
      lift $ waitForSignal sigUSR1

      toggleHidden


showUI :: Producer String IO () -> Int -> IO ()
showUI txtSource yOffset = do
  void $ Gtk.init Nothing

  void . async $ do
    void $ runLoop (app txtSource yOffset)
    -- Hack to force the window to close
    mapM Gtk.windowClose =<< windowListToplevels
    Gtk.mainQuit
  --
  moveWindowsToTaskBar yOffset
  Gtk.main

  -- Move the window to sit below taskbar
moveWindowsToTaskBar :: Int -> IO ()
moveWindowsToTaskBar yOffset = do
  (x, _) <- getMouseCoords
  void $ async $ moveWindows (fromIntegral x - 50) (fromIntegral yOffset)

  -- Move the window to coordinates after waiting for them to spawn, HACK
moveWindows :: Int32 -> Int32 -> IO ()
moveWindows x y = windowListToplevels >>= \case
                     [] -> threadDelay 1000 >> moveWindows x y
                     xs -> mapM_ (\w -> Gtk.windowMove w x y) xs
