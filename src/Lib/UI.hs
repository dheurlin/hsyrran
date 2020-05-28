{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

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

import           GI.Gtk                         ( Label(..)
                                                , Window(..)
                                                )
import qualified GI.Gtk                        as Gtk
import qualified GI.Gdk                        as Gdk

import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

import           System.Posix.Signals
import           Control.Concurrent.Async      ( async )

import           Lib.Util
import           Lib.UIHelpers


type State = ()

data Event = Closed

view' :: Text -> State -> AppView Window Event
view' txt _ =
  bin
      Window
      [ #title          := "HSyrran"
      , #windowPosition := Gtk.WindowPositionMouse
      , #typeHint       := Gdk.WindowTypeHintNotification
      , on #deleteEvent (const (False, Closed))
      ] $ widget Label [#label := txt]


update' :: State -> Event -> Transition State Event
update' _ Closed        = Exit

helloApp c = App { view = view' (pack c)
                 , update       = update'
                 , inputs       = [waitSigUsr]
                 , initialState = ()
                 }
  where
    waitSigUsr :: Producer Event IO ()
    waitSigUsr = do
      lift $ waitForSignal sigUSR1
      yield $ Closed

-- moveWindow :: (Int, Int) -> IO ()
-- moveWindow (x, y) = void $ spawnCommand $
--   "xdotool search --sync --name \"^HSyrran$\" windowmove " <> show x <> " " <> show y

hello :: String -> IO ()
hello  contents = do
  void $ Gtk.init Nothing

  -- (x, _) <- getMouseCoords
  -- moveWindow (x - 50, 32)

  void . async $ do
    void $ runLoop (helloApp contents)

    mapM Gtk.windowClose =<< windowListToplevels
    Gtk.mainQuit
  Gtk.main
