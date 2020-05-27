{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.UI where

import           Data.Function                  ( (&) )
import           Data.Functor                   ( ($>) )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.ByteString                ( ByteString )
import           Pipes
import qualified Pipes.Extras                  as Pipes
import           Control.Monad                  ( void )

import           GI.Gtk                         ( Label(..)
                                                , Window(..)
                                                )
import qualified GI.Gtk                        as Gtk

import qualified GI.Gdk                        as Gdk

import           System.Exit                    ( die )
import           System.Process

import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

import           System.Posix.Signals
import qualified Control.Concurrent            as CC

import           Lib.Util

type State = ()

data Event = Closed


view' :: Text -> State -> AppView Window Event
view' txt _ =
  bin
      Window
      [ #title          := "HSyrran"
      , #windowPosition := Gtk.WindowPositionMouse
      , #typeHint       := Gdk.WindowTypeHintNotification
      , on #deleteEvent (const (True, Closed))
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

-- An ugly hack to close the window even though the process is still running
forceCloseWindow :: IO ()
forceCloseWindow =
  void $ system "xdotool search --name \"^HSyrran$\" windowclose"

hello :: String -> IO ()
hello contents = do
  run (helloApp contents)
  forceCloseWindow



