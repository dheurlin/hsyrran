{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.UI where

import           Data.Function                  ( (&) )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Pipes
import qualified Pipes.Extras                  as Pipes
import           Control.Monad                  ( void )

import           GI.Gtk                         ( Label(..)
                                                , Window(..)
                                                , WindowPosition(..)
                                                )

import           GI.Gdk.Enums                   ( WindowTypeHint(..) )

import           System.Exit                    ( die )

import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

data State = Initial | Close

data Event = Closed

view' :: Text -> State -> AppView Window Event
view' txt s =
  bin
      Window
      [ #title          := "Hello"
      , #windowPosition := WindowPositionMouse
      -- , #typeHint       := WindowTypeHintNotification
      , on #deleteEvent (const (False, Closed))
      ] $ widget Label [#label := txt]

update' :: State -> Event -> Transition State Event
update' _ Closed = Exit

hello :: String -> IO ()
hello contents = void $ run $ helloApp contents

helloApp c = App { view = view' (pack c)
                 , update       = update'
                 , inputs       = []
                 , initialState = Initial
                 }

