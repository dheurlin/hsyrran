{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.UI where

import           Data.Function                  ( (&) )
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

import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

import           System.Posix.Signals

import           Lib.Util

data State = Initial | Greeting Text

data Event = Closed | Greet Text

view' :: Text -> State -> AppView Window Event
view' txt s =
  bin
      Window
      [ #title          := "Hello"
      , #windowPosition := Gtk.WindowPositionMouse
      -- , #typeHint       := WindowTypeHintNotification
      , on #deleteEvent (const (False, Closed))
      , classes ["window"]
      ] $ case s of
            Initial    -> widget Label [#label := txt]
            Greeting t -> widget Label [#label := t]

-- view' :: Text -> State -> AppView Window Event
-- view' txt s =
--   bin
--       Window
--       [ #title          := "Hello"
--       , #windowPosition := Gtk.WindowPositionMouse
--       -- , #typeHint       := WindowTypeHintNotification
--       , on #deleteEvent (const (False, Closed))
--       , classes ["window"]
--       ] $ case s of
--             Initial    -> widget Label [#label := txt]
--             Greeting t -> widget Label [#label := t]

update' :: State -> Event -> Transition State Event
update' _ Closed     = Exit
update' _ (Greet t)  = Transition (Greeting t) (return Nothing)

helloApp c = App { view = view' (pack c)
                 , update       = update'
                 , inputs       = [waitSigUsr]
                 , initialState = Initial
                 }
  where
    waitSigUsr :: Producer Event IO ()
    waitSigUsr = do
      lift $ waitForSignal sigUSR1
      lift $ putStrLn "closing..."
      yield (Greet "ocll") >-> Pipes.delay 1.0
      yield (Greet "closing") >-> Pipes.delay 1.0
      yield $ Closed


hello :: String -> IO ()
hello contents = void $ run (helloApp contents)

---------------- ATTEMPTS AT KEYBINDINGS ---------------------------------------

-- hello :: String -> IO ()
-- hello contents = do
--   void $ Gtk.init Nothing

--   -- Set up screen and CSS provider
--   screen <- maybe (fail "No screen?!") return =<< Gdk.screenGetDefault
--   p      <- Gtk.cssProviderNew
--   Gtk.cssProviderLoadFromData p styles
--   Gtk.styleContextAddProviderForScreen
--     screen
--     p
--     (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)

--   -- Start main loop
--   void . async $ do
--     void $ runLoop (helloApp contents)
--     Gtk.mainQuit
--   Gtk.main


-- bindings :: Gtk.BindingSet
-- bindings = Gtk.BindingSet
--            [ #entries := [ Gtk.BindingEntry
--                             [ #keyval  := Gdk.KEY_Q
--                             , #signals := [ Gtk.BindingSignal
--                                             [ #signalName := "delete-event" ]
--                                           ]
--                             ]
--                          ]
--            ]

-- styles :: ByteString
-- styles = mconcat
--   [ "@binding-set CloseWindow"
--   , "{"
--   , "  bind \"q\" { \"delete-event\" (main-quit) };"
--   , "}"

--   , "window"
--   , "{"
--   , "  -gtk-key-bindings: CloseWindow;"
--   , "}"
--   ]
