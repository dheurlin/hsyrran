module Lib.UIHelpers where

import           GI.Gtk                        ( Window(..) )
import           Data.GI.Base.ShortPrelude     ( Ptr )
import           Data.GI.Base.BasicTypes       ( GList )
import           Data.GI.Base.BasicConversions ( unpackGList )
import           Data.GI.Base.ManagedPtr       ( newObject )

foreign import ccall unsafe "gtk_window_list_toplevels"
  gtk_window_list_toplevels :: (IO (Ptr (GList (Ptr Window))))

windowListToplevels :: IO [Window]
windowListToplevels = do
  glistPtr <- gtk_window_list_toplevels
  winPtrs <- unpackGList glistPtr
  mapM (\ptr -> (newObject Window) ptr) winPtrs


