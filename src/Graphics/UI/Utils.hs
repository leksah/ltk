{-# LANGUAGE LambdaCase #-}
module Graphics.UI.Utils (
    setPrimaryAlign
  , setSecondaryAlign
  , setPrimaryExpand
  , setSecondaryExpand
) where

import Control.Monad.IO.Class (MonadIO)
import GI.Gtk
       (widgetSetVexpand, widgetSetHexpand, widgetSetValign,
        widgetSetHalign, orientableGetOrientation, Align, IsWidget,
        IsOrientable)
import GI.Gtk.Enums (Orientation(..))

setPrimaryAlign :: (MonadIO m, IsOrientable parent, IsWidget child) => parent -> child -> Align -> m ()
setPrimaryAlign parent child align =
    orientableGetOrientation parent >>= \case
        OrientationHorizontal -> widgetSetHalign child align
        OrientationVertical   -> widgetSetValign child align

setPrimaryExpand :: (MonadIO m, IsOrientable parent, IsWidget child) => parent -> child -> Bool -> m ()
setPrimaryExpand parent child expand =
    orientableGetOrientation parent >>= \case
        OrientationHorizontal -> widgetSetHexpand child expand
        OrientationVertical   -> widgetSetVexpand child expand

setSecondaryAlign :: (MonadIO m, IsOrientable parent, IsWidget child) => parent -> child -> Align -> m ()
setSecondaryAlign parent child align =
    orientableGetOrientation parent >>= \case
        OrientationHorizontal -> widgetSetValign child align
        OrientationVertical   -> widgetSetHalign child align

setSecondaryExpand :: (MonadIO m, IsOrientable parent, IsWidget child) => parent -> child -> Bool -> m ()
setSecondaryExpand parent child expand =
    orientableGetOrientation parent >>= \case
        OrientationHorizontal -> widgetSetVexpand child expand
        OrientationVertical   -> widgetSetHexpand child expand

