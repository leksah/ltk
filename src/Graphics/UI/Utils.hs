{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Graphics.UI.Utils (
    setPrimaryAlign
  , setSecondaryAlign
  , setPrimaryExpand
  , setSecondaryExpand
  , fontDescriptionToCssProps
) where

import Prelude ()
import Prelude.Compat
import Data.Text (Text)
import qualified Data.Text as T (pack)
import Control.Monad.IO.Class (MonadIO)
import GI.Gtk
       (widgetSetVexpand, widgetSetHexpand, widgetSetValign,
        widgetSetHalign, orientableGetOrientation, Align, IsWidget,
        IsOrientable)
import GI.Gtk.Enums (Orientation(..))
import GI.Pango
       (FontDescription, Variant(..), Style(..), FontMask(..),
        fontDescriptionGetSetFields, fontDescriptionGetFamily,
        fontDescriptionGetStyle, fontDescriptionGetVariant,
        fontDescriptionGetWeight, fontDescriptionGetSize,
        pattern SCALE)

setPrimaryAlign :: (MonadIO m, IsOrientable parent, IsWidget child) => parent -> child -> Align -> m ()
setPrimaryAlign parent child align =
    orientableGetOrientation parent >>= \case
        OrientationHorizontal -> widgetSetHalign child align
        OrientationVertical   -> widgetSetValign child align
        _ -> error "Invalid Orientation"

setPrimaryExpand :: (MonadIO m, IsOrientable parent, IsWidget child) => parent -> child -> Bool -> m ()
setPrimaryExpand parent child expand =
    orientableGetOrientation parent >>= \case
        OrientationHorizontal -> widgetSetHexpand child expand
        OrientationVertical   -> widgetSetVexpand child expand
        _ -> error "Invalid Orientation"

setSecondaryAlign :: (MonadIO m, IsOrientable parent, IsWidget child) => parent -> child -> Align -> m ()
setSecondaryAlign parent child align =
    orientableGetOrientation parent >>= \case
        OrientationHorizontal -> widgetSetValign child align
        OrientationVertical   -> widgetSetHalign child align
        _ -> error "Invalid Orientation"

setSecondaryExpand :: (MonadIO m, IsOrientable parent, IsWidget child) => parent -> child -> Bool -> m ()
setSecondaryExpand parent child expand =
    orientableGetOrientation parent >>= \case
        OrientationHorizontal -> widgetSetVexpand child expand
        OrientationVertical   -> widgetSetHexpand child expand
        _ -> error "Invalid Orientation"

fontDescriptionToCssProps :: MonadIO m => FontDescription -> m Text
fontDescriptionToCssProps fd = do
  mask <- fontDescriptionGetSetFields fd
  let prop fm n | fm `elem` mask = (maybe "" (\f -> n <> ": " <> f <> "; ") <$>)
                | otherwise      = const $ return ""
      weight w = T.pack . show $ (fromEnum w + 50) `div` 100 * 100
      style = \case
          StyleNormal  -> Just "normal"
          StyleItalic  -> Just "italic"
          StyleOblique -> Just "obligue"
          _ -> Nothing
      variant = \case
          VariantNormal    -> Just "normal"
          VariantSmallCaps -> Just "small-caps"
          _ -> Nothing
  fontFamily  <- prop FontMaskFamily  "font-family"  $ fontDescriptionGetFamily fd
  fontStyle   <- prop FontMaskStyle   "font-style"   $ style <$> fontDescriptionGetStyle fd
  fontVariant <- prop FontMaskVariant "font-variant" $ variant <$> fontDescriptionGetVariant fd
  fontWeight  <- prop FontMaskWeight  "font-weight"  $ Just . weight <$> fontDescriptionGetWeight fd
  fontSize    <- prop FontMaskSize    "font-size"    $ Just . T.pack . show
    .   (/ (fromIntegral SCALE :: Double))
    .   fromIntegral
    <$> fontDescriptionGetSize fd
  return $ mconcat [fontFamily, fontStyle, fontVariant, fontWeight, fontSize]
