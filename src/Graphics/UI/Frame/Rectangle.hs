{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
--
-- Module      :  IDE.Core.ViewFrame
-- Copyright   :  (c) Juergen Nicklisch-Franken, Hamish Mackenzie
-- License     :  GNU-GPL
--
-- Maintainer  :  <maintainer at leksah.org>
-- Stability   :  provisional
-- Portability :  portable
--
--
-- | Portable Rectangle type that works with older Gdk versions
--
---------------------------------------------------------------------------------

module Graphics.UI.Frame.Rectangle (
    Rectangle(..)
,   newRectangle
,   rectangleWidth
,   rectangleHeight
,   rectangleX
,   rectangleY
,   rectangleReadWidth
,   rectangleReadHeight
,   rectangleReadX
,   rectangleReadY
) where

import Data.GI.Base (new, AttrOp)
import Data.GI.Base.Attributes (AttrOpTag(..))
import Control.Monad.IO.Class (MonadIO)
#ifdef MIN_VERSION_GDK_3_18
import GI.Gdk (Rectangle(..), rectangleWidth, rectangleHeight, rectangleX, rectangleY, rectangleReadWidth, rectangleReadHeight, rectangleReadX, rectangleReadY)
#else
import Data.Int (Int32)
import GI.Cairo (RectangleInt(..), rectangleIntWidth, rectangleIntHeight, rectangleIntX, rectangleIntY, rectangleIntReadWidth, rectangleIntReadHeight, rectangleIntReadX, rectangleIntReadY)
#endif

#ifdef MIN_VERSION_GDK_3_18
newRectangle :: MonadIO m => [AttrOp Rectangle AttrSet] -> m Rectangle
newRectangle = new Rectangle
#else
type Rectangle      = RectangleInt
rectangleWidth      = rectangleIntWidth
rectangleHeight     = rectangleIntHeight
rectangleX          = rectangleIntX
rectangleY          = rectangleIntY
rectangleReadWidth :: MonadIO m => Rectangle -> m Int32
rectangleReadWidth  = rectangleIntReadWidth
rectangleReadHeight :: MonadIO m => Rectangle -> m Int32
rectangleReadHeight = rectangleIntReadHeight
rectangleReadX :: MonadIO m => Rectangle -> m Int32
rectangleReadX      = rectangleIntReadX
rectangleReadY :: MonadIO m => Rectangle -> m Int32
rectangleReadY      = rectangleIntReadY
newRectangle :: MonadIO m => [AttrOp Rectangle AttrSet] -> m Rectangle
newRectangle = new RectangleInt
#endif
