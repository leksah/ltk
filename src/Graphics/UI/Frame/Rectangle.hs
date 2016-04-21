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
#if MIN_VERSION_gi_gdk(0,3,18)
import GI.Gdk (Rectangle(..), rectangleWidth, rectangleHeight, rectangleX, rectangleY, rectangleReadWidth, rectangleReadHeight, rectangleReadX, rectangleReadY)
#else
import GI.Cairo (RectangleInt(..), rectangleIntReadWidth, rectangleIntReadHeight, rectangleIntReadX, rectangleIntReadY)
#endif

#if !MIN_VERSION_gi_gdk(0,3,18)
type Rectangle      = RectangleInt
rectangleWidth      = rectangleIntWidth
rectangleHeight     = rectangleIntHeight
rectangleX          = rectangleIntX
rectangleY          = rectangleIntY
rectangleReadWidth  = rectangleIntReadWidth
rectangleReadHeight = rectangleIntReadHeight
rectangleReadX      = rectangleIntReadX
rectangleReadY      = rectangleIntReadY
newRectangle :: MonadIO m => [AttrOp Rectangle AttrSet] -> m Rectangle
newRectangle = new RectangleInt
#else
newRectangle :: MonadIO m => [AttrOp Rectangle AttrSet] -> m Rectangle
newRectangle = new Rectangle
#endif
