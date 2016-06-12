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
,   setRectangleWidth
,   setRectangleHeight
,   setRectangleX
,   setRectangleY
,   getRectangleWidth
,   getRectangleHeight
,   getRectangleX
,   getRectangleY
) where

import Data.Int (Int32)
import Data.GI.Base (new, AttrOp)
import Data.GI.Base.Attributes (AttrOpTag(..))
import Control.Monad.IO.Class (MonadIO)
#ifdef MIN_VERSION_GDK_3_18
import GI.Gdk (Rectangle(..), getRectangleWidth, getRectangleHeight, getRectangleX, getRectangleY,
                              setRectangleWidth, setRectangleHeight, setRectangleX, setRectangleY)
#else
import GI.Cairo (RectangleInt(..), getRectangleIntWidth, getRectangleIntHeight, getRectangleIntX, getRectangleIntY,
                                   setRectangleIntWidth, setRectangleIntHeight, setRectangleIntX, setRectangleIntY)
#endif

#ifdef MIN_VERSION_GDK_3_18
rectangle = Rectangle
#else
type Rectangle      = RectangleInt
rectangle           = RectangleInt
getRectangleWidth :: Rectangle -> IO Int32
getRectangleWidth  = getRectangleIntWidth
getRectangleHeight :: Rectangle -> IO Int32
getRectangleHeight = getRectangleIntHeight
getRectangleX :: Rectangle -> IO Int32
getRectangleX      = getRectangleIntX
getRectangleY :: Rectangle -> IO Int32
getRectangleY      = getRectangleIntY
setRectangleWidth :: Rectangle -> Int32 -> IO ()
setRectangleWidth  = setRectangleIntWidth
setRectangleHeight :: Rectangle -> Int32 -> IO ()
setRectangleHeight = setRectangleIntHeight
setRectangleX :: Rectangle -> Int32 -> IO ()
setRectangleX      = setRectangleIntX
setRectangleY :: Rectangle -> Int32 -> IO ()
setRectangleY      = setRectangleIntY
#endif

newRectangle :: MonadIO m => Int32 -> Int32 -> Int32 -> Int32 -> m Rectangle
newRectangle x y width height = do
    r <- new rectangle []
    setRectangleX      r x
    setRectangleY      r y
    setRectangleWidth  r width
    setRectangleHeight r height
    return r    
