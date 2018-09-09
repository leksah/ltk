{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall #-}
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

import Prelude ()
import Prelude.Compat
import Data.Int (Int32)
import Data.GI.Base (new)
import Control.Monad.IO.Class (MonadIO)
import GI.Gdk (Rectangle(..), getRectangleWidth, getRectangleHeight, getRectangleX, getRectangleY,
                              setRectangleWidth, setRectangleHeight, setRectangleX, setRectangleY)

newRectangle :: MonadIO m => Int32 -> Int32 -> Int32 -> Int32 -> m Rectangle
newRectangle x y width height = do
    r <- new Rectangle []
    setRectangleX      r x
    setRectangleY      r y
    setRectangleWidth  r width
    setRectangleHeight r height
    return r
