{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Classh.Reflex
Description : ClasshSS-typed UI component library for Reflex
Copyright   : (c) 2024, Galen Sprout
License     : BSD-style (see end of this file)
Maintainer  : Galen Sprout <galen.sprout@gmail.com>

A collection of patterns that have emerged from usage of ClasshSS in order to reduce the amount of
code necessary to build beautiful UIs that will be easier to maintain.

For example, lets use grids to show 3 components (just text here) that on mobile and
tablet(sm) will take up the full row, and at larger sizes take up 3\/12 width, 4\/12 width, and 5\/12 width
respectively.

Let's also pretend this is a landing page and we want all rows evenly spaced, we can make this obvious
with 'row'

@
  {\-\# LANGUAGE OverloadedStrings \#-\}
  {\-\# LANGUAGE FlexibleContexts \#-\}
  module Main where

  import Reflex.Dom.Core
  import Classh

  mySimpleResponsivePage :: DomBuilder t m => m ()
  mySimpleResponsivePage = do
    row [y .~~ TWSize 10] $ do
      gridCol Col12 $ do
        col [12,12,3] $ normalText "hey"
        col [12,12,4] $ normalText "hello"
        col [12,12,5] $ normalText "howdy"
    where
       normalText = textS $(classh' [text_size .|~ [LG, XL, XL2, XL3] ] )
@
-}
module Classh.Reflex (
    -- * Re-exported modules

    -- ** Element builders

    -- | Typed @elClass@ variants, responsive images, TH element builders
    module El,

    -- ** Text rendering

    -- | Styled text spans, dynamic text, paragraphs with spacing
    module CRText,

    -- ** Placement and centering

    -- | Centering utilities, responsive padded regions
    module Place,

    -- ** Grid layout

    -- | Rows, columns, grid containers, rotating boxes
    module Layout,
) where

import Classh.Reflex.El as El
import Classh.Reflex.Layout as Layout
import Classh.Reflex.Place as Place
import Classh.Reflex.Text as CRText
