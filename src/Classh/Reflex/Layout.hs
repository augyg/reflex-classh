{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Classh.Reflex.Layout
Description : Grid-based layout primitives for Reflex with ClasshSS
Copyright   : (c) 2024, Galen Sprout
License     : BSD-style (see end of this file)
Maintainer  : Galen Sprout <galen.sprout@gmail.com>

A collection of functions to quickly divide up a Dom into rows and columns.

Note that these functions are really just meant to be reduced Boxes that visually instruct
other developers what the UI is doing.

For example, lets use grids to show 3 components (just text here) that on mobile and
tablet(sm) will take up the full row, and at larger sizes take up 3\/12 cols, 4\/12 cols,
and 5\/12 cols respectively.

Let's also pretend this is a landing page and we want all rows evenly spaced, we can make
this obvious with 'row'

@
  row [y .~~ TWSize 10] $ do
    gridCol Col12 $ do
      col [12,12,3] $ text "hey"
      col [12,12,4] $ text "hello"
      col [12,12,5] $ text "howdy"
@
-}
module Classh.Reflex.Layout (
    -- * Type aliases
    Mutation,
    MinWidth,
    MaxWidth,
    TargetWidth,

    -- * Centering constants

    -- | Pre-built mutation lists for full-size centered containers
    centeredFullH,
    centeredFullW,
    centeredFull,

    -- * Inline display

    -- | Force inline-block display for elements
    inline,
    inlines,

    -- * Layering

    -- | Stack two elements on top of each other with z-index ordering
    overtopOf,

    -- * Rows

    -- | Divide vertical space with consistent padding
    row,
    rowW,

    -- * Grid containers

    -- | Create CSS grid containers with a fixed or responsive column count
    gridCol,
    gridCol',
    gridColW,
    gridColWhen,

    -- * Column spans

    -- | Place children within a grid by specifying responsive column spans
    col,
    colFrom,
    colDyn,
    col',
    responsiveCol,
    responsiveRowCol,

    -- * Rotation

    -- | Control how two elements reorder when forced into a single column on smaller screens
    Rotation (..),
    rotatingBox,
) where

import Classh
import Reflex.Dom.Core as Dom

import Classh.Reflex.Place (centerSimple)
import Control.Monad (void, when)
import qualified Data.Text as T

type Mutation a = a -> a

type MinWidth = DimensionConstraint
type MaxWidth = DimensionConstraint
type TargetWidth = TWSizeOrFraction

-- | Centering relative to full sized containers, as constants
centeredFullH :: [Mutation BoxConfig]
centeredFullH = [h .~~ pct 100, pos .~~ centered]

-- | Centering relative to full sized containers, as constants
centeredFullW :: [Mutation BoxConfig]
centeredFullW = [w .~~ pct 100, pos .~~ centered]

-- | Centering relative to full sized containers, as constants
centeredFull :: [Mutation BoxConfig]
centeredFull = [h .~~ pct 100, w .~~ pct 100, pos .~~ centered]

-- | Wrap content in an inline-block div
inline :: (DomBuilder t m) => m a -> m a
inline ma = divClass (classhUnsafe [box_custom .~ "inline-block"]) ma

-- | Render a list of monadic actions each wrapped in an inline-block div
inlines :: (DomBuilder t m) => [m a] -> m ()
inlines [] = pure ()
inlines (ma : mas) = void (inline ma) >> inlines mas

{- | Call as infix: @topDOM \`overtopOf\` bottomDOM@.
Stacks two elements at the same responsive heights using absolute positioning and z-index.
-}
overtopOf :: (DomBuilder t m) => [TWSizeOrFraction] -> m () -> m () -> m ()
overtopOf heights topDOM bottomDOM = do
    when (null heights) $ do
        error "`overtopOf` received null list"
    el "div" $ do
        divClass shell_relative $ do
            divClass shell_top $ do
                topDOM
            divClass shell_bottom $ do
                bottomDOM
  where
    shell_relative = classhUnsafe [custom .~ "relative", h .|~ heights]
    shell_top = classhUnsafe [custom .~ "absolute z-30", w .~~ TWSize_Full, h .|~ heights]
    shell_bottom = classhUnsafe [custom .~ "absolute z-20", w .~~ TWSize_Full, h .|~ heights]

-- | A generic interface to creating a centered row of some width+width constraints
rowW :: (DomBuilder t m) => [(MinWidth, TargetWidth, MaxWidth)] -> (Justify, Align) -> m a -> m a
rowW widthCfg pos_ = centerSimple . divClass (classhUnsafe [minW .|~ minWidths, maxW .|~ maxWidths, w .|~ targetWidths, pos .~~ pos_])
  where
    (minWidths, targetWidths, maxWidths) = unzip3 widthCfg

{- | A handy way to visually (to the dev and to the user) divide up a section vertically

> row [ y .~~ TWSize 10, l .~~ pix 5 ] $ text "hey"
-}
row :: (DomBuilder t m) => [Mutation BoxPadding] -> m a -> m a
row paddingF = elClass "div" ("" <&> showTW (applyFs def paddingF))

{- | Divide a row into a number of columns.
Purposefully designed to take no other data besides 'ColInt' in order to be very self-contained.

> gridCol Col12 $ do { col [6] $ text "hey"; col [6] $ text "hi" }
-}
gridCol :: (DomBuilder t m) => ColInt -> m a -> m a
gridCol cInt ma = elClass "div" ("grid grid-cols-" <> showTW cInt) ma

{- | Divide a row into a number of columns, with other arbitrary classes

> gridCol' Col12 "bg-black" $ do { col [6] $ text "hey"; col [6] $ text "hi" }
-}
gridCol' :: (DomBuilder t m) => ColInt -> T.Text -> m a -> m a
gridCol' cInt customCs ma = elClass "div" ("grid grid-cols-" <> showTW cInt <&> customCs) ma

{- | Divide a row into a number of columns, and take up a fraction of parent row

> gridColW Col12 (pct 80) $ do { col [6] $ text "hey"; col [6] $ text "hi" }
-}
gridColW :: (DomBuilder t m) => ColInt -> TWSizeOrFraction -> m a -> m a
gridColW cInt width_ ma = elClass "div" ("grid grid-cols-" <> showTW cInt <&> ("w-" <> showTW width_)) ma

{- | Divide a row into a number of columns where columns is based on screen size.
Purposefully designed to take no other data besides 'ColInt' in order to be very self-contained.

> gridColWhen [("def", Col6), ("md:", Col12)] $ do { col [6] $ text "hey"; col [6] $ text "hi" }
-}
gridColWhen :: (DomBuilder t m) => WhenTW ColInt -> m a -> m a
gridColWhen cInts' ma = elClass "div" (showCInts cInts') ma
  where
    showCInts [] = ""
    showCInts (("def", cInt) : cInts) = "grid grid-cols-" <> showTW cInt <&> showCInts cInts
    showCInts ((case_, cInt) : cInts) =
        case_
            <> ":"
            <> "grid" <&> case_
            <> ":"
            <> "grid-cols-"
            <> showTW cInt
                <&> showCInts cInts

{- | Takes a list of values for how many columns should be taken up at a given screen size
using 'zipScreens'
-}
col :: (DomBuilder t m) => [Int] -> m a -> m a
col = responsiveRowCol

{- | Takes a list of values for how many columns should be taken up at a given screen size
using 'zipScreens', as well as where this should start (if you want to skip columns)

> do { colFrom [(2,2)] $ text "hey" ; colFrom [(9,2)] $ text "hi" }
-}
colFrom :: (DomBuilder t m) => [(Int, Int)] -> m a -> m a
colFrom colsCfg ma =
    let
        (colStarts, colSpans) = unzip colsCfg
        spandex = renderWhenTW (zipScreens colSpans) ((<>) "col-span-" . tshow)
        startdex = renderWhenTW (zipScreens colStarts) ((<>) "col-start-" . tshow)
     in
        elClass "div" (spandex <&> startdex) ma

{- | Like 'col' except that the list of cols to take up is Dynamic. An example use is
an element that expands and contracts based on a click and that we also want to be responsive
to screen sizes, like a side navbar.
-}
colDyn ::
    forall a t m.
    (PostBuild t m, DomBuilder t m) =>
    Dynamic t [Int] ->
    m a ->
    m a
colDyn dynCols ma =
    let
        spandex = ffor dynCols $ \colSpans ->
            renderWhenTWRank2
                (zipScreens colSpans)
                ( \condition c ->
                    if c == 0
                        then twWhenText condition ("col-span-" <> tshow c) <&> twWhenText condition "hidden"
                        else twWhenText condition ("col-span-" <> tshow c) <&> twWhenText condition "block"
                )
     in
        elDynClass "div" spandex ma

{- | Like 'col' except that its height is 100% of container.

If we look at Width and height of CSS Boxes as Categories then there are 2 main ones on either end:

1. Assume Height == HeightOfContents
2. Assume Height == HeightOfContainer

and this is true for width as well, but this is handled by the argument which is logical since
responsiveness has a far bigger bearing on width.
-}
col' :: (DomBuilder t m) => [Int] -> m a -> m a
col' colSpans ma =
    let
        spandex = renderWhenTW (zipScreens colSpans) ((<>) "col-span-" . tshow)
        heightCol = $(classh' [h .~~ TWSize_Full])
     in
        elClass "div" (heightCol <&> spandex) ma

{- | Takes a list of values for how many columns should be taken up at a given screen size
using 'zipScreens'
-}
responsiveCol :: (DomBuilder t m) => [Int] -> m a -> m a
responsiveCol = responsiveRowCol

{- | Takes a list of values for how many columns should be taken up at a given screen size
using 'zipScreens'
-}
responsiveRowCol :: (DomBuilder t m) => [Int] -> m a -> m a
responsiveRowCol colSpans ma =
    let spandex = renderWhenTW (zipScreens colSpans) ((<>) "col-span-" . tshow)
     in elClass "div" spandex ma

{- | A type to model how two elements should rotate
as the screen size gets smaller and they are forced into one column.
ie. should the element on the right be on top or bottom?
-}
data Rotation a b = CounterClockwise a b | Clockwise a b

{- | An interface to control how two elements should rotate
as the screen size gets smaller and they are forced into one column.
ie. should the element on the right be on top or bottom?
-}
rotatingBox :: (DomBuilder t m) => TWSizeOrFraction -> Rotation (m ()) (m ()) -> m ()
rotatingBox widthA rotation = do
    case rotation of
        CounterClockwise domA domB -> do
            nonMobile widthA domA domB
            flip mobile domA domB
        Clockwise domA domB -> do
            nonMobile widthA domA domB
            mobile domA domB
  where
    boxMobileOnly = col [12] . divClass "block md:hidden w-[100%]"
    boxNonMobileLeft width_ = col [7] . divClass (classhUnsafe [w .~~ width_, custom .~ "hidden md:block"])
    boxNonMobileRight = col [5] . divClass "hidden md:block"

    nonMobile width_ domX domY = do
        void $ boxNonMobileLeft width_ $ do
            domX
        boxNonMobileRight $ do
            domY
    mobile domX domY = do
        void $ boxMobileOnly domX
        void $ boxMobileOnly domY
