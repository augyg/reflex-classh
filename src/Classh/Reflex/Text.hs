{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Classh.Reflex.Text
Description : Styled text rendering with ClasshSS for Reflex
Copyright   : (c) 2024, Galen Sprout
License     : BSD-style (see end of this file)
Maintainer  : Galen Sprout <galen.sprout@gmail.com>

A collection of interfaces to handle text from a single word to multiple paragraphs,
with ClasshSS compile-time-checked styling.
-}
module Classh.Reflex.Text (
    -- * Static text with compiled styles

    -- | Wrap text in a styled span element
    textS,
    textU,

    -- * Dynamic text variants

    -- | Text or style classes may change in response to user events
    textDynS,
    dynTextS,
    dynTextDynS,

    -- * Multi-span text

    -- | Compose multiple styled text spans into a single line
    intercalate,

    -- * Paragraphs

    -- | Display multiple paragraphs with consistent or variable spacing
    paragraphs,
    paragraphs',

    -- * Text positioning

    -- | Apply text-position classes to a container div
    textPosition,
) where

import Classh
import Classh.Reflex.Layout (row)
import Reflex.Dom.Core as Dom

import qualified Data.Text as T

-- | Text wrapped in a span to ensure classes work properly regardless of context
textS :: (DomBuilder t m) => CompiledS -> T.Text -> m ()
textS s txt = elClass "span" s $ text txt

-- | Text wrapped in a @\<u\>@ tag to ensure classes work properly regardless of context
textU :: (DomBuilder t m) => CompiledS -> T.Text -> m ()
textU s txt = elClass "u" s $ text txt

-- | Like 'textS' except that the classes are Dynamic so they may change in response to user events
textDynS ::
    (PostBuild t m, DomBuilder t m) =>
    Dynamic t CompiledS ->
    T.Text ->
    m ()
textDynS s txt = elDynClass "span" s $ text txt

{- | Like 'textS' except that the Text is Dynamic so it may change in response to user events.
This is the classhified version of 'dynText'.
-}
dynTextS ::
    (PostBuild t m, DomBuilder t m) =>
    CompiledS ->
    Dynamic t T.Text ->
    m ()
dynTextS s txt = elClass "span" s $ dynText txt

-- | Like 'textS' except that both the Text and class attribute are Dynamic
dynTextDynS ::
    (PostBuild t m, DomBuilder t m) =>
    Dynamic t CompiledS ->
    Dynamic t T.Text ->
    m ()
dynTextDynS s txt = elDynClass "span" s $ dynText txt

{- | For creating generic templates which dont deeply depend on the text or text config given, or allow
for multiple 'TextConfigTW's across a single text.
A simple example is a sentence with highlighted or different color words for emphasis.
The one caveat is you do need to manually set the TextSize.

> intercalate (only XL2) " " [ textS $(classh' [text_size .~~ XL2, text_color .~~ Black]) "Hey"
>                             , dynTextS $(classh' [text_size .~~ XL2, text_color .~~ White]) user
>                             ]
-}
intercalate ::
    (DomBuilder t m) =>
    WhenTW TextSize ->
    T.Text ->
    [m ()] ->
    m ()
intercalate _ _ [] = pure ()
intercalate textSize inter (txt' : txts') = do
    txt' >> prependAll textSize inter txts'
  where
    prependAll _ _ [] = pure ()
    prependAll s i (txt : txts) = elClass "span" (renderWhenTW s showTW) (text i) >> txt >> prependAll s i txts

{- | Display a number of paragraphs (where a paragraph is 1 or more sentences, with whatever crazy styling,
like 'intercalate') with even spacing between them, defined by the first arg of TWSize.
Does not add padding for the first element.
-}
paragraphs ::
    (DomBuilder t m) =>
    WhenTW (WithTransition TWSize) ->
    [m ()] ->
    m ()
paragraphs _ [] = pure ()
paragraphs spacing (para : paras) = do
    row [] $ para
    withTopPadding spacing paras
  where
    withTopPadding _ [] = pure ()
    withTopPadding p_ (r_ : rs_) = row [t .~ p_] r_ >> withTopPadding p_ rs_

{- | Display a number of paragraphs with variable spacing between them,
defined by the first element of the tuple, of type TWSize.
-}
paragraphs' ::
    (DomBuilder t m) =>
    [(WhenTW (WithTransition TWSize), m ())] ->
    m ()
paragraphs' [] = pure ()
paragraphs' ((topPadding, para) : rows) = row [t .~ topPadding] para >> paragraphs' rows

-- | Apply text-position classes to a container div
textPosition :: (DomBuilder t m) => CompiledS -> m a -> m a
textPosition = divClass
