{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Classh.Reflex.El
Description : Classh-typed element builders for Reflex
Copyright   : (c) 2024, Galen Sprout
License     : BSD-style (see end of this file)
Maintainer  : Galen Sprout <galen.sprout@gmail.com>

Classh-typed versions of @elClass@ and similar functions, plus
Template Haskell builders that compile CSS at compile time via 'compileS'.
-}
module Classh.Reflex.El (
    -- * Type aliases
    Shell,
    PlaceHolder,
    TextAreaRows,
    StaticImagePath,
    Pixels,

    -- * Typed element builders

    -- | Variants of @elClass@ that accept a 'BoxConfig' instead of a raw class string
    elTW,
    elTW',
    elDynTW,
    elDynTW',

    -- * Convenience wrappers

    -- | Shorthand for common element patterns
    div_,
    in_,
    inH_,

    -- * Form elements

    -- | Classh-typed input and textarea builders
    inputEl,
    textAreaEl,

    -- * Responsive images

    -- | Build @\<img\>@ elements with srcset and sizes attributes
    imgResponsive,
    imgSrcSet,

    -- * Template Haskell element builders

    -- | Compile-time checked element and text builders using 'compileS'
    divClassh,
    divClassh',
    textClassh,
    textClassh',
    textPos',
    textPos,
) where

import Classh
import Reflex.Dom.Core as Dom

import Control.Monad (void)
import qualified Data.Map as Map
import qualified Data.Text as T

type Shell m a = m a -> m a

-- | Like 'elClass' except that it takes a 'BoxConfig' instead of a string for classes
elTW :: (DomBuilder t m) => T.Text -> BoxConfig -> m a -> m a
elTW etag cfg ma = elClass etag (defaultClasses <> " " <> showTW cfg) ma

-- | Like 'elClass'' except that it takes a 'BoxConfig' instead of a string for classes
elTW' :: (DomBuilder t m) => T.Text -> BoxConfig -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elTW' etag cfg ma = elClass' etag (defaultClasses <> " " <> showTW cfg) ma

-- | Like 'elDynClass' except that it takes a 'BoxConfig' instead of a string for classes
elDynTW :: (PostBuild t m, DomBuilder t m) => T.Text -> Dynamic t BoxConfig -> m a -> m a
elDynTW etag cfgDyn ma = elDynClass etag ((\cfg -> defaultClasses <> " " <> showTW cfg) <$> cfgDyn) ma

-- | Like 'elDynClass'' except that it takes a 'BoxConfig' instead of a string for classes
elDynTW' :: (PostBuild t m, DomBuilder t m) => T.Text -> Dynamic t BoxConfig -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elDynTW' etag cfgDyn ma = elDynClass' etag ((\cfg -> defaultClasses <> " " <> showTW cfg) <$> cfgDyn) ma

-- | A bare @\<div\>@ with no classes
div_ :: (DomBuilder t m) => m a -> m a
div_ = el "div"

{- | When we want to create an area devoid of influence by outside styles.
Most common case is for differing displays like inline-block vs grid.

In practice this can also be used to solve a common problem where
helper functions unnecessarily restrict size of children.
-}
in_ :: (DomBuilder t m) => m a -> m a
in_ = divClass $(classh' [h .~~ pct 100, w .~~ pct 100])

{- | Like 'in_' but only forces height to 100% of container.

Example use case:

> divClass $(classh' [pos .~~ centered]) $ do
>   inH_ leaf
-}
inH_ :: (DomBuilder t m) => m a -> m a
inH_ = divClass $(classh' [h .~~ pct 100])

type PlaceHolder = T.Text

-- | Build an @\<input\>@ element with box and text classes plus a placeholder
inputEl :: (DomBuilder t m) => T.Text -> T.Text -> PlaceHolder -> m ()
inputEl elClasses textClasses placeholder =
    void $
        inputElement $
            def
                & inputElementConfig_elementConfig
                . initialAttributes
                .~ ( "class" =: (elClasses <&> textClasses)
                        <> "placeholder" =: placeholder
                   )

type TextAreaRows = Int

-- | Build a @\<textarea\>@ element with box and text classes, row count, and placeholder
textAreaEl :: (DomBuilder t m) => TextAreaRows -> T.Text -> T.Text -> PlaceHolder -> m ()
textAreaEl rows elClasses textClasses placeholder =
    void $
        textAreaElement $
            def
                & textAreaElementConfig_elementConfig
                . initialAttributes
                .~ ( "class" =: (elClasses <&> textClasses)
                        <> "rows" =: tshow rows
                        <> "placeholder" =: placeholder
                   )

-- | Build a responsive @\<img\>@ element with srcset and sizes attributes
type ScreenWidth = DimensionConstraint

imgResponsive ::
    (DomBuilder t m) =>
    [(StaticImagePath, (CSSSize, ScreenWidth))] ->
    CompiledS ->
    m ()
imgResponsive imageInfo classes = elAttr "img" (imgSrcSet imageInfo classes) blank

type StaticImagePath = T.Text
type Pixels = Int

{- | Build a 'Map.Map' of @srcset@, @sizes@, and @class@ attributes from a list of
image paths paired with their CSS size and max-width breakpoint.
Errors if the input list is empty.
-}
imgSrcSet :: [(StaticImagePath, (CSSSize, DimensionConstraint))] -> CompiledS -> Map.Map T.Text T.Text
imgSrcSet [] _ = error "No images given to Classh.Reflex.imgSrcSet"
imgSrcSet xs classes =
    let
        mkPair = \(src', (w_, _)) -> src' <&> renderCSS w_ <> "w"
        pairs = mkPair <$> xs

        showMW :: DimensionConstraint -> T.Text
        showMW m_ = "(max-width: " <> showTW m_ <> ")"

        mkSizes :: [(StaticImagePath, (CSSSize, DimensionConstraint))] -> T.Text
        mkSizes [] = error "mkSizes received no input"
        mkSizes ((_, (w_, maxW__)) : xs_) =
            if null xs_
                then renderCSS w_
                else showMW maxW__ <> renderCSS w_ <> "," <> mkSizes xs_
     in
        "srcset" =: T.intercalate "," pairs
            <> "sizes" =: mkSizes xs
            <> "class" =: classes

-- | Compile-time checked @divClass@ builder from a base 'BoxConfig' and list of mutations
divClassh :: BoxConfig -> [BoxConfig -> BoxConfig] -> Compiled Expression
divClassh base muts = case compileS $ foldl (\acc f -> f acc) base muts of
    Left e -> fail $ T.unpack e
    Right styleString -> [|divClass styleString|]

-- | Like 'divClassh' but starts from 'def'
divClassh' :: [BoxConfig -> BoxConfig] -> Compiled Expression
divClassh' muts = case compileS $ foldl (\acc f -> f acc) def muts of
    Left e -> fail $ T.unpack e
    Right styleString -> [|divClass styleString|]

-- | Compile-time checked text builder from a base 'TextConfigTW' and list of mutations
textClassh :: TextConfigTW -> [TextConfigTW -> TextConfigTW] -> Compiled Expression
textClassh base muts = case compileS $ foldl (\acc f -> f acc) base muts of
    Left e -> fail $ T.unpack e
    Right styleString -> [|textS styleString|]

-- | Like 'textClassh' but starts from 'def'
textClassh' :: [TextConfigTW -> TextConfigTW] -> Compiled Expression
textClassh' muts = case compileS $ foldl (\acc f -> f acc) def muts of
    Left e -> fail $ T.unpack e
    Right styleString -> [|textS styleString|]

-- | Like 'textClassh'' but for 'TextPosition' configs
textPos' :: [TextPosition -> TextPosition] -> Compiled Expression
textPos' muts = case compileS $ foldl (\acc f -> f acc) def muts of
    Left e -> fail $ T.unpack e
    Right styleString -> [|textPosition styleString|]

-- | Like 'textClassh' but for 'TextPosition' configs
textPos :: TextPosition -> [TextPosition -> TextPosition] -> Compiled Expression
textPos base muts = case compileS $ foldl (\acc f -> f acc) base muts of
    Left e -> fail $ T.unpack e
    Right styleString -> [|textPosition styleString|]
