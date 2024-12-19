{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Reflex.Text
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  A collection of Classh-typed versions of elClass and similar funcs using classhUnsafe
--
-------------------------------------------------------------------------------


module Classh.Reflex.El where

import Reflex.Dom.Core
import Classh
import qualified Data.Text as T

-- | is elClass except that it takes a BoxConfig instead of a string for classes
elTW :: DomBuilder t m => T.Text -> BoxConfig -> m a -> m a
elTW etag cfg ma = elClass etag (defaultClasses <> " " <> showTW cfg) ma

-- | is elClass' except that it takes a BoxConfig instead of a string for classes. 
elTW' :: DomBuilder t m => T.Text -> BoxConfig -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elTW' etag cfg ma = elClass' etag (defaultClasses <> " " <> showTW cfg) ma

-- | is elDynClass except that it takes a BoxConfig instead of a string for classes
elDynTW :: (PostBuild t m, DomBuilder t m) => T.Text -> Dynamic t BoxConfig -> m a -> m a
elDynTW etag cfgDyn ma = elDynClass etag ( (\cfg -> defaultClasses <> " " <> showTW cfg) <$> cfgDyn ) ma

-- | is elDynClass' except that it takes a BoxConfig instead of a string for classes
elDynTW' :: (PostBuild t m, DomBuilder t m) => T.Text -> Dynamic t BoxConfig -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elDynTW' etag cfgDyn ma = elDynClass' etag ( (\cfg -> defaultClasses <> " " <> showTW cfg) <$> cfgDyn ) ma
