{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Classh.Reflex where

import Reflex.Dom.Core
import Classh

import qualified Data.Text as T

intercalate :: DomBuilder t m => WhenTW TextSize -> T.Text -> [m ()] -> m ()
intercalate size inter (t:ts) = do
  t >> prependAll size inter ts
  where
    prependAll _ _ [] = pure ()
    prependAll s i (t:ts) = elClass "span" (renderWhenTW s showTW ) (text i) >> t >> prependAll s i ts

-- | Works by setting top padding by one consistent value. Does not add padding for first element
paragraphs :: DomBuilder t m => WhenTW TWSize -> [m ()] -> m ()
paragraphs spacing (r:rows) = do
  row [] $ r
  withTopPadding spacing rows
  where
    withTopPadding _ [] = pure ()
    withTopPadding p (r:rs) = row [t .~ p] r >> withTopPadding p rs

-- | Works by setting top padding variably
paragraphs' :: DomBuilder t m => [(WhenTW TWSize, m ())] -> m ()
paragraphs' ((s,r):rows) = row [t .~ s] r >> paragraphs' rows

rows :: DomBuilder t m => [(WhenTW TWSize, m ())] -> m ()
rows = paragraphs'

-- | TODO: should we have a variant which controls for padding needs when being a full row? What should that look like? Do we even need that??
responsiveRowCol :: DomBuilder t m => [Int] -> m a -> m a
responsiveRowCol colSpans m =
  let spandex = renderWhenTW (zipScreens colSpans) ((<>) "col-span-" . tshow)
  in elClass "div" spandex m


-- | Not real: just ideating
-- | How can we model a discrete set of consistent brand options?
--
-- data Mytextsize = Heading | Subtext | Normal
-- domain = Map.fromList [ (Heading , XL7) , (Normal , XL3) , (Subtext , XS) ]
--
-- brandF :: k -> WhenTW k
-- brandF (domainType :: Ord k => k) =
--   let baseSize = fromJust $ Map.lookup domainType (domain :: Map (k :: Ord k => k) (v :: ShowTW x => x))
--   in
--     autoScale baseSize
--
-- then this above code suggests that we should think of our domain as mobile first
-- it is important to remember this is not meant to represent 100% of CSS but rather ~80%
-- and should be a way to communicate branding concisely such that a stoopid cud impl. it to be
-- consistent
--
-- Now, it wont always be super complex: for example, color
--
-- def = TW.def & text_color .~~ aceBlue
--
-- But then again, a brand may have multiple colors
--
-- brandColors = Map.fromList [ (AceBlue, hex _), (AceNavyBlue, hex _), (AceBabyBlue, hex _) ]
-- and also:
       -- What about dark mode? \
       --
-- and note: lets say 90% of titles should have Bold as their weight, we can set this and


-- | TODO: this should have the ability to add rows somehow
-- | and how we can think of this is that if we start at a large->Col12 and scale down to
-- | lets say a 'sm' then Col12 / n where n is from config. So lets say n=2 then sm->Col6 which will show up as
-- | 2 rows of 6; similarly n=6 => 6 rows of 2 ; n=12 -> 12 rows of 1; n=3 => 3 rows of 4 ; n=4 => 4 rows of 3
gridColWhen :: DomBuilder t m => WhenTW ColInt -> m a -> m a
gridColWhen cInts ma = elClass "div" (showCInts cInts) ma
  where
    showCInts [] = ""
    showCInts (("def",cInt):cInts) = "grid grid-cols-" <> showTW cInt <&> showCInts cInts
    showCInts ((w,cInt):cInts) = w <> ":" <> "grid" <&> w <> ":" <> "grid-cols-" <> showTW cInt
                                 <&> showCInts cInts

-- | Denotes a normal row. Does not have inline-block
-- row :: DomBuilder t m => BoxPadding -> m a -> m a
-- row padding = elClass "div" ( "" <&> showTW padding )

gridColW :: DomBuilder t m => ColInt -> TWSizeOrFraction -> m a -> m a
gridColW cInt width ma = elClass "div" ("grid grid-cols-" <> showTW cInt <&> ("w-" <> showTW width)) ma



row :: DomBuilder t m => [BoxPadding -> BoxPadding] -> m a -> m a
row paddingF = elClass "div" ( "" <&> showTW (applyFs def paddingF) )


-- | Purposefully designed to take no other data besides ColInt in order to be very self-contained
gridCol :: DomBuilder t m => ColInt -> m a -> m a
gridCol cInt ma = elClass "div" ("grid grid-cols-" <> showTW cInt) ma

-- | TODO: styledParagraphs :: [( BottomPadding, [(tw,T.Text)])] -> m ()
-- then apply to bannerFor in Landing.*

-- | TODO: variants which do not get checked/compiled
textS :: DomBuilder t m => CompiledS -> T.Text -> m ()
textS s txt = elClass "span" s $ text txt

textU :: DomBuilder t m => CompiledS -> T.Text -> m ()
textU s txt = elClass "u" s $ text txt

textDynS :: (PostBuild t m, DomBuilder t m) => Dynamic t CompiledS -> T.Text -> m ()
textDynS s txt = elDynClass "span" s $ text txt

dynTextS :: (PostBuild t m, DomBuilder t m) => CompiledS -> Dynamic t T.Text -> m ()
dynTextS s txt = elClass "span" s $ dynText txt

dynTextDynS :: (PostBuild t m, DomBuilder t m) => Dynamic t CompiledS -> Dynamic t T.Text -> m ()
dynTextDynS s txt = elDynClass "span" s $ dynText txt


-- buttonC :: Map.Map T.Text T.Text -> m a -> m (Event t a)
-- buttonC attrs inner = do
--   (e,x) <- elAttr' "button" attrs inner
--   pure $ x <$ domEvent Click e


elTW :: DomBuilder t m => T.Text -> BoxConfig -> m a -> m a
elTW tag cfg m = elClass tag (defaultClasses <> " " <> showTW cfg) m

elTW' :: DomBuilder t m => T.Text -> BoxConfig -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elTW' tag cfg m = elClass' tag (defaultClasses <> " " <> showTW cfg) m

elDynTW :: (PostBuild t m, DomBuilder t m) => T.Text -> Dynamic t BoxConfig -> m a -> m a
elDynTW tag cfgDyn m = elDynClass tag ( (\cfg -> defaultClasses <> " " <> showTW cfg) <$> cfgDyn ) m

elDynTW' :: (PostBuild t m, DomBuilder t m) => T.Text -> Dynamic t BoxConfig -> m a -> m (Element EventResult (DomBuilderSpace m) t, a)
elDynTW' tag cfgDyn m = elDynClass' tag ( (\cfg -> defaultClasses <> " " <> showTW cfg) <$> cfgDyn ) m

-- textTW' :: (DomBuilder t m, ShowTW tw) => tw -> T.Text -> m ()
-- textTW' = styledText'

-- styledText' :: (DomBuilder t m, ShowTW tw) => tw -> T.Text -> m ()
-- styledText' cfg txt = elClass "span" (showTW cfg) $ text txt

-- dynTextTW' :: (DomBuilder t m, PostBuild t m, ShowTW tw) => tw -> Dynamic t T.Text -> m ()
-- dynTextTW' cfg txt = elClass "span" (showTW cfg) $ dynText txt

-- textDynTW' :: (DomBuilder t m, PostBuild t m, ShowTW tw) => Dynamic t tw -> T.Text -> m ()
-- textDynTW' dynCfg txt = elDynClass "span" ( (\cfg -> defaultClasses <> " " <> showTW cfg) <$> dynCfg ) $ text txt

-- styledTexts' :: (ShowTW tw, DomBuilder t m) => [(tw, T.Text)] -> m ()
-- styledTexts' styTexs = el "div" $ forM_ styTexs $ \(cfg, txt) -> styledText' cfg txt

