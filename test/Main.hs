{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Classh (DimensionConstraint (..), pix)
import Classh.Reflex.El (imgSrcSet)
import Control.Exception (SomeException, evaluate, try)
import qualified Data.Map as Map
import qualified Data.Text as T
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "reflex-classhss"
        [ testGroup
            "imgSrcSet"
            [ testProperty "single image produces srcset, sizes, class keys" prop_singleImageKeys
            , testProperty "multiple images comma-separates srcset" prop_multiImageSrcset
            , testProperty "class key matches input" prop_classPassthrough
            , testProperty "empty input errors" prop_emptyErrors
            ]
        ]

-- Helpers

genPath :: Gen T.Text
genPath = do
    name <- Gen.text (Range.linear 1 20) Gen.alphaNum
    ext <- Gen.element [".jpg", ".png", ".webp"]
    pure (name <> ext)

genPixelWidth :: Gen Int
genPixelWidth = Gen.int (Range.linear 50 2000)

genDimensionConstraint :: Gen DimensionConstraint
genDimensionConstraint = Gen.element [DC_sm, DC_md, DC_lg, DC_xl, DC_2xl]

-- Properties

prop_singleImageKeys :: Property
prop_singleImageKeys = property $ do
    path <- forAll genPath
    w <- forAll genPixelWidth
    dc <- forAll genDimensionConstraint
    let result = imgSrcSet [(path, (pix w, dc))] "my-class"
    Map.member "srcset" result === True
    Map.member "sizes" result === True
    Map.member "class" result === True

prop_multiImageSrcset :: Property
prop_multiImageSrcset = property $ do
    n <- forAll $ Gen.int (Range.linear 2 5)
    rawPairs <-
        forAll $
            Gen.list
                (Range.singleton n)
                ( (,,)
                    <$> genPath
                    <*> genPixelWidth
                    <*> genDimensionConstraint
                )
    let pairs = [(p, (pix w, dc)) | (p, w, dc) <- rawPairs]
        result = imgSrcSet pairs "cls"
        srcset = result Map.! "srcset"
        commaCount = T.count "," srcset
    commaCount === n - 1

prop_classPassthrough :: Property
prop_classPassthrough = property $ do
    path <- forAll genPath
    cls <- forAll $ Gen.text (Range.linear 1 30) Gen.alphaNum
    let result = imgSrcSet [(path, (pix 100, DC_md))] cls
    result Map.! "class" === cls

prop_emptyErrors :: Property
prop_emptyErrors = withTests 1 . property $ do
    threw <- evalIO $ do
        r <- try (evaluate (imgSrcSet [] "x")) :: IO (Either SomeException (Map.Map T.Text T.Text))
        pure $ case r of
            Left _ -> True
            Right _ -> False
    threw === True
