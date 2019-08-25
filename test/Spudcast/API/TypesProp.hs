{-# LANGUAGE TemplateHaskell #-}
module Spudcast.API.TypesProp (tests) where

import Control.Lens
import Data.Time
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Spudcast.API.Types

genUTCTime :: Gen UTCTime
genUTCTime = do
  y <- toInteger <$> Gen.int (Range.constant 2000 2019)
  m <- Gen.int (Range.constant 1 12)
  d <- Gen.int (Range.constant 1 28)
  let day = fromGregorian y m d
  secs <- toInteger <$> Gen.int (Range.constant 0 86400)
  let diff = secondsToDiffTime secs
  pure $ UTCTime day diff

genEpisodeDetails :: Gen (EpisodeDetails, UTCTime)
genEpisodeDetails = do
  title <- Gen.text (Range.linear 10 100) Gen.unicode
  description <- Gen.text (Range.linear 10 500) Gen.unicode
  number <- Gen.int (Range.constant 0 100)
  time <- genUTCTime
  pure $ (EpisodeDetails title description number, time)

prop_newEpIso :: Property
prop_newEpIso =
  property $ do
    edt <- forAll genEpisodeDetails
    assert $ edt^.newEpIso^.from newEpIso == edt

tests :: IO Bool
tests = checkParallel $$(discover)
