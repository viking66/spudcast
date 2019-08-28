{-# LANGUAGE DuplicateRecordFields #-}
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

genPodcastResp :: Gen PodcastResp
genPodcastResp = do
  pId <- Gen.text (Range.linear 10 20) Gen.unicode
  time <- genUTCTime
  title <- Gen.text (Range.linear 10 100) Gen.unicode
  description <- Gen.text (Range.linear 10 500) Gen.unicode
  link <- Gen.text (Range.linear 10 50) Gen.unicode
  host <- Gen.text (Range.linear 5 20) Gen.unicode
  email <- Gen.text (Range.linear 10 50) Gen.unicode
  explicit <- Gen.bool
  category <- Gen.text (Range.linear 10 20) Gen.unicode
  pure $ PodcastResp
    { _podcastId = pId
    , _createDate = time
    , _title = title
    , _description = description
    , _link = link
    , _host = host
    , _email = email
    , _explicit = explicit
    , _category = category
    }

genNewPodcastDetails :: Gen (NewPodcastDetails, UTCTime)
genNewPodcastDetails = do
  time <- genUTCTime
  title <- Gen.text (Range.linear 10 100) Gen.unicode
  description <- Gen.text (Range.linear 10 500) Gen.unicode
  link <- Gen.text (Range.linear 10 50) Gen.unicode
  host <- Gen.text (Range.linear 5 20) Gen.unicode
  email <- Gen.text (Range.linear 10 50) Gen.unicode
  explicit <- Gen.bool
  category <- Gen.text (Range.linear 10 20) Gen.unicode
  let npd = NewPodcastDetails
              { _title = title
              , _description = description
              , _link = link
              , _host = host
              , _email = email
              , _explicit = explicit
              , _category = category
              }
  pure (npd, time)

genEpisodeDetails :: Gen (EpisodeDetails, UTCTime)
genEpisodeDetails = do
  title <- Gen.text (Range.linear 10 100) Gen.unicode
  description <- Gen.text (Range.linear 10 500) Gen.unicode
  number <- Gen.int (Range.constant 0 100)
  time <- genUTCTime
  pure $ (EpisodeDetails title description number, time)

prop_podcastRespIso :: Property
prop_podcastRespIso =
  property $ do
    resp <- forAll genPodcastResp
    assert $ resp^.podcastRespIso^.from podcastRespIso == resp

prop_newPodcastIso :: Property
prop_newPodcastIso =
  property $ do
    npd <- forAll genNewPodcastDetails
    assert $ npd^.newPodcastIso^.from newPodcastIso == npd

prop_newEpIso :: Property
prop_newEpIso =
  property $ do
    edt <- forAll genEpisodeDetails
    assert $ edt^.newEpIso^.from newEpIso == edt

tests :: IO Bool
tests = checkParallel $$(discover)
