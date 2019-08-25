module Spudcast.API.TypesSpec
  ( spec
  ) where

import Data.Time
import Test.Hspec

import Spudcast.Types
import Spudcast.API.Types

spec :: Spec
spec = do
  describe "podcastToResp" $ do
    it "constructs PodcastResp with proper fields" $ do
      let pd = PodcastDetails utcTime title' description' link' host' email'
            explicit' category'
          podcast = Podcast podcastId' pd
          resp = PodcastResp podcastId' utcTime title' description' link' host'
            email' explicit' category'
      podcastToResp podcast `shouldBe` resp
  describe "reqToPodcastDetails" $ do
    it "constructs PodcastDetails with proper fields" $ do
      let ext = "jpeg"
          npd = NewPodcastDetails title' description' link' host' email'
            explicit' category'
          req = CreatePodcastReq npd path ext
          pd = PodcastDetails utcTime title' description' link' host' email'
            explicit' category'
      reqToPodcastDetails req utcTime `shouldBe` pd
  where
    podcastId' = "podcastId"
    utcTime = UTCTime (ModifiedJulianDay 58713) 0
    title' = "title"
    description' = "description"
    link' = "link"
    host' = "host"
    email' = "email"
    explicit' = True
    category' = "category"
    path = "foo/bar/baz"
