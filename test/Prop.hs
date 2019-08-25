module Main where

import Hedgehog.Main

import qualified Spudcast.API.TypesProp as Spudcast.API.TypesProp

main :: IO ()
main = defaultMain
  [ Spudcast.API.TypesProp.tests
  ]
