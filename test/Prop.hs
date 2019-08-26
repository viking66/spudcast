module Main where

import Control.Monad (unless)
import System.Exit (exitFailure)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)

import qualified Spudcast.API.TypesProp as Spudcast.API.TypesProp

defaultMain :: [IO Bool] -> IO ()
defaultMain tests = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  result <- and <$> sequence tests
  unless result
    exitFailure

main :: IO ()
main = defaultMain
  [ Spudcast.API.TypesProp.tests
  ]
