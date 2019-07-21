module Spudcast.Storage
  ( listObjects
  ) where

import Control.Lens ( (<&>)
                    , (.~)
                    )
import Network.Google ( LogLevel (..)
                      , envLogger
                      , envScopes
                      , newEnv
                      , newLogger
                      , runGoogle
                      , runResourceT
                      , send
                      )
import Network.Google.Storage ( Objects
                              , objectsList
                              , storageReadWriteScope
                              )
import System.IO (stdout)

listObjects :: IO Objects
listObjects = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ storageReadWriteScope)
  runResourceT . runGoogle env $
    send $ objectsList "www.stanleystots.com"
