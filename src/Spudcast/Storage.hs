module Spudcast.Storage
  ( write
  ) where

import Control.Lens ( (<&>)
                    , (.~)
                    , (&)
                    , (?~)
                    )
import Network.Google ( LogLevel (..)
                      , envLogger
                      , envScopes
                      , newEnv
                      , newLogger
                      , runGoogle
                      , runResourceT
                      , sourceBody
                      , upload
                      )
import Network.Google.Storage ( Object
                              , object'
                              , objectsInsert
                              , oiName
                              , storageReadWriteScope
                              )
import System.IO (stdout)
import Data.Text (Text)

write :: Text -> FilePath -> Text -> IO Object
write bucket input output = do
  lgr <- newLogger Debug stdout
  env <- newEnv <&> (envLogger .~ lgr) . (envScopes .~ storageReadWriteScope)
  body <- sourceBody input
  runResourceT . runGoogle env $
    upload (objectsInsert bucket object' & oiName ?~ output) body
