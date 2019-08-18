module Spudcast.AppM
  ( AppM
  , runAppM
  ) where

import Control.Monad.Reader
import Control.Monad.Except

import Spudcast.Types

type AppM = ReaderT Env (ExceptT AppError IO)

runAppM :: AppM a -> Env -> IO (Either AppError a)
runAppM a = runExceptT . runReaderT a
