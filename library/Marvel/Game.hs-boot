module Marvel.Game where

import Marvel.Prelude

import Control.Monad.Catch
import {-# SOURCE #-} Marvel.Queue

class HasGame a

class
  ( MonadCatch m
  , MonadThrow m
  , MonadIO m
  , MonadReader env m
  , HasGame env
  , HasQueue env
  , MonadRandom m
  )
  => MonadGame env m | env -> m
