module Marvel.Game where

import Marvel.Prelude

import Marvel.Ability.Type
import Marvel.Debug
import Marvel.Id
import {-# SOURCE #-} Marvel.Queue

class HasGame a

class
  ( MonadCatch m
  , MonadThrow m
  , MonadIO m
  , MonadReader env m
  , HasGame env
  , HasQueue env
  , HasDebugLogger env
  , MonadRandom m
  )
  => MonadGame env m | env -> m, m -> env

data Game

getPlayers :: MonadGame env m => m [IdentityId]
getsGame :: MonadGame env m => (Game -> a) -> m a
getUsedAbilities :: MonadGame env m => m (HashMap IdentityId [Ability])

instance HasAbilities Game
