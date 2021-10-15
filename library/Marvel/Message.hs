module Marvel.Message where

import Marvel.Prelude

import GHC.Generics
import Marvel.Card.Code
import Marvel.Card.PlayerCard
import {-# SOURCE #-} Marvel.Game
import Marvel.Id
import {-# SOURCE #-} Marvel.Identity
import Marvel.Phase
import {-# SOURCE #-} Marvel.Question

data Message
  = StartGame
  | StartScenario
  | BeginPhase Phase
  | AddVillain CardCode
  | SetPlayerOrder [PlayerIdentity]
  | IdentityMessage IdentityId IdentityMessage
  | Ask IdentityId Question
  deriving stock Show

data IdentityMessage = SetDeck [PlayerCard] | BeginTurn | TakeAction | CheckIfPassed
  deriving stock Show

class RunMessage a where
  runMessage :: MonadGame env m => Message -> a -> m a

class RunMessage' f where
  runMessage' :: MonadGame env m => Message -> f p -> m (f p)

genericRunMessage
  :: (MonadGame env m, RunMessage' (Rep a), Generic a) => Message -> a -> m a
genericRunMessage msg = fmap to . runMessage' msg . from

instance RunMessage' f => RunMessage' (M1 i c f) where
  runMessage' msg = fmap M1 . runMessage' msg . unM1

instance (RunMessage' l, RunMessage' r) => RunMessage' (l :+: r) where
  runMessage' msg = \case
    L1 x -> L1 <$> runMessage' msg x
    R1 x -> R1 <$> runMessage' msg x

instance RunMessage c => RunMessage' (K1 i c) where
  runMessage' msg = fmap K1 . runMessage msg . unK1
