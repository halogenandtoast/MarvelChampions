module Marvel.Message where

import Marvel.Prelude

import Marvel.Card.Code
import {-# SOURCE #-} Marvel.Game
import Marvel.Identity
import Marvel.Player
import GHC.Generics

newtype Sorted a = Sorted { unSorted :: [a] }
  deriving newtype (Show, Semigroup, Monoid)

newtype Unsorted a = Unsorted { unUnsorted :: [a] }
  deriving newtype (Show, Semigroup, Monoid)

data Message
  = ChoosePlayerOrder IdentityId (Unsorted Player) (Sorted Player)
  | StartGame
  | StartScenario
  | AddVillain CardCode
  | Ask IdentityId Question
  deriving stock Show

newtype Question = ChooseOne [Choice]
  deriving stock Show

data Choice = CardLabel CardCode [Message]
  deriving stock Show

choiceMessages :: Choice -> [Message]
choiceMessages = \case
  CardLabel _ msgs -> msgs

class RunMessage a where
  runMessage :: MonadGame env m => Message -> a -> m a

class RunMessage' f where
  runMessage' :: MonadGame env m => Message -> f p -> m (f p)

genericRunMessage :: (MonadGame env m, RunMessage' (Rep a), Generic a) => Message -> a -> m a
genericRunMessage msg = fmap to . runMessage' msg . from

instance RunMessage' f => RunMessage' (M1 i c f) where
  runMessage' msg = fmap M1 . runMessage' msg . unM1

instance (RunMessage' l, RunMessage' r) => RunMessage' (l :+: r) where
  runMessage' msg = \case
    L1 x -> L1 <$> runMessage' msg x
    R1 x -> R1 <$> runMessage' msg x

instance RunMessage c => RunMessage' (K1 i c) where
  runMessage' msg = fmap K1 . runMessage msg . unK1
