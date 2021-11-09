{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
module Marvel.Message where

import Marvel.Prelude

import GHC.Generics
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Card.PlayerCard
import Marvel.Card.Side
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Id
import Marvel.Phase
import {-# SOURCE #-} Marvel.Question
import Marvel.Source
import Marvel.Target
import Marvel.Window

data FromZone = FromDeck
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Message
  = StartGame
  | StartScenario
  | BeginPhase Phase
  | AddVillain CardCode
  | SetPlayerOrder [IdentityId]
  | IdentityMessage IdentityId IdentityMessage
  | VillainMessage VillainId VillainMessage
  | EventMessage EventId EventMessage
  | Ask IdentityId Question
  | UsedAbility IdentityId Ability
  | RanAbility Target Natural
  | WithDiscarded Target FromZone [PlayerCard]
  | SetActiveCost ActiveCost
  | Spent PlayerCard
  | Paid Payment
  | FinishedPayment
  | PutCardIntoPlay IdentityId PlayerCard Payment
  | CheckWindows [Window]
  | EndCheckWindows
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data VillainMessage = SetVillainHp | VillainDamaged Source Natural
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EventMessage = PlayedEvent IdentityId Payment
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data IdentityMessage
  = BeginTurn
  | PlayerTurnOption
  | CheckIfPassed
  | ChooseOtherForm
  | EndedTurn
  | ChangedToForm Side
  | DrawStartingHand HandSize
  | DrawCards FromZone Natural
  | ShuffleDeck
  | SideMessage SideMessage
  | PlayedCard PlayerCard
  | PayedWithCard PlayerCard
  | AllyCreated AllyId
  | AddToHand PlayerCard
  | Discard FromZone Natural (Maybe Target)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SideMessage = SetupIdentity
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

class (forall a b. Coercible a b => Coercible (f a) (f b)) => CoerceRole f
instance (forall a b. Coercible a b => Coercible (f a) (f b)) => CoerceRole f

class RunMessage a where
  runMessage :: (MonadGame env m, CoerceRole m) => Message -> a -> m a

class RunMessage' f where
  runMessage' :: (MonadGame env m, CoerceRole m) => Message -> f p -> m (f p)

genericRunMessage
  :: (MonadGame env m, RunMessage' (Rep a), Generic a, CoerceRole m)
  => Message
  -> a
  -> m a
genericRunMessage msg = fmap to . runMessage' msg . from

instance RunMessage' f => RunMessage' (M1 i c f) where
  runMessage' msg = fmap M1 . runMessage' msg . unM1

instance (RunMessage' l, RunMessage' r) => RunMessage' (l :+: r) where
  runMessage' msg = \case
    L1 x -> L1 <$> runMessage' msg x
    R1 x -> R1 <$> runMessage' msg x

instance RunMessage c => RunMessage' (K1 i c) where
  runMessage' msg = fmap K1 . runMessage msg . unK1
