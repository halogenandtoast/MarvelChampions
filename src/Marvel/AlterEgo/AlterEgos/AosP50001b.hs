{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.AlterEgo.AlterEgos.AosP50001b where

import Marvel.Prelude
import Marvel.Question
import Marvel.Id
import Marvel.Projection
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Payment
import Marvel.Query
import Marvel.Resource.Types qualified as R
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.AgentsOfShield.PlayerHelpers

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats
import Marvel.Obligation.Cards qualified as Obligations

aosP50001b :: AlterEgoCard AosP50001b
aosP50001b = alterEgo AosP50001b Cards.aosP50001b (HP $ Static 9) (HandSize 6) (Rec 3) [Obligations.aosP50029]

newtype AosP50001b = AosP50001b (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50001b where
  getAbilities (AosP50001b attrs) =
    [ability attrs 1 Action (OwnsThis <> ExtendedCardExists (InDeckOf You $ BasicCardMatches $ CardWithType SupportType <> CardWithTrait T.Shield)) ExhaustCost (runAbility attrs 1)]

instance RunMessage AosP50001b where
  runMessage msg card@(AosP50001b attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> card <$ search you (SearchIdentityDeck you AllOfDeck) (CardWithType SupportType <> CardWithTrait T.Shield) SearchDrawOne ShuffleBackIn
    _ -> AosP50001b <$> runMessage msg attrs
