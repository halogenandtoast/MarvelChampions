{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Support.Supports.AosP50015 where

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
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

aosP50015 :: SupportCard AosP50015
aosP50015 = support AosP50015 Cards.aosP50015

newtype AosP50015 = AosP50015 (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50015 where
  getAbilities (AosP50015 attrs) =
    [limitedWindowAbility attrs 1 (W.EncounterCardRevealedBy W.When (IdentityWithId $ supportController attrs) AnyEncounterCard) Interrupt (OwnsThis <> SelfMatches (IdentityWithTrait T.Shield) <> NotCriteria (AllyExists $ AllyControlledBy You <> NotAlly (AllyWithTrait T.Shield)) <> CanCancelRevealedEncounterCard) ExhaustCost (runAbility attrs 1)]

instance RunMessage AosP50015 where
  runMessage msg card@(AosP50015 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      for_ [encounter | W.EncounterCardRevealed _ _ encounter <- windows] $ \encounter -> do
        cancelEncounterCard encounter
        discardCard encounter
        drawAndRevealEncounterCard you
      pure card
    _ -> AosP50015 <$> runMessage msg attrs
