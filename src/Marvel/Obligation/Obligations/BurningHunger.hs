module Marvel.Obligation.Obligations.BurningHunger where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Query ()
import Marvel.Ref
import Marvel.Trait qualified as Trait

burningHunger :: ObligationCard BurningHunger
burningHunger = obligation BurningHunger Cards.burningHunger

newtype BurningHunger = BurningHunger (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage BurningHunger where
  runMessage msg e@(BurningHunger attrs) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId attrs -> do
      unleashed <- identityMatches (IdentityWithTrait Trait.Unleashed) you
      if unleashed
        then do
          cards <- (<>) <$> getEncounterDeck <*> getEncounterDiscard
          aside <- getSetAsideEncounterCards
          let found = any (cardMatch $ CardIs Minions.darkPhoenix) (cards <> aside)
          searchEverywhereAndRevealEncounterCard you (CardIs Minions.darkPhoenix)
          when found $ removeFromGame attrs
          pure e
        else do
          restrained <- identityMatches (IdentityWithTrait Trait.Restrained) you
          when restrained do
            removePhoenixPower you 1
            gainSurge attrs
          pure e
    _ -> BurningHunger <$> runMessage msg attrs
