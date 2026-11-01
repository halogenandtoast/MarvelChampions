module Marvel.Environment.Environments.KreeCommandShip (
  kreeCommandShip,
  KreeCommandShip (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Ref ()
import Marvel.Window qualified as W

kreeCommandShip :: EnvironmentCard KreeCommandShip
kreeCommandShip = environment KreeCommandShip Cards.kreeCommandShip

-- | Put into play by Interception Imminent's setup.
newtype KreeCommandShip = KreeCommandShip (Attrs Environment)
  deriving anyclass (IsEnvironment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

-- | First Player Interrupt: when a treachery card is revealed from the encounter deck,
-- exhaust the Milano and spend 1 resource of any type → cancel that card's "When
-- Revealed" effects.
instance HasAbilities KreeCommandShip where
  getAbilities (KreeCommandShip a) =
    [ limitedWindowAbility
        a
        1
        (W.TreacheryRevealed W.When AnyTreachery W.RevealedFromEncounterDeck)
        Interrupt
        (SelfMatches FirstPlayer)
        (ExhaustMilanoCost <> MultiResourceCost [Nothing])
        $ runAbility a 1
    ]

instance RunMessage KreeCommandShip where
  runMessage msg e@(KreeCommandShip attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 [W.RevealTreachery tid W.RevealedFromEncounterDeck] _ -> do
      cancelWhenRevealed tid
      pure e
    _ -> KreeCommandShip <$> runMessage msg attrs
