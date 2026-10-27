module Marvel.Upgrade.Upgrades.DeftFocus (
  deftFocus,
  deftFocusEffect,
  DeftFocus (..),
  DeftFocusEffect (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

deftFocus :: UpgradeCard DeftFocus
deftFocus = upgrade DeftFocus Cards.deftFocus

newtype DeftFocus = DeftFocus (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Exhaust Deft Focus → reduce the resource cost of the next Superpower
-- card you play this turn by 1.
instance HasAbilities DeftFocus where
  getAbilities (DeftFocus a) = [ability a 1 HeroAction OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage DeftFocus where
  runMessage msg u@(DeftFocus attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      createEffect Cards.deftFocus attrs (IdentityEntity $ IdentityWithId you)
      pure u
    _ -> DeftFocus <$> runMessage msg attrs

superpower :: CardMatcher
superpower = CardWithTrait Superpower

-- | The next Superpower card the player plays this turn costs 1 less.
newtype DeftFocusEffect = DeftFocusEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

deftFocusEffect :: CardEffect DeftFocusEffect
deftFocusEffect =
  effectWith DeftFocusEffect Cards.deftFocus $
    (modifiersL .~ [ResourceCostReductionFor superpower 1])
      . (endsL ?~ DisableAtEndOfTurn)

instance RunMessage DeftFocusEffect where
  runMessage msg e@(DeftFocusEffect attrs) = runQueueT $ case msg of
    IdentityMessage ident (PlayedCard card _) | cardMatch superpower card -> do
      whenM (effectValidFor attrs (toRef ident)) $ disableEffect (effectId attrs)
      pure e
    _ -> DeftFocusEffect <$> runMessage msg attrs
