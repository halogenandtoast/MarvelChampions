module Marvel.AlterEgo.AlterEgos.FlashThompson (
  flashThompson,
  FlashThompson (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.Card
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.GameValue
import Marvel.Hand
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Ref
import Marvel.Question
import Marvel.Stats
import Marvel.Trait (Trait (Weapon))

flashThompson :: AlterEgoCard FlashThompson
flashThompson =
  alterEgo
    FlashThompson
    Cards.flashThompson
    (HP $ Static 12)
    (HandSize 6)
    (Rec 4)
    [Cards.struggleForControl]

newtype FlashThompson = FlashThompson (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

-- | "You can control 1 additional upgrade that has the restricted keyword."
instance HasModifiersFor FlashThompson where
  getModifiersFor _ target (FlashThompson a) | isTarget a target = pure [RestrictedLimitModifier 1]
  getModifiersFor _ _ _ = pure []

-- | Armed and Ready — Setup: discard cards from the top of your deck until you discard a
-- Weapon upgrade, then add that card to your hand. With none, the whole deck is
-- discarded and nothing added (hero-venom.md).
instance HasAbilities FlashThompson where
  getAbilities a = [label "Armed and Ready" $ ability a 1 Setup NoCriteria NoCost $ runAbility a 1]

instance RunMessage FlashThompson where
  runMessage msg ae@(FlashThompson attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      discardFromDeckUntil ident (CardWithType UpgradeType <> CardWithTrait Weapon) attrs
      pure ae
    WithDiscardedMatch (isTarget attrs -> True) _ (PlayerCard card) -> do
      addToHand (alterEgoIdentityId attrs) card
      pure ae
    _ -> FlashThompson <$> runMessage msg attrs
