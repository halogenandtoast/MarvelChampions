module Marvel.Upgrade.Upgrades.HugeWoodenHammer (
  hugeWoodenHammer,
  hugeWoodenHammerEffect,
  HugeWoodenHammer (..),
  HugeWoodenHammerEffect (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

hugeWoodenHammer :: UpgradeCard HugeWoodenHammer
hugeWoodenHammer = upgrade HugeWoodenHammer Cards.hugeWoodenHammer

newtype HugeWoodenHammer = HugeWoodenHammer (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Hero Interrupt: When your hero makes a basic attack, exhaust Hand Cannon and remove
-- 1 charge counter from it → your hero gets +2 ATK for that attack. That attack gains
-- overkill."
instance HasModifiersFor HugeWoodenHammer where
  getModifiersFor _ (IdentityRef you) (HugeWoodenHammer attrs)
    | you == upgradeController attrs = do
      heroForm <- identityMatches HeroIdentity you
      pure [AttackModifier 1 | heroForm]
  getModifiersFor _ _ _ = pure []

instance HasAbilities HugeWoodenHammer where
  getAbilities a =
    [ limitedWindowAbility a 1 (W.MakesBasicAttack W.When You) HeroInterrupt OwnsThis (ExhaustCost <> IdentityCountersCost 1) $
        runAbility a 1
    ]

-- | The overkill is a lasting effect: it stays for that attack when the cannon's last
-- counter has discarded it (RR Uses, Lasting Effects).
instance RunMessage HugeWoodenHammer where
  runMessage msg u@(HugeWoodenHammer attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      addToBasicPower ident 2
      createEffect Cards.hugeWoodenHammer attrs (IdentityEntity $ IdentityWithId ident)
      pure u
    _ -> HugeWoodenHammer <$> runMessage msg attrs

newtype HugeWoodenHammerEffect = HugeWoodenHammerEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

hugeWoodenHammerEffect :: CardEffect HugeWoodenHammerEffect
hugeWoodenHammerEffect =
  effectWith HugeWoodenHammerEffect Cards.hugeWoodenHammer $
    (modifiersL .~ [KeywordModifier Overkill]) . (endsL ?~ DisableAtEndOfTurn)

-- | Ends with that attack.
instance RunMessage HugeWoodenHammerEffect where
  runMessage msg e@(HugeWoodenHammerEffect attrs) = runQueueT $ case msg of
    CheckWindows ws | any madeBasicAttack ws -> e <$ disableEffect (effectId attrs)
    _ -> HugeWoodenHammerEffect <$> runMessage msg attrs
   where
    madeBasicAttack = \case
      W.Window W.After (W.MadeBasicAttack _) -> True
      _ -> False
