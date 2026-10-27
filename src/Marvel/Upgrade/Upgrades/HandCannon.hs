module Marvel.Upgrade.Upgrades.HandCannon (
  handCannon,
  handCannonEffect,
  HandCannon (..),
  HandCannonEffect (..),
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
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

handCannon :: UpgradeCard HandCannon
handCannon = upgradeWith HandCannon Cards.handCannon ((usesL .~ 3) . (discardIfNoUsesL .~ True))

newtype HandCannon = HandCannon (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Hero Interrupt: When your hero makes a basic attack, exhaust Hand Cannon and remove
-- 1 charge counter from it → your hero gets +2 ATK for that attack. That attack gains
-- overkill."
instance HasAbilities HandCannon where
  getAbilities a =
    [ limitedWindowAbility a 1 (W.MakesBasicAttack W.When You) HeroInterrupt OwnsThis (ExhaustCost <> UseCost) $
        runAbility a 1
    ]

-- | The overkill is a lasting effect: it stays for that attack when the cannon's last
-- counter has discarded it (RR Uses, Lasting Effects).
instance RunMessage HandCannon where
  runMessage msg u@(HandCannon attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      addToBasicPower ident 2
      createEffect Cards.handCannon attrs (IdentityEntity $ IdentityWithId ident)
      pure u
    _ -> HandCannon <$> runMessage msg attrs

newtype HandCannonEffect = HandCannonEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

handCannonEffect :: CardEffect HandCannonEffect
handCannonEffect =
  effectWith HandCannonEffect Cards.handCannon $
    (modifiersL .~ [KeywordModifier Overkill]) . (endsL ?~ DisableAtEndOfTurn)

-- | Ends with that attack.
instance RunMessage HandCannonEffect where
  runMessage msg e@(HandCannonEffect attrs) = runQueueT $ case msg of
    CheckWindows ws | any madeBasicAttack ws -> e <$ disableEffect (effectId attrs)
    _ -> HandCannonEffect <$> runMessage msg attrs
   where
    madeBasicAttack = \case
      W.Window W.After (W.MadeBasicAttack _) -> True
      _ -> False
