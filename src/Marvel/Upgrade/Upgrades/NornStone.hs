-- | Norn Stone (The Mad Titan's Shadow campaign set): Find the Norn Stones defeated, or
-- the campaign pool, puts a copy into play under each player's control on its Setup side.
module Marvel.Upgrade.Upgrades.NornStone (
  nornStoneSetup,
  nornStone,
  NornStone (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

nornStoneSetup :: UpgradeCard NornStone
nornStoneSetup = upgrade NornStone Cards.nornStoneSetup

nornStone :: UpgradeCard NornStone
nornStone = upgrade NornStone Cards.nornStone

-- | Both sides: Permanent. Your hero gets +1 THW, +1 ATK, and +1 DEF (an alter-ego has
-- none of them).
newtype NornStone = NornStone (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

isSetupSide :: Attrs Upgrade -> Bool
isSetupSide a = toCardCode a == toCardCode Cards.nornStoneSetup

instance HasModifiersFor NornStone where
  getModifiersFor _ (IdentityRef iid) (NornStone a)
    | iid == upgradeController a = pure [ThwartModifier 1, AttackModifier 1, DefenseModifier 1]
  getModifiersFor _ _ _ = pure []

-- | Setup side, Hero Action: ready your hero. Flip this card over. The other side,
-- Alter-Ego Action: exhaust Norn Stone to heal 1 damage from your identity.
instance HasAbilities NornStone where
  getAbilities (NornStone a)
    | isSetupSide a = [ability a 1 HeroAction OwnsThis NoCost (runAbility a 1)]
    | otherwise = [ability a 2 AlterEgoAction (OwnsThis <> SelfMatches healableIdentity) ExhaustCost (runAbility a 2)]

-- | Flipped in place, the same card and type: exhausted or not, it stays as it is (RR
-- Flip).
instance RunMessage NornStone where
  runMessage msg u@(NornStone a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      ready (upgradeController a)
      pure $ NornStone a {upgradeCardDef = Cards.nornStone}
    RanAbility _ (isTarget a -> True) 2 _ _ -> u <$ heal (upgradeController a) 1
    _ -> NornStone <$> runMessage msg a
