module Marvel.Upgrade.Upgrades.SkyCycle (
  skyCycle,
  SkyCycle (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Trait (Trait (Aerial, Avenger))

skyCycle :: UpgradeCard SkyCycle
skyCycle = upgrade SkyCycle Cards.skyCycle

newtype SkyCycle = SkyCycle (Attrs Upgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Attach to an Avenger ally. Max 1 per ally.
instance IsUpgrade SkyCycle where
  upgradeTargets _ =
    Just . map toTarget <$> selectList (AllyWithTrait Avenger <> NotAlly (AllyWithUpgrade $ UpgradeIs Cards.skyCycle))

-- | Attached ally gains Aerial.
instance HasModifiersFor SkyCycle where
  getModifiersFor _ (AllyRef aid) (SkyCycle a) | Just aid == upgradeAttachedAlly a = pure [TraitModifier Aerial]
  getModifiersFor _ _ _ = pure []

-- | Action: Exhaust Sky Cycle → ready attached ally.
instance HasAbilities SkyCycle where
  getAbilities a = [ability a 1 Action OwnsThis ExhaustCost $ runAbility a 1]

instance RunMessage SkyCycle where
  runMessage msg u@(SkyCycle attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      for_ (upgradeAttachedAlly attrs) ready
      pure u
    _ -> SkyCycle <$> runMessage msg attrs
