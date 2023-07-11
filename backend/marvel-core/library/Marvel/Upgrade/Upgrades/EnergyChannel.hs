module Marvel.Upgrade.Upgrades.EnergyChannel (
  energyChannel,
  EnergyChannel (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ability qualified as Ability
import Marvel.Card.Code
import Marvel.Choice
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Resource
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

energyChannel :: UpgradeCard EnergyChannel
energyChannel = upgrade EnergyChannel Cards.energyChannel

newtype EnergyChannel = EnergyChannel (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities EnergyChannel where
  getAbilities a =
    [ ability a 1 Action OwnsThis (DynamicResourceCost $ Just Energy) $
        RunAbility (toTarget a) 1
    , subtype Ability.Attack $
        ability a 2 HeroAction OwnsThis (DiscardCost $ toTarget a) $
          RunAbility (toTarget a) 2
    ]

instance RunMessage EnergyChannel where
  runMessage msg u@(EnergyChannel attrs) = case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ payment -> do
      energyResources <- count (`elem` [Energy, Wild]) <$> paymentResources payment
      push $ UpgradeMessage (upgradeId attrs) $ AddUpgradeUses energyResources
      pure u
    RanAbility ident (isTarget attrs -> True) 2 _ _ -> do
      let amount = min 10 (2 * (upgradeUses attrs))
      pushChoice ident $
        ChooseDamage (toSource attrs) (toDamage amount FromAbility) AnyEnemy
      pure u
    _ -> EnergyChannel <$> runMessage msg attrs
