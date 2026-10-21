module Marvel.Upgrade.Upgrades.PoweredGauntlets (
  poweredGauntlets,
  PoweredGauntlets (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Choice
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

poweredGauntlets :: UpgradeCard PoweredGauntlets
poweredGauntlets = upgrade PoweredGauntlets Cards.poweredGauntlets

newtype PoweredGauntlets = PoweredGauntlets (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PoweredGauntlets where
  getAbilities (PoweredGauntlets a) =
    [ ability
        a
        1
        HeroAction
        (OwnsThis <> EnemyExists AttackableEnemy)
        ExhaustCost
        $ RunAbility (toTarget a) 1
    ]

instance RunMessage PoweredGauntlets where
  runMessage msg u@(PoweredGauntlets attrs) = case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      aerial <-
        selectAny
          (IdentityWithId ident <> IdentityWithTrait Aerial)
      let
        dmg = if aerial then 2 else 1
      enemies <- selectList AttackableEnemy
      chooseOne ident $
        map
          (damageChoice attrs (toDamage dmg $ FromPlayerAttack ident))
          enemies
      pure u
    _ -> PoweredGauntlets <$> runMessage msg attrs
