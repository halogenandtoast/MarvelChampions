module Marvel.Upgrade.Upgrades.SuperhumanStrength (
  superhumanStrength,
  SuperhumanStrength (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

superhumanStrength :: UpgradeCard SuperhumanStrength
superhumanStrength = upgrade SuperhumanStrength Cards.superhumanStrength

newtype SuperhumanStrength = SuperhumanStrength (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor SuperhumanStrength where
  getModifiersFor _ (IdentityRef ident) (SuperhumanStrength attrs)
    | ident == upgradeController attrs = pure [AttackModifier 2]
  getModifiersFor _ _ _ = pure []

instance HasAbilities SuperhumanStrength where
  getAbilities (SuperhumanStrength a) =
    [ limitedWindowAbility
        a
        1
        (IdentityAttacked After You AnyEnemy)
        ForcedResponse
        OwnsThis
        (DiscardCost $ toTarget a)
        (RunAbility (toRef a) 1)
    ]

getEnemyTarget :: [WindowType] -> EnemyId
getEnemyTarget [] = error "Wrong window"
getEnemyTarget (IdentityAttack _ eid : _) = eid
getEnemyTarget (_ : xs) = getEnemyTarget xs

instance RunMessage SuperhumanStrength where
  runMessage msg u@(SuperhumanStrength attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 (getEnemyTarget -> enemy) _ -> do
      stun enemy attrs
      pure u
    _ -> SuperhumanStrength <$> runMessage msg attrs
