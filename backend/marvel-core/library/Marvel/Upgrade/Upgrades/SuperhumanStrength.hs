module Marvel.Upgrade.Upgrades.SuperhumanStrength
  ( superhumanStrength
  , SuperhumanStrength(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Upgrade.Types
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Window

superhumanStrength :: UpgradeCard SuperhumanStrength
superhumanStrength = upgrade SuperhumanStrength Cards.superhumanStrength

newtype SuperhumanStrength = SuperhumanStrength UpgradeAttrs
  deriving anyclass IsUpgrade
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasModifiersFor SuperhumanStrength where
  getModifiersFor _ (IdentityTarget ident) (SuperhumanStrength attrs)
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
        NoCost
        (RunAbility (toTarget a) 1)
    ]

getEnemyTarget :: [WindowType] -> Target
getEnemyTarget [] = error "Wrong window"
getEnemyTarget (IdentityAttack _ eid : _) = EnemyTarget eid
getEnemyTarget (_ : xs) = getEnemyTarget xs

instance RunMessage SuperhumanStrength where
  runMessage msg u@(SuperhumanStrength attrs) = case msg of
    RanAbility target 1 windows | isTarget attrs target -> do
      let enemyTarget = getEnemyTarget windows
      stunMsgs <- choiceMessages (upgradeController attrs)
        $ Stun enemyTarget (toSource attrs)
      pushAll $ RemoveFromPlay target : stunMsgs
      pure u
    _ -> SuperhumanStrength <$> runMessage msg attrs
