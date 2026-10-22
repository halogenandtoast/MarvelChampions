module Marvel.Upgrade.Upgrades.ElectrostaticArmor (
  electrostaticArmor,
  ElectrostaticArmor (..),
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
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Choice
import Marvel.Damage
import Marvel.Window

electrostaticArmor :: UpgradeCard ElectrostaticArmor
electrostaticArmor = upgrade ElectrostaticArmor Cards.electrostaticArmor

newtype ElectrostaticArmor = ElectrostaticArmor (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Response: After you defend against an attack, deal 1 damage to the attacking
-- character."
instance HasAbilities ElectrostaticArmor where
  getAbilities (ElectrostaticArmor a) =
    [ limitedWindowAbility
        a
        1
        (HeroDefended After (IdentityWithId $ upgradeController a) AnyEnemy)
        Response
        OwnsThis
        NoCost
        (runAbility a 1)
    ]

attacker :: [WindowType] -> Maybe Target
attacker = \case
  HeroDefends _ enemy : _ -> Just (toRef enemy)
  _ : ws -> attacker ws
  [] -> Nothing

instance RunMessage ElectrostaticArmor where
  runMessage msg u@(ElectrostaticArmor attrs) = case msg of
    RanAbility ident (isTarget attrs -> True) 1 (attacker -> Just enemy) _ -> do
      pushChoice ident $ DamageEnemy enemy (toSource attrs) (toDamage 1 FromAbility)
      pure u
    _ -> ElectrostaticArmor <$> runMessage msg attrs
