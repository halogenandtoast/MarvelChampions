module Marvel.Upgrade.Upgrades.WidowsBite (
  widowsBite,
  WidowsBite (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ability qualified as Ability
import Marvel.Card
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
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

widowsBite :: UpgradeCard WidowsBite
widowsBite = upgrade WidowsBite Cards.widowsBite

newtype WidowsBite = WidowsBite (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities WidowsBite where
  getAbilities a =
    [ subtype Ability.Attack $
        limitedWindowAbility
          a
          1
          (MinionEntersPlay After AnyMinion)
          HeroResponse
          OwnsThis
          (DiscardCost $ toTarget a)
          (runAbility a 1)
    ]

instance RunMessage WidowsBite where
  runMessage msg u@(WidowsBite attrs) = case msg of
    RanAbility ident (isTarget attrs -> True) 1 [MinionEnteredPlay minion] _ ->
      attackGuard ident u do
        pushAll
          [ MinionMessage minion $ MinionDamaged (toSource attrs) (toDamage 2 $ FromPlayerAttack ident)
          , MinionMessage minion $ MinionStunned (toSource attrs)
          ]
        pure u
    _ -> WidowsBite <$> runMessage msg attrs
