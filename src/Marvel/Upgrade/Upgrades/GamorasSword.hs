module Marvel.Upgrade.Upgrades.GamorasSword (
  gamorasSword,
  GamorasSword (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

gamorasSword :: UpgradeCard GamorasSword
gamorasSword = upgrade GamorasSword Cards.gamorasSword

newtype GamorasSword = GamorasSword (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Response: after you play an Attack event, deal 1 damage to an enemy.
instance HasAbilities GamorasSword where
  getAbilities a@(GamorasSword attrs) =
    [ limitedWindowAbility
        a
        1
        ( W.PlaysEvent W.After (IdentityWithId $ upgradeController attrs) $
            CardWithType EventType <> CardWithTrait Trait.Attack
        )
        Response
        (OwnsThis <> EnemyExists DamageableEnemy)
        NoCost
        (ChooseDamage (toSource a) (toDamage 1 FromAbility) AnyEnemy)
    ]

instance RunMessage GamorasSword where
  runMessage msg (GamorasSword attrs) = GamorasSword <$> runMessage msg attrs
