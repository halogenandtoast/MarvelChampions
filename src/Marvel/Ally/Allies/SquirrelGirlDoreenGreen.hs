module Marvel.Ally.Allies.SquirrelGirlDoreenGreen where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Window

squirrelGirlDoreenGreen :: AllyCard SquirrelGirlDoreenGreen
squirrelGirlDoreenGreen = ally SquirrelGirlDoreenGreen Cards.squirrelGirlDoreenGreen (Thw 1, 1) (Atk 1, 1) (HP 2)

instance HasAbilities SquirrelGirlDoreenGreen where
  getAbilities a =
    [ limitedWindowAbility a 1 (PlayThis After) Response OwnsThis NoCost $
        DamageAllEnemies AnyEnemy (toSource a) (toDamage 1 FromAbility)
    ]

newtype SquirrelGirlDoreenGreen = SquirrelGirlDoreenGreen (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage SquirrelGirlDoreenGreen where
  runMessage msg (SquirrelGirlDoreenGreen attrs) = SquirrelGirlDoreenGreen <$> runMessage msg attrs
