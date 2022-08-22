module Marvel.Ally.Allies.TigraGreerGrantNelson
  ( tigraGreerGrantNelson
  , TigraGreerGrantNelson(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.GameValue
import Marvel.Id
import Marvel.Matchers
import Marvel.Window

tigraGreerGrantNelson :: AllyCard TigraGreerGrantNelson
tigraGreerGrantNelson = ally
  TigraGreerGrantNelson
  Cards.tigraGreerGrantNelson
  (Thw 1, 1)
  (Atk 2, 1)
  (HP 3)

newtype TigraGreerGrantNelson = TigraGreerGrantNelson AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, IsSource, IsTarget)

instance HasAbilities TigraGreerGrantNelson where
  getAbilities a =
    [ limitedWindowAbility
          a
          1
          (EnemyDefeated After MinionEnemy . AttackFromAlly $ AllyWithId
            (toId a)
          )
          Response
          (AllyExists $ AllyWithDamage (GreaterThan $ Static 0) <> AllyWithId
            (toId a)
          )
          NoCost
        $ Heal (AllyCharacter $ toId a) 1
    ]

instance RunMessage TigraGreerGrantNelson where
  runMessage msg a = TigraGreerGrantNelson <$> runMessage msg (toAttrs a)
