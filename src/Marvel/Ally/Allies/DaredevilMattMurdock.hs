module Marvel.Ally.Allies.DaredevilMattMurdock (
  daredevilMattMurdock,
  DaredevilMattMurdock (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Window qualified as W

daredevilMattMurdock :: AllyCard DaredevilMattMurdock
daredevilMattMurdock =
  ally
    DaredevilMattMurdock
    Cards.daredevilMattMurdock
    (Thw 2, 1)
    (Atk 2, 1)
    (HP 3)

newtype DaredevilMattMurdock = DaredevilMattMurdock (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities DaredevilMattMurdock where
  getAbilities (DaredevilMattMurdock a) =
    [ limitedWindowAbility
        a
        1
        (W.AllyThwarted W.After (AllyWithId $ allyId a) AnyScheme)
        Response
        OwnsThis
        NoCost
        $ ChooseDamage (toRef a) (toDamage 1 FromAbility) AnyEnemy
    ]

instance RunMessage DaredevilMattMurdock where
  runMessage msg (DaredevilMattMurdock a) =
    DaredevilMattMurdock <$> runMessage msg a
