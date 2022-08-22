module Marvel.Ally.Allies.NickFury
  ( nickFury
  , NickFury(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Window

nickFury :: AllyCard NickFury
nickFury = ally NickFury Cards.nickFury (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype NickFury = NickFury AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, IsSource, IsTarget)

instance HasAbilities NickFury where
  getAbilities a =
    [ limitedWindowAbility a 1 (PlayThis After) ForcedResponse OwnsThis NoCost
      $ ChooseOneLabelChoice
          [ ( "Remove 2 threat from a scheme"
            , RemoveThreat (toSource a) 2 ThwartableScheme
            )
          , ("Draw 3 cards", ChooseDrawCards 3 You)
          , ( "Deal 4 damage to an enemy"
            , ChooseDamage (toSource a) (toDamage 4 FromAbility) AnyEnemy
            )
          ]
    , limitedWindowAbility a 2 RoundEnds ForcedResponse OwnsThis NoCost
      $ TargetLabel (toTarget a) [DiscardTarget $ toTarget a]
    ]

instance RunMessage NickFury where
  runMessage msg a = NickFury <$> runMessage msg (toAttrs a)
