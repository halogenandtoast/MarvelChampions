module Marvel.Ally.Allies.NickFury
  ( nickFury
  , NickFury(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Attrs
import Marvel.Ally.Cards qualified as Cards
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Window

nickFury :: AllyCard NickFury
nickFury = ally NickFury Cards.nickFury (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype NickFury = NickFury AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities NickFury where
  getAbilities (NickFury a) =
    [ limitedWindowAbility
      a
      1
      (PlayThis After)
      ForcedResponse
      OwnsThis
      NoCost
      (ChooseOneLabelChoice
        [ ("Remove 2 threat from a scheme", RemoveThreat (toSource a) 2 ThwartableScheme)
        , ("Draw 3 cards", ChooseDrawCards 3 You)
        , ("Deal 4 damage to an enemy", ChooseDamage (toSource a) (toDamage 4 FromAbility) AnyEnemy)
        ]
      )
    , limitedWindowAbility
      a
      2
      RoundEnds
      ForcedResponse
      OwnsThis
      NoCost
      (TargetLabel (toTarget a) [DiscardTarget $ toTarget a])
    ]

instance RunMessage NickFury where
  runMessage msg (NickFury attrs) = NickFury <$> runMessage msg attrs
