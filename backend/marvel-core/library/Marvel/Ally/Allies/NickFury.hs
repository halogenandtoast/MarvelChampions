module Marvel.Ally.Allies.NickFury
  ( nickFury
  , NickFury(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Attrs
import qualified Marvel.Ally.Cards as Cards
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message
import Marvel.Question
import Marvel.Source
import Marvel.Stats
import Marvel.Target
import Marvel.Window

nickFury :: AllyCard NickFury
nickFury = ally NickFury Cards.nickFury (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype NickFury = NickFury AllyAttrs
  deriving anyclass IsAlly
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
        (RunAbility (toTarget a) 1)
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
  runMessage msg a@(NickFury attrs) = case msg of
    RanAbility target 1 _ | isTarget a target -> do
      chooseOne
        (allyController attrs)
        [ Label
          "Remove 2 threat from a scheme"
          [RemoveThreat (toSource a) 2 AnyScheme]
        , Label "Draw 3 cards" [YouDrawCards 3]
        , Label
          "Deal 4 damage to an enemy"
          [ChooseDamage (toSource a) 4 AnyEnemy]
        ]
      pure a
    _ -> NickFury <$> runMessage msg (toAttrs a)
