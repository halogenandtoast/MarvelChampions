module Marvel.Ally.Allies.NickFury (
  nickFury,
  NickFury (..),
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

newtype NickFury = NickFury (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities NickFury where
  getAbilities a =
    [ limitedWindowAbility a 1 (PlayThis After) ForcedResponse OwnsThis NoCost $
        ChooseOneLabelChoice
          [
            ( "$label.remove2ThreatFromAScheme"
            , RemoveThreat (toSource a) 2 ThwartableScheme
            )
          , ("$label.draw3Cards", ChooseDrawCards 3 You)
          ,
            ( "$label.deal4DamageToAnEnemy"
            , ChooseDamage (toSource a) (toDamage 4 FromAbility) AnyEnemy
            )
          ]
    , limitedWindowAbility a 2 RoundEnds ForcedResponse OwnsThis NoCost $
        TargetLabel (toTarget a) [DiscardTarget $ toTarget a]
    ]

instance RunMessage NickFury where
  runMessage msg (NickFury attrs) = NickFury <$> runMessage msg attrs
