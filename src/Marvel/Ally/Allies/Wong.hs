module Marvel.Ally.Allies.Wong (
  wong,
  Wong (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Id
import Marvel.Invocation

wong :: AllyCard Wong
wong = ally Wong Cards.wong (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype Wong = Wong (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasAbilities Wong where
  getAbilities (Wong a) =
    [ ability a 1 Action OwnsThis ExhaustCost $
        ChooseOneLabelChoice
          [ ((countLabel "healYourIdentity" 1), Heal (IdentityCharacter you) 1)
          , ("$label.discardTopInvocation", Run [invocation you DiscardTopInvocation])
          ]
    ]
   where
    you = allyController a

instance RunMessage Wong where
  runMessage msg (Wong a) = Wong <$> runMessage msg a
