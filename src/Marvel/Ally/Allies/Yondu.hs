module Marvel.Ally.Allies.Yondu (
  yondu,
  Yondu (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

yondu :: AllyCard Yondu
yondu = ally Yondu Cards.yondu (Thw 2, 1) (Atk 1, 0) (HP 2)

-- | "[star] Yondu's attacks gain ranged": the keyword on his card def.
newtype Yondu = Yondu (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Yondu where
  runMessage msg (Yondu attrs) = Yondu <$> runMessage msg attrs
