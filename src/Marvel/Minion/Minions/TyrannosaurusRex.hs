module Marvel.Minion.Minions.TyrannosaurusRex (
  tyrannosaurusRex,
  TyrannosaurusRex (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Keyword
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

tyrannosaurusRex :: MinionCard TyrannosaurusRex
tyrannosaurusRex = minion TyrannosaurusRex Cards.tyrannosaurusRex (Sch 0) (Atk 3) (HP 6)

newtype TyrannosaurusRex = TyrannosaurusRex (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Toughness. [star] Its attacks gain piercing.
instance HasModifiersFor TyrannosaurusRex where
  getModifiersFor _ (MinionRef mid) (TyrannosaurusRex a) | mid == minionId a = pure [KeywordModifier Piercing]
  getModifiersFor _ _ _ = pure []

instance RunMessage TyrannosaurusRex where
  runMessage msg (TyrannosaurusRex attrs) = TyrannosaurusRex <$> runMessage msg attrs
