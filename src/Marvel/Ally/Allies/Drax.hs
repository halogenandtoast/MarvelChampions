module Marvel.Ally.Allies.Drax (
  drax,
  Drax (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Modifier
import Marvel.Ref

drax :: AllyCard Drax
drax = ally Drax Cards.drax (Thw 1, 1) (Atk 3, 1) (HP 4)

newtype Drax = Drax (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "Drax cannot attack minions." "Play only if your identity has the Guardian trait"
-- is the def's criteria.
instance HasModifiersFor Drax where
  getModifiersFor _ (AllyRef aid) (Drax a)
    | aid == allyId a = pure [CannotAttackMinions]
  getModifiersFor _ _ _ = pure []

instance RunMessage Drax where
  runMessage msg (Drax attrs) = Drax <$> runMessage msg attrs
