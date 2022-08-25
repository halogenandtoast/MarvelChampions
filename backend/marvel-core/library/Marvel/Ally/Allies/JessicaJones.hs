module Marvel.Ally.Allies.JessicaJones
  ( jessicaJones
  , JessicaJones(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Matchers
import Marvel.Query

jessicaJones :: AllyCard JessicaJones
jessicaJones =
  ally JessicaJones Cards.jessicaJones (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype JessicaJones = JessicaJones (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsSource, IsTarget)

instance HasModifiersFor JessicaJones where
  getModifiersFor _ target (JessicaJones attrs) | isTarget attrs target = do
    modifier <- selectListCount AnySideScheme
    pure [ThwartModifier modifier]
  getModifiersFor _ _ _ = pure []

instance RunMessage JessicaJones where
  runMessage msg (JessicaJones attrs) = JessicaJones <$> runMessage msg attrs
