module Marvel.Ally.Allies.JessicaJones
  ( jessicaJones
  , JessicaJones(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Attrs
import Marvel.Ally.Cards qualified as Cards
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Source
import Marvel.Stats
import Marvel.Target

jessicaJones :: AllyCard JessicaJones
jessicaJones =
  ally JessicaJones Cards.jessicaJones (Thw 1, 1) (Atk 2, 1) (HP 3)

newtype JessicaJones = JessicaJones AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasModifiersFor JessicaJones where
  getModifiersFor _ target (JessicaJones attrs) | isTarget attrs target = do
    sideSchemes <- selectList AnySideScheme
    pure [ThwartModifier . fromIntegral $ length sideSchemes]
  getModifiersFor _ _ _ = pure []

instance HasAbilities JessicaJones where
  getAbilities _ = []

instance RunMessage JessicaJones where
  runMessage msg (JessicaJones attrs) = JessicaJones <$> runMessage msg attrs
