module Marvel.Ally.Allies.LukeCage
  ( lukeCage
  , LukeCage(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Attrs
import qualified Marvel.Ally.Cards as Cards
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Hp
import Marvel.Message
import Marvel.Modifier
import Marvel.Source
import Marvel.Stats
import Marvel.Target

lukeCage :: AllyCard LukeCage
lukeCage =
  allyWith LukeCage Cards.lukeCage (Thw 1, 1) (Atk 2, 1) (HP 5) (toughL .~ True)

newtype LukeCage = LukeCage AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasAbilities LukeCage where
  getAbilities _ = []

instance RunMessage LukeCage where
  runMessage msg (LukeCage attrs) = LukeCage <$> runMessage msg attrs
