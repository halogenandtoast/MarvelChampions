module Marvel.Ally.Allies.Rockslide where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Stats
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

rockslide :: AllyCard Rockslide
rockslide = ally Rockslide Cards.rockslide (Thw 1, 1) (Atk 3, 2) (HP 6)

newtype Rockslide = Rockslide (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Rockslide where
  runMessage msg (Rockslide attrs) = runQueueT $ Rockslide <$> runMessage msg attrs
