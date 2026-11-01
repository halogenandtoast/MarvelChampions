module Marvel.Ally.Allies.WhiteFox where
import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

whiteFox :: AllyCard WhiteFox
whiteFox = ally WhiteFox Cards.whiteFox (Thw 1, 1) (Atk 1, 1) (HP 3)
newtype WhiteFox = WhiteFox (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage WhiteFox where
  runMessage msg (WhiteFox attrs) = WhiteFox <$> runMessage msg attrs
