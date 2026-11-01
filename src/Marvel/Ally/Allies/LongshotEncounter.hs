module Marvel.Ally.Allies.LongshotEncounter where

import Marvel.Prelude
import Marvel.Ally.Runner
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ability
import Marvel.Card
import Marvel.Entity
import Marvel.Id
import Marvel.Keyword
import Marvel.Modifier
import Marvel.Ref

newtype LongshotEncounter = LongshotEncounter (Attrs Ally)
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mojo39071 :: AllyCard LongshotEncounter
mojo39071 = ally encounterAlly Cards.mojo39071 (Thw 2, 1) (Atk 2, 1) (HP 3)
 where
  encounterAlly attrs = LongshotEncounter attrs
    { allyOriginalCard = Just $ EncounterCard $
        MkEncounterCard (CardId $ unAllyId $ allyId attrs) Cards.mojo39071
    }

instance HasModifiersFor LongshotEncounter where
  getModifiersFor _ target (LongshotEncounter attrs) = pure
    [modifier | target == toRef attrs, modifier <- [NotCountedAgainstAllyLimit, KeywordModifier Piercing]]

instance RunMessage LongshotEncounter where
  runMessage msg (LongshotEncounter attrs) = LongshotEncounter <$> runMessage msg attrs
