module Marvel.Treachery.Treacheries.Misled where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question ()
import Marvel.Ref
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as Trait ()
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

misled :: TreacheryCard Misled
misled = treachery Misled Cards.misled

newtype Misled = Misled (Attrs Treachery)
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Misled where
  runMessage msg t@(Misled attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      let card = MkEncounterCard (CardId $ unTreacheryId tid) (getCardDef attrs)
      removeFromGame attrs
      shuffleIntoDeck you [encounterCardInPlayerDeck you card]
      surge you
      pure t
    _ -> Misled <$> runMessage msg attrs
