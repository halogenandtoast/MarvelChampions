-- | The treachery of The Mad Titan's Shadow's campaign set.
module Marvel.Treachery.Treacheries.MadTitansShadowCampaign (
  summonedBack,
  SummonedBack (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

summonedBack :: TreacheryCard SummonedBack
summonedBack = treachery SummonedBack Cards.summonedBack

newtype SummonedBack = SummonedBack (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Peril. Surge (the def). When Revealed: search the encounter deck, discard pile, and
-- set-aside area for your nemesis minion and put it into play engaged with you. Shuffle
-- the encounter deck.
instance RunMessage SummonedBack where
  runMessage msg t@(SummonedBack attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      minions <- getNemesisMinionsOutOfPlay you
      for_ (take 1 minions) (putEncounterCardIntoPlay you)
      shuffleEncounterDeck
      pure t
    _ -> SummonedBack <$> runMessage msg attrs
