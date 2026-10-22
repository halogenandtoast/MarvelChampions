module Marvel.Treachery.Treacheries.PastMachinations (
  pastMachinations,
  PastMachinations (..),
) where

import Marvel.Prelude

import Data.Function (on)
import Data.List (nubBy)
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Message.Lifted
import Marvel.Name
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

pastMachinations :: TreacheryCard PastMachinations
pastMachinations = treachery PastMachinations Cards.pastMachinations

newtype PastMachinations = PastMachinations (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Incite 1. When Revealed: each player searches the encounter deck and discard pile
-- for a different obligation and reveals it. Shuffle the encounter deck. Each player in
-- turn, with the obligations the others found before them out of their search.
instance RunMessage PastMachinations where
  runMessage msg t@(PastMachinations attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> do
      players <- getPlayers
      cardSpecific attrs "search" (players, [] :: [Text])
      pure t
    CardSpecific (isTarget attrs -> True) "search" (fromJSON -> Success (players :: [IdentityId], found :: [Text])) -> case players of
      [] -> t <$ shuffleEncounterDeck
      p : rest -> do
        cards <- (<>) <$> getEncounterDeck <*> getEncounterDiscard
        let obligations =
              nubBy ((==) `on` titleOf) $
                filter (\c -> cdCardType (getCardDef c) == ObligationType && titleOf c `notElem` found) cards
        if null obligations
          then cardSpecific attrs "search" (rest, found)
          else
            chooseOrRunOne p
              [ targeting c do
                  revealEncounterCard p c
                  cardSpecific attrs "search" (rest, titleOf c : found)
              | c <- obligations
              ]
        pure t
    _ -> PastMachinations <$> runMessage msg attrs
   where
    titleOf = title . cdName . getCardDef
