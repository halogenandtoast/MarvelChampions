module Marvel.MainScheme.MainSchemes.SecretRendezvous where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref

secretRendezvous :: MainSchemeCard SecretRendezvous
secretRendezvous =
  mainScheme SecretRendezvous Cards.secretRendezvous (PerPlayer 8) (Static 0) (PerPlayer 1)

newtype SecretRendezvous = SecretRendezvous (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage SecretRendezvous where
  runMessage msg ms@(SecretRendezvous attrs) = runQueueT $ case msg of
    MainSchemeMessage ident msg' | ident == mainSchemeId attrs ->
      case msg' of
        RevealMainScheme -> ms <$ discardUntil FromEncounterDeck (CardWithType MinionType) attrs
        _ -> SecretRendezvous <$> runMessage msg attrs
    AdvanceMainScheme ((== mainSchemeId attrs) -> True) -> ms <$ gameOver Lost
    WithDiscardedMatch (isTarget attrs -> True) _ (EncounterCard card) -> do
      firstPlayer <- selectJust FirstPlayer
      ms <$ putEncounterCardIntoPlay firstPlayer card
    _ -> SecretRendezvous <$> runMessage msg attrs
