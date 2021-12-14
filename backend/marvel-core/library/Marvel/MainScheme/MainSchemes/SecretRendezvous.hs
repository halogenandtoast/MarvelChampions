module Marvel.MainScheme.MainSchemes.SecretRendezvous where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.GameValue
import Marvel.MainScheme.Attrs
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Queue
import Marvel.Source
import Marvel.Target

secretRendezvous :: MainSchemeCard SecretRendezvous
secretRendezvous =
  mainScheme SecretRendezvous Cards.secretRendezvous (PerPlayer 8) (Static 0) (PerPlayer 1)

newtype SecretRendezvous = SecretRendezvous MainSchemeAttrs
  deriving anyclass IsMainScheme
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage SecretRendezvous where
  runMessage msg ms@(SecretRendezvous attrs) = case msg of
    MainSchemeMessage mainSchemeId msg' | mainSchemeId == toId attrs ->
      case msg' of
        RevealMainScheme -> ms <$ push (DiscardUntil FromEncounterDeck (CardWithType MinionType) (Just $ toTarget attrs))
        _ -> SecretRendezvous <$> runMessage msg attrs
    AdvanceMainScheme -> ms <$ push (GameOver Lost)
    WithDiscardedMatch (isTarget attrs -> True) _ (EncounterCard card) -> do
      firstPlayer <- selectJust FirstPlayer
      ms <$ push (RevealEncounterCard firstPlayer card)
    _ -> SecretRendezvous <$> runMessage msg attrs
