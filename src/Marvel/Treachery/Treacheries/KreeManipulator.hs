module Marvel.Treachery.Treacheries.KreeManipulator (
  kreeManipulator,
  KreeManipulator (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

kreeManipulator :: TreacheryCard KreeManipulator
kreeManipulator = treachery KreeManipulator Cards.kreeManipulator

newtype KreeManipulator = KreeManipulator (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage KreeManipulator where
  runMessage msg t@(KreeManipulator attrs) = runQueueT $ case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery _ -> do
        placeThreatOnEach attrs 1 EncounterMainScheme
        pure t
      _ -> KreeManipulator <$> runMessage msg attrs
    Boost msg' -> case msg' of
      RevealedAsBoost target enemyId | isTarget attrs target -> do
        undefended <-
          member enemyId
            <$> select (VillainEnemy <> UndefendedEnemy)
        when undefended $ placeThreatOnEach attrs 1 EncounterMainScheme
        pure t
      _ -> KreeManipulator <$> runMessage msg attrs
    _ -> KreeManipulator <$> runMessage msg attrs
