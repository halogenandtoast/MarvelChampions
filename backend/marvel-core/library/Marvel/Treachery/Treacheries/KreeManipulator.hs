module Marvel.Treachery.Treacheries.KreeManipulator (
  kreeManipulator,
  KreeManipulator (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Queue
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

kreeManipulator :: TreacheryCard KreeManipulator
kreeManipulator = treachery KreeManipulator Cards.kreeManipulator

newtype KreeManipulator = KreeManipulator (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage KreeManipulator where
  runMessage msg t@(KreeManipulator attrs) = case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery _ -> do
        mainScheme <- selectJust MainScheme
        case mainScheme of
          SchemeMainSchemeId mainSchemeId -> do
            push $ MainSchemeMessage mainSchemeId $ MainSchemePlaceThreat 1
          _ -> error "Not the main scheme"
        pure t
      _ -> KreeManipulator <$> runMessage msg attrs
    Boost msg' -> case msg' of
      RevealedAsBoost target enemyId | isTarget attrs target -> do
        undefended <-
          member enemyId
            <$> select (VillainEnemy <> UndefendedEnemy)
        when undefended $ do
          mainScheme <- selectJust MainScheme
          case mainScheme of
            SchemeMainSchemeId mainSchemeId -> do
              push $ MainSchemeMessage mainSchemeId $ MainSchemePlaceThreat 1
            _ -> error "Not the main scheme"
        pure t
      _ -> KreeManipulator <$> runMessage msg attrs
    _ -> KreeManipulator <$> runMessage msg attrs
