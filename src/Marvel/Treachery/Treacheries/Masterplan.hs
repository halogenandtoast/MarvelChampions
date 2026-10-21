module Marvel.Treachery.Treacheries.Masterplan (
  masterplan,
  Masterplan (..),
) where

import Marvel.Prelude

import Marvel.Id (SchemeId (..))

import Marvel.Card
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Queue
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

masterplan :: TreacheryCard Masterplan
masterplan = treachery Masterplan Cards.masterplan

newtype Masterplan = Masterplan (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Masterplan where
  runMessage msg t@(Masterplan attrs) = case msg of
    TreacheryMessage ident (RevealTreachery identityId) | ident == treacheryId attrs -> do
      sideSchemes <- selectList AnySideScheme
      pushAll $
        if null sideSchemes
          then [DiscardUntil FromEncounterDeck (CardWithType SideSchemeType) (toTarget attrs)]
          else concatMap (\s -> placeThreatOn (SchemeSideSchemeId s) 4) sideSchemes
      pure . Masterplan $ attrs & resolverL ?~ identityId
    WithDiscardedMatch (isTarget attrs -> True) _ (EncounterCard card) -> do
      for_ (treacheryResolver attrs) \identityId -> push $ RevealEncounterCard identityId card
      pure t
    _ -> Masterplan <$> runMessage msg attrs
