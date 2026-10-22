module Marvel.Treachery.Treacheries.Masterplan (
  masterplan,
  Masterplan (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

masterplan :: TreacheryCard Masterplan
masterplan = treachery Masterplan Cards.masterplan

newtype Masterplan = Masterplan (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Masterplan where
  runMessage msg t@(Masterplan attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery identityId) | ident == treacheryId attrs -> do
      sideSchemes <- selectList AnySideScheme
      if null sideSchemes
        then discardEncounterUntil (CardWithType SideSchemeType) attrs
        else for_ sideSchemes \s -> placeThreat s 4
      pure . Masterplan $ attrs & resolverL ?~ identityId
    WithDiscardedMatch (isTarget attrs -> True) _ (EncounterCard card) -> do
      for_ (treacheryResolver attrs) \identityId -> revealEncounterCard identityId card
      pure t
    _ -> Masterplan <$> runMessage msg attrs
