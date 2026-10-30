module Marvel.Treachery.Treacheries.SmCampaign where

import Marvel.Prelude
import Marvel.Card
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

newtype SmCampaignTreachery = SmCampaignTreachery (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smSmearCampaign :: TreacheryCard SmCampaignTreachery
smSmearCampaign = treachery SmCampaignTreachery Cards.smSmearCampaign

instance RunMessage SmCampaignTreachery where
  runMessage msg t@(SmCampaignTreachery attrs) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId attrs -> do
      expert <- (== Expert) <$> getDifficulty
      public <- selectList (EnvironmentWithTitle "Public Outcry")
      case public of
        eid : _ -> do
          placeCounters eid 2
          removeFromGame attrs
          -- Removing the physical card does not cancel its surge keyword.
          when expert $ surge you
        [] -> do
          schemes <- selectList EncounterMainScheme
          unless (null schemes) $ chooseOrRunOne you [targeting (schemeRef s) (placeThreat s 2) | s <- schemes]
          when expert $ gainSurge attrs
      pure t
    _ -> SmCampaignTreachery <$> runMessage msg attrs
