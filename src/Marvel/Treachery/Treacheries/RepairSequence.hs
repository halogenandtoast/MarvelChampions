module Marvel.Treachery.Treacheries.RepairSequence (
  repairSequence,
  RepairSequence (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Trait
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

repairSequence :: TreacheryCard RepairSequence
repairSequence = treachery RepairSequence Cards.repairSequence

newtype RepairSequence = RepairSequence (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

dronesEngagedWith :: (HasGame m) => IdentityId -> m Natural
dronesEngagedWith ident =
  selectListCount $ MinionWithTrait Drone <> MinionEngagedWith (IdentityWithId ident)

instance RunMessage RepairSequence where
  runMessage msg t@(RepairSequence attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery identityId) | ident == treacheryId attrs -> do
      drones <- dronesEngagedWith identityId
      villainId <- selectJust ActiveVillain
      damaged <- villainMatches VillainWithAnyDamage villainId
      if drones > 0 && damaged
        then do
          heal villainId (2 * drones)
          pure t
        else pure . RepairSequence $ attrs & surgeL .~ True
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      drones <- dronesEngagedWith =<< getActivePlayerId
      -- with no villain in play (as between one Kang and the next), nothing
      selectOne ActiveVillain >>= \case
        Nothing -> pure t
        Just villainId -> do
          when (drones > 0) $ heal villainId drones
          pure t
    _ -> RepairSequence <$> runMessage msg attrs
