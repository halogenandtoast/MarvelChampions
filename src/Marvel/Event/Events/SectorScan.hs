module Marvel.Event.Events.SectorScan where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Id (CardId (..))
import Marvel.Id (EventId (..))
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Effect.Types
import Marvel.Identity (ironheartVersion)
import Marvel.Matchers

sectorScan :: EventCard SectorScan
sectorScan = event SectorScan Cards.sectorScan

newtype SectorScan = SectorScan (Attrs Event)
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor SectorScan where
  getModifiersFor _ (CardIdRef cid) (SectorScan a) | cid == CardId (unEventId $ eventId a) = do
    version <- ironheartVersion (eventController a)
    pure [ResourceCostReduction version]
  getModifiersFor _ _ _ = pure []

instance RunMessage SectorScan where
  runMessage msg e@(SectorScan attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      createEffect Cards.sectorScan attrs (IdentityEntity $ IdentityWithId you)
      pure e
    _ -> SectorScan <$> runMessage msg attrs

newtype SectorScanEffect = SectorScanEffect (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

sectorScanEffect :: CardEffect SectorScanEffect
sectorScanEffect = effectWith SectorScanEffect Cards.sectorScan $
  (modifiersL .~ [CanLookAtTopOfEncounterDeck]) . (endsL ?~ DisableAtEndOfRound)

instance RunMessage SectorScanEffect where
  runMessage msg (SectorScanEffect attrs) = SectorScanEffect <$> runMessage msg attrs
