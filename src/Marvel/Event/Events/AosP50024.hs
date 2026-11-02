{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Event.Events.AosP50024 where

import Marvel.Prelude
import Marvel.Question
import Marvel.Id
import Marvel.Projection
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Payment
import Marvel.Query
import Marvel.Resource.Types qualified as R
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.AgentsOfShield.PlayerHelpers

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

aosP50024 :: EventCard AosP50024
aosP50024 = event AosP50024 Cards.aosP50024

newtype AosP50024 = AosP50024 (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage AosP50024 where
  runMessage msg card@(AosP50024 attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      cardSpecific attrs "superSpies" (you,3 :: Natural)
      pure card
    CardSpecific (isTarget attrs -> True) "superSpies" (fromJSON -> Success (you,n :: Natural)) -> do
      when (n > 0) do
        supports <- selectList (SupportWithTrait T.Shield)
        suits <- selectList (UpgradeOneOf [UpgradeWithCard (CardWithTitle "Assault"),UpgradeWithCard (CardWithTitle "Stealth")])
        chooseOne you $ [targeting sid (placeCounters sid 1 >> cardSpecific attrs "superSpies" (you,n-1)) | sid <- supports]
          <> [targeting uid (cardSpecific uid "suitThreat" (True,1 :: Natural) >> cardSpecific attrs "superSpies" (you,n-1)) | uid <- suits]
      pure card
    _ -> AosP50024 <$> runMessage msg attrs
