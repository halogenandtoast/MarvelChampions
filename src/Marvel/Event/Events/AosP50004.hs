{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Event.Events.AosP50004 where

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

aosP50004 :: EventCard AosP50004
aosP50004 = event AosP50004 Cards.aosP50004

newtype AosP50004 = AosP50004 (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage AosP50004 where
  runMessage msg card@(AosP50004 attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      chooseBudgetSupports you attrs 6 [] False
      pure card
    CardSpecific (isTarget attrs -> True) "budgetSupports" (fromJSON -> Success (you,budget,used,counter)) -> card <$ chooseBudgetSupports you attrs budget used counter
    _ -> AosP50004 <$> runMessage msg attrs
