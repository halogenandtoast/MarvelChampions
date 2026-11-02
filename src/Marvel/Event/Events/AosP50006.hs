{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Event.Events.AosP50006 where

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

aosP50006 :: EventCard AosP50006
aosP50006 = event AosP50006 Cards.aosP50006

newtype AosP50006 = AosP50006 (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage AosP50006 where
  runMessage msg card@(AosP50006 attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent _ payment _) | eid == eventId attrs -> do
      for_ [target | DiscardPayment target <- payments payment] $ \target -> do
        def <- getsGame (`abilityCardDef` target)
        let n = fromIntegral $ max 0 $ fromMaybe 0 $ def >>= cdCost
        enemies <- selectList DamageableEnemy
        for_ enemies $ \enemy -> dealDamage enemy attrs (toDamage n FromAbility)
      pure card
    _ -> AosP50006 <$> runMessage msg attrs
