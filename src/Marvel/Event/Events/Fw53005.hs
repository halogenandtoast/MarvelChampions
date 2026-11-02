{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Event.Events.Fw53005 where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Event.Types

import Marvel.FalconWinter.Helpers
fw53005 :: EventCard Fw53005
fw53005 = event Fw53005 Cards.fw53005

newtype Fw53005 = Fw53005 (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Fw53005 where
  runMessage msg a@(Fw53005 attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment windows) | ident == eventId attrs -> do
      for_ [(enemy, card) | W.FacedownBoostGiven enemy card <- maybeToList windows] $ \(enemy, originallyAssigned) -> do
        current <- getAssignedBoostInWindow enemy originallyAssigned
        for_ current $ \old -> do
          top <- headMay <$> getEncounterDeck
          showCardsTo you $ map EncounterCard $ old : maybeToList top
          chooseOne you $
            [labeled "$label.keepBoostCard" (hideCardsFrom you >> drawCards you (allBoostIcons old))]
            <> [labeled "$label.swapBoostCards" (swapFacedownBoostWithTop enemy old replacement >> hideCardsFrom you >> drawCards you (allBoostIcons replacement)) | replacement <- maybeToList top]
      pure a
    _ -> Fw53005 <$> runMessage msg attrs
