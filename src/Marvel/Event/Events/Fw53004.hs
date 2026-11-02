{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Event.Events.Fw53004 where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Boost qualified as B
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
fw53004 :: EventCard Fw53004
fw53004 = event Fw53004 Cards.fw53004

newtype Fw53004 = Fw53004 (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Fw53004 where
  runMessage msg a@(Fw53004 attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      targets <- selectList ThwartableScheme
      top <- take 1 <$> getEncounterDeck
      visible <- elem CanLookAtTopOfEncounterDeck <$> getModifiers GameRef
      countNow <- sum <$> traverse (\card -> (+ fromIntegral (length $ filter (== B.Star) $ cdBoostIcons $ getCardDef card)) <$> getBoostIcons card) top
      chooseOne you [targeting (schemeRef scheme) $
        chooseOne you $
          [labeled "$label.doNotDiscard" $ thwartSchemeAs you scheme attrs (3)]
          <> [labeled "$label.discardTopEncounterCard" (discardTopOfEncounterDeck 1 >> countFalconIcons attrs "birdBonus" (you, scheme) top) | notNull top, not visible || countNow > 0]
        | scheme <- targets]
      pure a
    WithBoostIcons (isTarget attrs -> True) value counts -> a <$ resolveFalconIcons attrs value counts
    CardSpecific (isTarget attrs -> True) "birdBonus" (fromJSON -> Success ((you, scheme :: SchemeId), bonus :: Natural)) -> do
      thwartSchemeAs you scheme attrs (3 + bonus)
      pure a
    _ -> Fw53004 <$> runMessage msg attrs
