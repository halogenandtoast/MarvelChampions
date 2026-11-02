{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Event.Events.Fw53003 where
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
fw53003 :: EventCard Fw53003
fw53003 = event Fw53003 Cards.fw53003

newtype Fw53003 = Fw53003 (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Fw53003 where
  runMessage msg a@(Fw53003 attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      targets <- selectList AttackableEnemy
      top <- take 1 <$> getEncounterDeck
      visible <- elem CanLookAtTopOfEncounterDeck <$> getModifiers GameRef
      countNow <- sum <$> traverse (\card -> (+ fromIntegral (length $ filter (== B.Star) $ cdBoostIcons $ getCardDef card)) <$> getBoostIcons card) top
      chooseOne you [targeting (enemyRef enemy) $
        chooseOne you $
          [labeled "$label.doNotDiscard" $ dealDamage enemy attrs (toDamage (4) $ FromPlayerAttack you)]
          <> [labeled "$label.discardTopEncounterCard" (discardTopOfEncounterDeck 1 >> countFalconIcons attrs "birdBonus" (you, enemy) top) | notNull top, not visible || countNow > 0]
        | enemy <- targets]
      pure a
    WithBoostIcons (isTarget attrs -> True) value counts -> a <$ resolveFalconIcons attrs value counts
    CardSpecific (isTarget attrs -> True) "birdBonus" (fromJSON -> Success ((you, enemy :: EnemyId), bonus :: Natural)) -> do
      dealDamage enemy attrs (toDamage (4 + bonus) $ FromPlayerAttack you)
      pure a
    _ -> Fw53003 <$> runMessage msg attrs
