{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Event.Events.Fw54022 where
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
fw54022 :: EventCard Fw54022
fw54022 = event Fw54022 Cards.fw54022

newtype Fw54022 = Fw54022 (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Fw54022 where
  runMessage msg a@(Fw54022 attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment windows) | ident == eventId attrs -> do
      chooseDamage you attrs (toDamage 6 $ FromPlayerAttack you) AttackableEnemy
      characters <- namedCharacters ["Captain America", "Winter Soldier"]
      for_ characters giveTough
      pure a
    _ -> Fw54022 <$> runMessage msg attrs
