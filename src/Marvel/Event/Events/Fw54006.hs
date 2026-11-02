{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Event.Events.Fw54006 where
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

fw54006 :: EventCard Fw54006
fw54006 = event Fw54006 Cards.fw54006

newtype Fw54006 = Fw54006 (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Fw54006 where
  runMessage msg a@(Fw54006 attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment windows) | ident == eventId attrs -> do
      energy <- paidWith R.Energy payment
      enemies <- selectList AttackableEnemy
      chooseOne you [targeting (enemyRef enemy) do
        dealDamage enemy attrs (toDamage 4 $ FromPlayerAttack you)
        when energy $ stun enemy attrs
        | enemy <- enemies]
      pure a
    _ -> Fw54006 <$> runMessage msg attrs
