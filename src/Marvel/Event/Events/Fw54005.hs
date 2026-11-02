{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Event.Events.Fw54005 where
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
fw54005 :: EventCard Fw54005
fw54005 = event Fw54005 Cards.fw54005

newtype Fw54005 = Fw54005 (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Fw54005 where
  runMessage msg a@(Fw54005 attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment windows) | ident == eventId attrs -> do
      enemies <- selectList AttackableEnemy
      armed <- cyberneticArmPaid attrs
      chooseOne you [targeting (enemyRef enemy) $ dealDamage enemy attrs ((if armed then withOverkill else id) $ toDamage 7 $ FromPlayerAttack you) | enemy <- enemies]
      pure a
    _ -> Fw54005 <$> runMessage msg attrs
