{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Event.Events.Fw54004 where
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
fw54004 :: EventCard Fw54004
fw54004 = event Fw54004 Cards.fw54004

newtype Fw54004 = Fw54004 (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Fw54004 where
  runMessage msg a@(Fw54004 attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment windows) | ident == eventId attrs -> do
      let enemies = [enemy | window <- maybeToList windows, enemy <- case window of W.EnemyAttack foe _ -> [foe]; W.EnemyAttackAlly foe _ _ -> [foe]; _ -> []]
      for_ enemies $ \enemy -> do
        dealDamage enemy attrs (toDamage 3 $ FromPlayerAttack you)
        armed <- cyberneticArmPaid attrs
        when armed $ preventAttackDamage enemy
      pure a
    _ -> Fw54004 <$> runMessage msg attrs
