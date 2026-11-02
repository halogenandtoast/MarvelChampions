{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Event.Events.Fw54023 where
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

import Marvel.Identity.Types (Field(PlayerIdentityDiscard))
import Marvel.Discard
fw54023 :: EventCard Fw54023
fw54023 = event Fw54023 Cards.fw54023

newtype Fw54023 = Fw54023 (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage Fw54023 where
  runMessage msg a@(Fw54023 attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment windows) | ident == eventId attrs -> do
      cards <- filter (cardMatch $ CardWithType UpgradeType <> CardWithTrait T.Preparation) <$> projectMap PlayerIdentityDiscard unDiscard you
      chooseOne you [targeting card $ putIntoPlay you card | card <- cards]
      chooseDamage you attrs (toDamage 4 $ FromPlayerAttack you) AttackableEnemy
      pure a
    _ -> Fw54023 <$> runMessage msg attrs
