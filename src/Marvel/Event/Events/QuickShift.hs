module Marvel.Event.Events.QuickShift where

import Marvel.Prelude
import Marvel.Card.Side ()
import Marvel.Payment ()
import Marvel.Game.Source ()
import Marvel.Hand ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Id ()
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Upgrade.Cards qualified as Upgrades

quickShift :: EventCard QuickShift
quickShift = event QuickShift Cards.quickShift

newtype QuickShift = QuickShift (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage QuickShift where
  runMessage msg e@(QuickShift attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      phased <- isInMassForm you Upgrades.phased
      if phased then drawCards you 2 else do
        forms <- selectList (UpgradeIs Upgrades.solid <> UpgradeControlledBy (IdentityWithId you))
        for_ (headMay forms) (flipMassForm you)
      pure e
    _ -> QuickShift <$> runMessage msg attrs
