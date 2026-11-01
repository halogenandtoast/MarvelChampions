module Marvel.Event.Events.MutantPeacekeepers where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Count

mutantPeacekeepers :: EventCard MutantPeacekeepers
mutantPeacekeepers = event MutantPeacekeepers Cards.mutantPeacekeepers

newtype MutantPeacekeepers = MutantPeacekeepers (Attrs Event)
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- This event increases its single distribution pool, rather than increasing each
-- scheme's share again when that share resolves (project-owner ruling).
instance HasModifiersFor MutantPeacekeepers where
  getModifiersFor _ target (MutantPeacekeepers attrs) =
    pure [ThreatRemovalBonusInTotal | target == toRef attrs]

-- The event is one labeled thwart, with each scheme receiving a positive share
-- thwarted through the normal pipeline. Exhausted allies contribute their THW;
-- they do not themselves make basic thwarts or suffer consequential damage.
instance IsEvent MutantPeacekeepers where
  eventTargets (MutantPeacekeepers attrs) = do
    confused <- identityMatches ConfusedIdentity (eventController attrs)
    legalScheme <- selectAny SchemeYouCanThwart
    pure $ if confused || legalScheme then Nothing else Just []

instance RunMessage MutantPeacekeepers where
  runMessage msg e@(MutantPeacekeepers attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you payment _) | eid == eventId attrs -> do
      hero <- selectCount HeroThwartValue (IdentityWithId you)
      allies <- traverse (selectCount AllyThwartValue . AllyWithId) (paymentExhaustedAllies payment)
      -- The divided-thwart pipeline reads bonuses after its interrupt window,
      -- then adds them to this single total before any points are assigned.
      thwartDivided you attrs (hero + sum allies) SchemeYouCanThwart
      pure e
    _ -> MutantPeacekeepers <$> runMessage msg attrs
