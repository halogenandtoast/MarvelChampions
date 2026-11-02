module Marvel.Event.Events.HeartofthePantherShuri where
import Marvel.Prelude
import Marvel.Id
import Marvel.Query
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()
import Marvel.Trait
import Marvel.BlackPantherShuri.Helpers ()

import Marvel.Ability ()
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Event.Types

heartofthePantherShuri :: EventCard HeartofthePantherShuri
heartofthePantherShuri = event HeartofthePantherShuri Cards.heartofthePantherShuri
newtype HeartofthePantherShuri = HeartofthePantherShuri (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage HeartofthePantherShuri where
  runMessage msg u@(HeartofthePantherShuri attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      search you (SearchIdentityDeckAndDiscardPile you) (CardWithType UpgradeType <> CardWithTrait BlackPanther) (SearchTarget $ toTarget attrs) ShuffleBackIn
      cardSpecific attrs "specials" you
      pure u
    SearchFoundCards (isTarget attrs -> True) (onlyPlayerCards -> cards) -> do
      let you = eventController attrs
      traverse_ (putIntoPlay you) cards
      pure u
    CardSpecific (isTarget attrs -> True) "specials" (fromJSON -> Success (you :: IdentityId)) -> do
      upgrades <- selectList (UpgradeWithTrait BlackPanther <> UpgradeControlledBy (IdentityWithId you) <> UpgradeWithResolvableSpecial)
      cardSpecific attrs "remaining" (you,upgrades,4 :: Natural)
      pure u
    CardSpecific (isTarget attrs -> True) "remaining" (fromJSON -> Success (you,upgrades :: [UpgradeId],n :: Natural)) -> do
      current <- selectList (UpgradeWithTrait BlackPanther <> UpgradeControlledBy (IdentityWithId you) <> UpgradeWithResolvableSpecial)
      let remaining = filter (`elem` current) upgrades
      when (n > 0) $ chooseOne you (labeled "$label.done" (pure ()) : [targeting uid (resolveAs you (runAbility uid 1) >> cardSpecific attrs "remaining" (you,filter (/= uid) remaining,n-1)) | uid <- remaining])
      pure u
    _ -> HeartofthePantherShuri <$> runMessage msg attrs
