module Marvel.Event.Events.BaitAndSwitch (
  baitAndSwitch,
  BaitAndSwitch (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

baitAndSwitch :: EventCard BaitAndSwitch
baitAndSwitch = event BaitAndSwitch Cards.baitAndSwitch

newtype BaitAndSwitch = BaitAndSwitch (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action (thwart): the villain attacks you, then remove 4 threat from the main
-- scheme.
instance RunMessage BaitAndSwitch where
  runMessage msg e@(BaitAndSwitch attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      withTheVillain you (`enemyAttacks` you)
      cardSpecific attrs "thwart" you
      pure e
    -- its own step, after the attack
    CardSpecific (isTarget attrs -> True) "thwart" (fromJSON -> Success (you :: IdentityId)) -> do
      mains <- filter isMain <$> selectList ThwartableScheme
      unless (null mains) $ chooseOrRunOne you [targeting (schemeRef scheme) (thwartScheme scheme attrs 4) | scheme <- mains]
      pure e
    _ -> BaitAndSwitch <$> runMessage msg attrs
   where
    isMain = \case
      SchemeMainSchemeId _ -> True
      _ -> False
