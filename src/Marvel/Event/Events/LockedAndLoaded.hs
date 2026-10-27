module Marvel.Event.Events.LockedAndLoaded (
  lockedAndLoaded,
  LockedAndLoaded (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Trait qualified as Trait

lockedAndLoaded :: EventCard LockedAndLoaded
lockedAndLoaded = event LockedAndLoaded Cards.lockedAndLoaded

newtype LockedAndLoaded = LockedAndLoaded (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Search your deck for a Weapon upgrade and add it to your hand; shuffle your deck.
instance RunMessage LockedAndLoaded where
  runMessage msg e@(LockedAndLoaded attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      search you (SearchIdentityDeck you AllOfDeck) (CardWithType UpgradeType <> CardWithTrait Trait.Weapon) SearchDrawOne ShuffleBackIn
      pure e
    _ -> LockedAndLoaded <$> runMessage msg attrs
