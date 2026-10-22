module Marvel.Event.Events.MysticalStudies (
  mysticalStudies,
  MysticalStudies (..),
) where

import Marvel.Prelude

import Marvel.Ally.Cards qualified as Allies
import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Message
import Marvel.Modifier
import Marvel.Queue
import Marvel.Ref
import Marvel.Support.Cards qualified as Supports
import Marvel.Upgrade.Cards qualified as Upgrades

mysticalStudies :: EventCard MysticalStudies
mysticalStudies = event MysticalStudies Cards.mysticalStudies

newtype MysticalStudies = MysticalStudies (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | The cards of Doctor Strange's own, that his deck is built with.
doctorStrangeCards :: [CardDef]
doctorStrangeCards =
  [ Allies.wong
  , Cards.astralProjection
  , Cards.magicBlast
  , Cards.masterOfTheMysticArts
  , Cards.mysticalStudies
  , Cards.protectiveWard
  , Supports.sanctumSanctorum
  , Upgrades.cloakOfLevitation
  , Upgrades.magicalEnhancements
  , Upgrades.theEyeOfAgamotto
  ]

-- | Search your deck and discard pile for a Doctor Strange card and add it to your hand.
-- Shuffle your deck.
instance RunMessage MysticalStudies where
  runMessage msg e@(MysticalStudies attrs) = case msg of
    EventMessage ident (PlayedEvent identityId _ _) | ident == eventId attrs -> do
      push . IdentityMessage identityId $
        Search
          (SearchIdentityDeck identityId AllOfDeckAndDiscard)
          (CardMatchesAny $ map CardIs doctorStrangeCards)
          SearchDrawOne
          ShuffleBackIn
      pure e
    _ -> MysticalStudies <$> runMessage msg attrs
