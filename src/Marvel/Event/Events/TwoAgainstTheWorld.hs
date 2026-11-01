module Marvel.Event.Events.TwoAgainstTheWorld (twoAgainstTheWorld, TwoAgainstTheWorld (..)) where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait (Trait (Tech))

twoAgainstTheWorld :: EventCard TwoAgainstTheWorld
twoAgainstTheWorld = event TwoAgainstTheWorld Cards.twoAgainstTheWorld

newtype TwoAgainstTheWorld = TwoAgainstTheWorld (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage TwoAgainstTheWorld where
  runMessage msg e@(TwoAgainstTheWorld attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      candidates <- selectList (CanEnterPlay <> BasicCardMatches (CardWithType UpgradeType <> CardWithTrait Tech))
      legal <- filterM (fmap (maybe True notNull) . getPlayTargets you) candidates
      search you (SearchIdentityDeck you AllOfDeck)
        (CardMatchesAny $ map (CardIs . getCardDef) legal) (SearchTarget $ toTarget attrs) ShuffleBackIn
      -- SearchFoundCards puts the selected card into play before the deck is
      -- shuffled. Readying follows the entire search, even when no card is found.
      cardSpecific attrs "ready" ()
      pure e
    SearchFoundCards (isTarget attrs -> True) (onlyPlayerCards -> cards) -> do
      for_ cards \card -> do
        targets <- getPlayTargets (eventController attrs) card
        case targets of
          Nothing -> putIntoPlay (eventController attrs) card
          Just choices -> unless (null choices) $ chooseOne (eventController attrs)
            [targeting target (putIntoPlayOn (eventController attrs) card target) | target <- choices]
      pure e
    CardSpecific (isTarget attrs -> True) "ready" _ -> do
      for_ ["Iron Man", "War Machine"] \name -> do
        identities <- selectList (IdentityWithTitle name)
        allies <- selectList (AllyNamedAs name)
        traverse_ ready identities
        traverse_ ready allies
      pure e
    _ -> TwoAgainstTheWorld <$> runMessage msg attrs
