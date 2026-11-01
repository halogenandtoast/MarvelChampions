module Marvel.Upgrade.Upgrades.RockPaperScissors where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Card.PlayerCard ()
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Hand
import Marvel.Identity.Types (Field (PlayerIdentityHand))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Payment
import Marvel.Projection
import Marvel.Ref
import Marvel.Resource
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

rockPaperScissors :: UpgradeCard RockPaperScissors
rockPaperScissors = upgrade RockPaperScissors Cards.rockPaperScissors

newtype RockPaperScissors = RockPaperScissors (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities RockPaperScissors where
  getAbilities a =
    [ability a 1 HeroAction (OwnsThis
      <> ExtendedCardExists (InHandOf You $ BasicCardMatches printedResourceCard)
      <> ExtendedCardExists (InDeckOf You $ BasicCardMatches AnyCard)) ExhaustCost (runAbility a 1)]

instance RunMessage RockPaperScissors where
  runMessage msg e@(RockPaperScissors attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      hand <- filter (cardMatch printedResourceCard) <$> projectMap PlayerIdentityHand unHand you
      chooseOne you [targeting card (cardSpecific attrs "chosen" card) | card <- hand]
      pure e
    CardSpecific (isTarget attrs -> True) "chosen" (fromJSON -> Success (chosen :: PlayerCard)) -> do
      payCostThenWithPayment (upgradeController attrs) attrs (DiscardTopOfDeckCardsCost 1 1) attrs "compare" chosen
      pure e
    CardSpecific (isTarget attrs -> True) "compare" (fromJSON -> Success (chosen :: PlayerCard, payment :: Payment)) -> do
      chosenIcons <- printedResourcesFor chosen
      for_ [card | PlayerCard card <- paymentDiscards payment] $ \discarded -> do
        discardedIcons <- printedResourcesFor discarded
        when (or [resourceBeats ours theirs | ours <- chosenIcons, theirs <- discardedIcons]) $
          addToHand (upgradeController attrs) discarded
      pure e
    _ -> RockPaperScissors <$> runMessage msg attrs

resourceBeats :: Resource -> Resource -> Bool
resourceBeats Energy Mental = True
resourceBeats Mental Physical = True
resourceBeats Physical Energy = True
resourceBeats Wild other = other /= Wild
resourceBeats _ _ = False

-- A zero-icon card cannot win against any hidden card; other possible losses
-- remain legal because the top card has not been revealed.
printedResourceCard :: CardMatcher
printedResourceCard = CardMatchesAny [CardWithResource resource | resource <- [Energy, Mental, Physical, Wild]]
