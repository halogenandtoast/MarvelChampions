module Marvel.Support.Supports.LogansCabin where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Card
import Marvel.Ally.Cards qualified as Allies
import Marvel.Event.Cards qualified as Events
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Query
import Marvel.Question
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

logansCabin :: SupportCard LogansCabin
logansCabin = support LogansCabin Cards.logansCabin

newtype LogansCabin = LogansCabin (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities LogansCabin where
  getAbilities a = [ability a 1 AlterEgoAction (OwnsThis <> ExtendedCardExists (InDiscardOf You $ BasicCardMatches $ wolverineCard)) ExhaustCost (runAbility a 1)]

instance RunMessage LogansCabin where
  runMessage msg s@(LogansCabin attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- selectList (InDiscardOf (IdentityWithId you) $ BasicCardMatches $ wolverineCard)
      chooseOne you [targeting c (shuffleIntoDeck you [c]) | c <- cards]
      pure s
    _ -> LogansCabin <$> runMessage msg attrs

wolverineCard :: CardMatcher
wolverineCard = CardMatchesAny $ map CardIs
  [ Allies.jubilee, Upgrades.wolverinesClaws, Upgrades.adamantiumSkeleton, Upgrades.berserkerFrenzy
  , Upgrades.iGotBetter, Cards.logansCabin, Events.berserkerBarrage, Events.sliceAndDice
  , Events.lungingStrike, Events.trackByScent, Events.regenerativeHealing
  ]
