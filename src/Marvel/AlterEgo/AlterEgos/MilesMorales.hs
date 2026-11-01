module Marvel.AlterEgo.AlterEgos.MilesMorales where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.AlterEgo.Cards qualified as Cards
import Marvel.AlterEgo.Runner
import Marvel.GameValue
import Marvel.Hand
import Marvel.Stats
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Card.Def
import Marvel.Event.Cards qualified as Events
import Marvel.Support.Cards qualified as Supports
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Discard
import Marvel.Identity.Types (Field (PlayerIdentityDiscard))
import Marvel.Matchers
import Marvel.Projection
import Marvel.Query ()
import Marvel.Question (runAbility)
import Marvel.Window qualified as W

milesMorales :: AlterEgoCard MilesMorales
milesMorales = alterEgo MilesMorales Cards.milesMorales (HP $ Static 9) (HandSize 6) (Rec 4) [Obligations.keepingSecrets]

newtype MilesMorales = MilesMorales (Attrs AlterEgo)
  deriving anyclass (IsAlterEgo, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities MilesMorales where
  getAbilities a@(MilesMorales attrs) =
    [ limitedWindowAbility a 1
        (W.IdentityChangedToForm W.After (IdentityWithId $ alterEgoIdentityId attrs))
        Response
        (IsSelf <> ExtendedCardExists (InDiscardOf You (BasicCardMatches milesSignature)))
        NoCost (runAbility a 1) ]

milesSignature :: CardMatcher
milesSignature = CardMatchesAny $ map CardIs [Events.arachnobatics, Events.doubleLife, Events.swingIn, Events.webShot, Supports.gankeLee, Supports.jeffersonDavis, Upgrades.powerWithin, Upgrades.defenseMechanism, Upgrades.webShooterMilesMorales]

instance RunMessage MilesMorales where
  runMessage msg ae@(MilesMorales attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- filter (cardMatch milesSignature) <$> projectMap PlayerIdentityDiscard unDiscard you
      unless (null cards) $ chooseOne you [targeting card (shuffleIntoDeck you [card]) | card <- cards]
      pure ae
    _ -> MilesMorales <$> runMessage msg attrs
