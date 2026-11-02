module Marvel.Upgrade.Upgrades.AjaAdannaShuri where
import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Projection
import Marvel.Identity.Types (Field (PlayerIdentityDiscard))
import Marvel.Discard
import Marvel.Deck ()

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Upgrade.Types

ajaAdannaShuri :: UpgradeCard AjaAdannaShuri
ajaAdannaShuri = upgrade AjaAdannaShuri Cards.ajaAdannaShuri
newtype AjaAdannaShuri = AjaAdannaShuri (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage AjaAdannaShuri where
  runMessage msg u@(AjaAdannaShuri attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      cards <- filter (cardMatch HeroSpecificCard) <$> projectMap PlayerIdentityDiscard unDiscard you
      chooseOne you [targeting card (shuffleIntoDeck you [card]) | card <- cards]
      pure u
    _ -> AjaAdannaShuri <$> runMessage msg attrs

instance HasAbilities AjaAdannaShuri where
  getAbilities u@(AjaAdannaShuri _attrs) = [ability u 1 Action (OwnsThis <> ExtendedCardExists (InDiscardOf You (BasicCardMatches HeroSpecificCard))) ExhaustCost (runAbility u 1)]
