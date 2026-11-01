module Marvel.Upgrade.Upgrades.HawkeyesQuiver (
  hawkeyesQuiver,
  HawkeyesQuiver (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Trait (Trait (Arrow))

hawkeyesQuiver :: UpgradeCard HawkeyesQuiver
hawkeyesQuiver = upgrade HawkeyesQuiver Cards.hawkeyesQuiver

newtype HawkeyesQuiver = HawkeyesQuiver (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | "You may play Arrow events attached to this card as if they were in your hand": the
-- cards it holds ('upgradeHeldCards'). Hero Action: Exhaust Hawkeye's Quiver → search
-- the top 5 cards of your deck for an Arrow event and attach it faceup to this card;
-- the rest are shuffled back with the deck (FAQ).
instance HasAbilities HawkeyesQuiver where
  getAbilities a = [ability a 1 HeroAction OwnsThis ExhaustCost $ runAbility a 1]

instance RunMessage HawkeyesQuiver where
  runMessage msg u@(HawkeyesQuiver attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      search ident (SearchIdentityDeck ident $ TopOfDeck 5) (CardWithType EventType <> CardWithTrait Arrow) (SearchTarget $ toTarget attrs) ShuffleBackIn
      pure u
    SearchFoundCards (isTarget attrs -> True) (onlyPlayerCards -> cards) ->
      pure . HawkeyesQuiver $ attrs & heldCardsL <>~ cards
    _ -> HawkeyesQuiver <$> runMessage msg attrs
