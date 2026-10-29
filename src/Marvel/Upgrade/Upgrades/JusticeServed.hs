module Marvel.Upgrade.Upgrades.JusticeServed (justiceServed, JusticeServed (..)) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Matchers qualified as M
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

justiceServed :: UpgradeCard JusticeServed
justiceServed = upgrade JusticeServed Cards.justiceServed

newtype JusticeServed = JusticeServed (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities JusticeServed where
  getAbilities a@(JusticeServed attrs) =
    [limitedWindowAbility a 1 (W.HeroThwartsAllThreat $ IdentityWithId $ upgradeController attrs)
      HeroResponse (OwnsThis <> SelfMatches M.ExhaustedIdentity)
      (DiscardCost $ toTarget a) (runAbility a 1)]

instance RunMessage JusticeServed where
  runMessage msg a@(JusticeServed attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> a <$ ready ident
    _ -> JusticeServed <$> runMessage msg attrs
