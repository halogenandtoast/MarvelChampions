module Marvel.Upgrade.Upgrades.Unflappable (
  unflappable,
  Unflappable (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

unflappable :: UpgradeCard Unflappable
unflappable = upgrade Unflappable Cards.unflappable

newtype Unflappable = Unflappable (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | After you defend against an attack and take no damage, exhaust it → draw 1 card.
instance HasAbilities Unflappable where
  getAbilities (Unflappable a) =
    [ limitedWindowAbility
        a
        1
        (W.HeroDefendedWithoutDamage W.After $ IdentityWithId you)
        Response
        OwnsThis
        ExhaustCost
        (Run [IdentityMessage you $ DrawCards FromDeck 1])
    ]
   where
    you = upgradeController a

instance RunMessage Unflappable where
  runMessage msg (Unflappable attrs) = Unflappable <$> runMessage msg attrs
