module Marvel.Upgrade.Upgrades.Followed (
  followed,
  Followed (..),
) where

import Marvel.Prelude

import Data.Maybe (maybeToList)

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Choice
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

followed :: UpgradeCard Followed
followed = upgrade Followed Cards.followed

newtype Followed = Followed (Attrs Upgrade)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Followed where
  getAbilities a@(Followed attrs) =
    [ limitedWindowAbility a 1 (SideSchemeDefeated When $ SideSchemeWithId sid) Interrupt OwnsThis NoCost $
        ChooseDamage (toSource a) (toDamage 4 FromAbility) AnyEnemy
    | sid <- maybeToList (upgradeAttachedSideScheme attrs)
    ]

-- | Attached to a side scheme without one.
instance IsUpgrade Followed where
  upgradeTargets _ = Just . map toTarget <$> selectList (NotSideScheme $ SideSchemeWithUpgrade $ UpgradeIs Cards.followed)

instance RunMessage Followed where
  runMessage msg (Followed a) = Followed <$> runMessage msg a
