module Marvel.Upgrade.Upgrades.DownTime (
  downTime,
  DownTime (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

downTime :: UpgradeCard DownTime
downTime = upgrade DownTime Cards.downTime

newtype DownTime = DownTime (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | "Your alter-ego gets +2 REC": recovering is done in alter-ego form only.
instance HasModifiersFor DownTime where
  getModifiersFor _ (IdentityRef iid) (DownTime a)
    | iid == upgradeController a = pure [RecoveryModifier 2]
  getModifiersFor _ _ _ = pure []

instance RunMessage DownTime where
  runMessage msg (DownTime attrs) = DownTime <$> runMessage msg attrs
