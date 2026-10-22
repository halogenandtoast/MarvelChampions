module Marvel.Upgrade.Upgrades.UnderSurveillance (
  underSurveillance,
  UnderSurveillance (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Queue
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

underSurveillance :: UpgradeCard UnderSurveillance
underSurveillance = upgrade UnderSurveillance Cards.underSurveillance

-- | Attached to the main scheme: its target threat is 4 more. As the main scheme leaves
-- play, what is attached to it is discarded.
newtype UnderSurveillance = UnderSurveillance (Attrs Upgrade)
  deriving anyclass (HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor UnderSurveillance where
  getModifiersFor _ (MainSchemeRef mid) (UnderSurveillance attrs)
    | upgradeAttachedMainScheme attrs == Just mid = pure [TargetThreatModifier 4]
  getModifiersFor _ _ _ = pure []

instance IsUpgrade UnderSurveillance where
  upgradeTargets _ = Just . map toTarget <$> selectList MainScheme

instance RunMessage UnderSurveillance where
  runMessage msg (UnderSurveillance attrs) = UnderSurveillance <$> runMessage msg attrs
