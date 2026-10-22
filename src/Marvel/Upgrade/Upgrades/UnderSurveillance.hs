module Marvel.Upgrade.Upgrades.UnderSurveillance (
  underSurveillance,
  UnderSurveillance (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Modifier
import Marvel.Queue
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

underSurveillance :: UpgradeCard UnderSurveillance
underSurveillance = upgrade UnderSurveillance Cards.underSurveillance

-- | Attached to the main scheme: its target threat is 4 more, and as the main scheme
-- advances the cards attached to it are discarded. ponytail: there is one main scheme
-- in play, so it is "the" main scheme rather than one kept by id.
newtype UnderSurveillance = UnderSurveillance (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor UnderSurveillance where
  getModifiersFor _ (MainSchemeRef _) _ = pure [TargetThreatModifier 4]
  getModifiersFor _ _ _ = pure []

instance RunMessage UnderSurveillance where
  runMessage msg u@(UnderSurveillance attrs) = case msg of
    NextMainScheme -> do
      push $ RemoveFromPlay (toTarget attrs)
      pure u
    _ -> UnderSurveillance <$> runMessage msg attrs
