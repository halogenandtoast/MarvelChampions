module Marvel.Upgrade.Upgrades.ItAintOver where

import Marvel.Prelude
import Marvel.Ability (HasAbilities)
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

itAintOver :: UpgradeCard ItAintOver
itAintOver = upgrade ItAintOver Cards.itAintOver

newtype ItAintOver = ItAintOver (Attrs Upgrade)
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade ItAintOver where
  upgradeTargets _ = Just . map toTarget <$> selectList MainScheme

instance HasModifiersFor ItAintOver where
  getModifiersFor _ (MainSchemeRef mid) (ItAintOver attrs) | upgradeAttachedMainScheme attrs == Just mid = do
    tokens <- getAccelerationTokensOnMainScheme mid
    pure [TargetThreatModifier (2 * tokens)]
  getModifiersFor _ _ _ = pure []

instance RunMessage ItAintOver where
  runMessage msg (ItAintOver attrs) = runQueueT $ ItAintOver <$> runMessage msg attrs
