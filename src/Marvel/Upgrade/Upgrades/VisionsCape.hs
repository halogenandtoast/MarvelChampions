module Marvel.Upgrade.Upgrades.VisionsCape where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Query
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Keyword (Keyword (Retaliate, Stalwart))
visionsCape :: UpgradeCard VisionsCape
visionsCape = upgrade VisionsCape Cards.visionsCape

newtype VisionsCape = VisionsCape (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor VisionsCape where
  getModifiersFor _ (IdentityRef you) (VisionsCape a) | you == upgradeController a = do
    dense <- isInMassForm you Upgrades.dense
    intangible <- isInMassForm you Upgrades.intangible
    pure $ [KeywordModifier (Retaliate 1) | dense] <> [KeywordModifier Stalwart | intangible]
  getModifiersFor _ _ _ = pure []

instance RunMessage VisionsCape where
  runMessage msg (VisionsCape attrs) = VisionsCape <$> runMessage msg attrs
