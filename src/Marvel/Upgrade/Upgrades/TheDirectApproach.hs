module Marvel.Upgrade.Upgrades.TheDirectApproach where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Window qualified as W ()
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Keyword

theDirectApproach :: UpgradeCard TheDirectApproach
theDirectApproach = upgrade TheDirectApproach Cards.theDirectApproach

newtype TheDirectApproach = TheDirectApproach (Attrs Upgrade)
  deriving anyclass (HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade TheDirectApproach where
  upgradeTargets _ = do
    schemes <- selectList $ NotSideScheme (SideSchemeWithKeyword Permanent)
    existing <- selectList $ UpgradeIs Cards.theDirectApproach
    occupied <- concatMapM getUpgradeAttachedSchemes existing
    pure $ Just [schemeRef (SchemeSideSchemeId sid) | sid <- schemes, SchemeSideSchemeId sid `notElem` occupied]

instance HasModifiersFor TheDirectApproach where
  getModifiersFor _ target (TheDirectApproach attrs) = pure
    [KeywordModifier Assault | Just sid <- [upgradeAttachedSideScheme attrs], target == schemeRef (SchemeSideSchemeId sid)]

instance RunMessage TheDirectApproach where
  runMessage msg (TheDirectApproach attrs) = runQueueT $ case msg of
    _ -> TheDirectApproach <$> runMessage msg attrs
