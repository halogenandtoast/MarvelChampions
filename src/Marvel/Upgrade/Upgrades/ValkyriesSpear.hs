module Marvel.Upgrade.Upgrades.ValkyriesSpear (valkyriesSpear, ValkyriesSpear (..)) where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

valkyriesSpear :: UpgradeCard ValkyriesSpear
valkyriesSpear = upgrade ValkyriesSpear Cards.valkyriesSpear

newtype ValkyriesSpear = ValkyriesSpear (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor ValkyriesSpear where
  getModifiersFor _ (IdentityRef ident) (ValkyriesSpear attrs) | ident == upgradeController attrs = do
    marked <- selectList (EnemyWithUpgrade $ UpgradeIs Cards.deathGlow <> upgradeControlledBy ident)
    pure $ DefenseModifier 1 : [DefenseModifierAgainst enemy 1 | enemy <- marked]
  getModifiersFor _ _ _ = pure []

instance RunMessage ValkyriesSpear where
  runMessage msg (ValkyriesSpear attrs) = runQueueT $ case msg of
    _ -> ValkyriesSpear <$> runMessage msg attrs
