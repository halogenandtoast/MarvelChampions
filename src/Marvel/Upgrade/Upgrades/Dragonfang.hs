module Marvel.Upgrade.Upgrades.Dragonfang (dragonfang, Dragonfang (..)) where

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

dragonfang :: UpgradeCard Dragonfang
dragonfang = upgrade Dragonfang Cards.dragonfang

newtype Dragonfang = Dragonfang (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Dragonfang where
  getModifiersFor _ (IdentityRef ident) (Dragonfang attrs) | ident == upgradeController attrs = do
    marked <- selectList (EnemyWithUpgrade $ UpgradeIs Cards.deathGlow <> upgradeControlledBy ident)
    pure $ AttackModifier 1 : [AttackModifierAgainst enemy 1 | enemy <- marked]
  getModifiersFor _ _ _ = pure []

instance RunMessage Dragonfang where
  runMessage msg (Dragonfang attrs) = runQueueT $ case msg of
    _ -> Dragonfang <$> runMessage msg attrs
