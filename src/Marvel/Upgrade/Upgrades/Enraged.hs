module Marvel.Upgrade.Upgrades.Enraged (
  enraged,
  Enraged (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Choice
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

enraged :: UpgradeCard Enraged
enraged = upgrade Enraged Cards.enraged

newtype Enraged = Enraged (Attrs Upgrade)
  deriving anyclass (IsUpgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance HasAbilities Enraged where
  getAbilities _ = []

instance HasModifiersFor Enraged where
  getModifiersFor _ (AllyRef aid) (Enraged attrs)
    | Just aid == upgradeAttachedAlly attrs =
        pure [AttackModifier 2, AttackConsequentialDamageModifier 1]
  getModifiersFor _ _ _ = pure []

-- | Attached to an ally without one.
instance RunMessage Enraged where
  runMessage msg u@(Enraged a) = case msg of
    UpgradeMessage ident PlayedUpgrade | ident == upgradeId a -> do
      allies <- selectList $ NotAlly $ AllyWithUpgrade $ UpgradeIs Cards.enraged
      chooseOne (upgradeController a) $
        [ TargetLabel (toRef allyId) [Run [UpgradeMessage (upgradeId a) $ UpgradeAttachedToAlly allyId]]
        | allyId <- allies
        ]
      pure u
    _ -> Enraged <$> runMessage msg a
