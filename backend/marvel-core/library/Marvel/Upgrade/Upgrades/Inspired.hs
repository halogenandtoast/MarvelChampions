module Marvel.Upgrade.Upgrades.Inspired
  ( inspired
  , Inspired(..)
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Source
import Marvel.Target
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

inspired :: UpgradeCard Inspired
inspired = upgrade Inspired Cards.inspired

newtype Inspired = Inspired (Attrs Upgrade)
  deriving anyclass IsUpgrade
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance HasAbilities Inspired where
  getAbilities _ = []

instance HasModifiersFor Inspired where
  getModifiersFor _ (AllyTarget aid) (Inspired attrs)
    | Just aid == upgradeAttachedAlly attrs = pure
      [ThwartModifier 1, AttackModifier 1]
  getModifiersFor _ _ _ = pure []

instance RunMessage Inspired where
  runMessage msg u@(Inspired a) = case msg of
    UpgradeMessage ident msg' | ident == upgradeId a -> case msg' of
      PlayedUpgrade -> do
        allies <- selectList AnyAlly
        chooseOne (upgradeController a) $ map
          (\allyId -> TargetLabel
            (AllyTarget allyId)
            [Run [UpgradeMessage (upgradeId a) $ UpgradeAttachedToAlly allyId]]
          )
          allies
        pure u
      _ -> Inspired <$> runMessage msg a
    _ -> Inspired <$> runMessage msg a

