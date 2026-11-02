{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Upgrade.Upgrades.Fw53034 where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Upgrade.Types

import Marvel.Keyword (Keyword(Retaliate))
fw53034 :: UpgradeCard Fw53034
fw53034 = upgrade Fw53034 Cards.fw53034

newtype Fw53034 = Fw53034 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Fw53034 where
  getModifiersFor _ (IdentityRef you) (Fw53034 attrs) | you == upgradeController attrs = pure [DefenseModifier 1, KeywordModifier (Retaliate 1)]
  getModifiersFor _ _ _ = pure []

instance RunMessage Fw53034 where
  runMessage msg (Fw53034 attrs) = runQueueT $ Fw53034 <$> runMessage msg attrs
