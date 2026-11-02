{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Upgrade.Upgrades.Fw54009 where
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

import Marvel.Keyword (Keyword(Steady))
fw54009 :: UpgradeCard Fw54009
fw54009 = upgrade Fw54009 Cards.fw54009

newtype Fw54009 = Fw54009 (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Fw54009 where
  getModifiersFor _ (IdentityRef you) (Fw54009 attrs)
    | you == upgradeController attrs = pure [HitPointModifier 3, KeywordModifier Steady]
  getModifiersFor _ _ _ = pure []

instance RunMessage Fw54009 where
  runMessage msg (Fw54009 attrs) = runQueueT $ Fw54009 <$> runMessage msg attrs
