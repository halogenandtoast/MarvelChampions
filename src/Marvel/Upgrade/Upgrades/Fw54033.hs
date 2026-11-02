{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Upgrade.Upgrades.Fw54033 where
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

fw54033 :: UpgradeCard Fw54033
fw54033 = upgrade Fw54033 Cards.fw54033

newtype Fw54033 = Fw54033 (Attrs Upgrade)
  deriving anyclass (HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Fw54033 where
  getModifiersFor _ target (Fw54033 attrs)
    | target `elem` catMaybes [toTarget <$> upgradeAttachedIdentity attrs, toTarget <$> upgradeAttachedAlly attrs] = pure [HitPointModifier 1, TraitModifier T.Shield]
  getModifiersFor _ _ _ = pure []

instance IsUpgrade Fw54033 where
  upgradeTargets _ = Just . map characterRef <$> selectList (CharacterControlledBy AnyIdentity)

instance RunMessage Fw54033 where
  runMessage msg (Fw54033 attrs) = runQueueT $ case msg of
    UpgradeMessage uid (PlayedUpgrade (Just (IdentityRef who))) | uid == upgradeId attrs -> do
      upgradeStep uid (UpgradeAttachedToIdentity who)
      Fw54033 <$> runMessage msg attrs
    _ -> Fw54033 <$> runMessage msg attrs
