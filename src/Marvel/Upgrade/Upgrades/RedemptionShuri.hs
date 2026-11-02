module Marvel.Upgrade.Upgrades.RedemptionShuri where

import Marvel.Prelude
import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Game.Source (isBlanked, getAllyControlSource)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Stats ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W ()
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Upgrade.Types

redemptionShuri :: UpgradeCard RedemptionShuri
redemptionShuri = upgrade (\a -> RedemptionShuri a False) Cards.redemptionShuri

-- Keep the active/inactive edge, so a later type-changing effect can win without
-- Mind Control continuously undoing it. Blanking ends this constant effect; when
-- the blanking ends it becomes the newest applicable effect again.
data RedemptionShuri = RedemptionShuri (Attrs Upgrade) Bool
  deriving stock (Show, Eq)
  deriving anyclass (HasAbilities)

instance ToJSON RedemptionShuri where
  toJSON (RedemptionShuri a enabled) = case toJSON a of
    Object o -> Object $ KeyMap.insert "redemptionShuriEnabled" (toJSON enabled) o
    _ -> error "upgrade attributes must serialize as an object"

instance FromJSON RedemptionShuri where
  parseJSON = withObject "RedemptionShuri" \o ->
    RedemptionShuri <$> parseJSON (Object o) <*> (o .:? "redemptionShuriEnabled" .!= True)

instance HasCardCode RedemptionShuri where
  toCardCode (RedemptionShuri a _) = toCardCode a

instance IsRef RedemptionShuri where
  toRef (RedemptionShuri a _) = toRef a

instance IsUpgrade RedemptionShuri where
  toUpgradeAttrs (RedemptionShuri a _) = a
  upgradeTargets _ = Just . map toTarget <$> selectList (NotMinion $ MinionWithTrait Trait.Elite)

instance RunMessage RedemptionShuri where
  runMessage msg e@(RedemptionShuri a enabled) = runQueueT $ case msg of
    UpgradeMessage uid (UpgradeAttachedToEnemy (EnemyMinionId mid)) | uid == upgradeId a -> do
      a' <- runMessage msg a
      blanked <- isBlanked a
      unless blanked $ takeControlOfMinionFrom a (upgradeController a) mid 1
      pure $ RedemptionShuri a' (not blanked)
    RemoveFromPlayAfterWindow target | target == toTarget a -> e <$ restoreHost
    RemoveFromGame target | target == toTarget a -> e <$ restoreHost
    ReturnToHand target | target == toTarget a -> e <$ restoreHost
    UpgradeMessage uid UpgradeDetached | uid == upgradeId a -> do
      restoreHost
      (\attrs -> RedemptionShuri attrs False) <$> runMessage msg a
    CardSpecific (isTarget a -> True) "syncRedemptionShuri" _ -> do
      present <- selectAny $ UpgradeWithId $ upgradeId a
      if not present then pure e else do
        blanked <- isBlanked a
        if enabled && blanked then do
          restoreHost
          pure $ RedemptionShuri a False
        else if not enabled && not blanked then do
          for_ (upgradeAttachedEnemy a) \case
            EnemyMinionId mid -> whenM (selectAny $ MinionWithId mid) $
              takeControlOfMinionFrom a (upgradeController a) mid 1
            _ -> pure ()
          pure $ RedemptionShuri a True
        else pure e
    -- One synchronization after a state change sees the completed state, rather
    -- than the pre-message game visible while entity handlers are running. Sync
    -- messages from any copy do not schedule more, avoiding multi-copy loops.
    CardSpecific _ "syncRedemptionShuri" _ -> pure e
    _ -> do
      when (isJust $ upgradeAttachedEnemy a) $
        whenM (selectAny $ UpgradeWithId $ upgradeId a) $ cardSpecific a "syncRedemptionShuri" ()
      (\attrs -> RedemptionShuri attrs enabled) <$> runMessage msg a
   where
    restoreHost = for_ (upgradeAttachedAlly a) \aid -> do
      leaving <- isQueued \case
        DefeatedLeavesPlay target -> target == AllyRef aid
        RemoveFromPlay target -> target == AllyRef aid
        RemoveFromPlayAfterWindow target -> target == AllyRef aid
        RemoveFromGame target -> target == AllyRef aid
        ReturnToHand target -> target == AllyRef aid
        AddToVictoryDisplay target -> target == AllyRef aid
        _ -> False
      present <- selectAny $ AllyWithId aid
      when (present && not leaving) do
        source <- getAllyControlSource aid
        -- Untagged snapshots predate source ownership and retain their old behavior.
        when (isNothing source || source == Just (toSource a)) $ restoreControlledMinion aid

instance HasModifiersFor RedemptionShuri where
  getModifiersFor _ (AllyRef aid) (RedemptionShuri attrs enabled)
    | enabled, Just aid == upgradeAttachedAlly attrs = pure [TraitModifier Trait.Redeemed]
  getModifiersFor _ _ _ = pure []
