module Marvel.Upgrade.Upgrades.MindControl where

import Marvel.Prelude
import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Game.Source (isBlanked, getAllyControlSource)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types

mindControl :: UpgradeCard MindControl
mindControl = upgrade (\a -> MindControl a False) Cards.mindControl

-- Keep the active/inactive edge, so a later type-changing effect can win without
-- Mind Control continuously undoing it. Blanking ends this constant effect; when
-- the blanking ends it becomes the newest applicable effect again.
data MindControl = MindControl (Attrs Upgrade) Bool
  deriving stock (Show, Eq)
  deriving anyclass (HasModifiersFor, HasAbilities)

instance ToJSON MindControl where
  toJSON (MindControl a enabled) = case toJSON a of
    Object o -> Object $ KeyMap.insert "mindControlEnabled" (toJSON enabled) o
    _ -> error "upgrade attributes must serialize as an object"

instance FromJSON MindControl where
  parseJSON = withObject "MindControl" \o ->
    MindControl <$> parseJSON (Object o) <*> (o .:? "mindControlEnabled" .!= True)

instance HasCardCode MindControl where
  toCardCode (MindControl a _) = toCardCode a

instance IsRef MindControl where
  toRef (MindControl a _) = toRef a

instance IsUpgrade MindControl where
  toUpgradeAttrs (MindControl a _) = a
  upgradeTargets _ = Just . map toTarget <$> selectList (NotMinion $ MinionWithTrait Trait.Elite)

instance RunMessage MindControl where
  runMessage msg e@(MindControl a enabled) = runQueueT $ case msg of
    UpgradeMessage uid (UpgradeAttachedToEnemy (EnemyMinionId mid)) | uid == upgradeId a -> do
      a' <- runMessage msg a
      blanked <- isBlanked a
      unless blanked $ takeControlOfMinionFrom a (upgradeController a) mid 1
      pure $ MindControl a' (not blanked)
    RemoveFromPlayAfterWindow target | target == toTarget a -> e <$ restoreHost
    RemoveFromGame target | target == toTarget a -> e <$ restoreHost
    ReturnToHand target | target == toTarget a -> e <$ restoreHost
    UpgradeMessage uid UpgradeDetached | uid == upgradeId a -> do
      restoreHost
      (\attrs -> MindControl attrs False) <$> runMessage msg a
    CardSpecific (isTarget a -> True) "syncMindControl" _ -> do
      present <- selectAny $ UpgradeWithId $ upgradeId a
      if not present then pure e else do
        blanked <- isBlanked a
        if enabled && blanked then do
          restoreHost
          pure $ MindControl a False
        else if not enabled && not blanked then do
          for_ (upgradeAttachedEnemy a) \case
            EnemyMinionId mid -> whenM (selectAny $ MinionWithId mid) $
              takeControlOfMinionFrom a (upgradeController a) mid 1
            _ -> pure ()
          pure $ MindControl a True
        else pure e
    -- One synchronization after a state change sees the completed state, rather
    -- than the pre-message game visible while entity handlers are running. Sync
    -- messages from any copy do not schedule more, avoiding multi-copy loops.
    CardSpecific _ "syncMindControl" _ -> pure e
    _ -> do
      when (isJust $ upgradeAttachedEnemy a) $
        whenM (selectAny $ UpgradeWithId $ upgradeId a) $ cardSpecific a "syncMindControl" ()
      (\attrs -> MindControl attrs enabled) <$> runMessage msg a
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
