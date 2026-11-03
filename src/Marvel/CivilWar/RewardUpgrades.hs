module Marvel.CivilWar.RewardUpgrades where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.CivilWar.RewardCards qualified as R
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

newtype LeaderRewardUpgrade = LeaderRewardUpgrade (Attrs Upgrade `With` Maybe AttachmentId)
  deriving anyclass HasModifiersFor
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

leaderRewardUpgrades :: [UpgradeCard LeaderRewardUpgrade]
leaderRewardUpgrades = map (upgrade $ \a -> LeaderRewardUpgrade (a `With` Nothing)) R.rewardUpgrades

instance IsUpgrade LeaderRewardUpgrade where
  toUpgradeAttrs (LeaderRewardUpgrade (a `With` _)) = a
  upgradeTargets (LeaderRewardUpgrade (a `With` _)) = case toCardCode a of
    "56135" -> do
      allies <- selectList AnyAlly
      eligible <- filterM (\ally -> (\mods -> all (`notElem` mods) [CannotHaveAttachedCards,CannotHaveAttachedPlayerCards]) <$> getModifiers ally) allies
      pure $ Just $ map toTarget eligible
    "56213" -> do
      eligible <- friendlyLeaderExists (upgradeController a) (Just "56168")
      friendly <- friendlyLeaderFor (upgradeController a)
      pure $ Just [VillainRef vid | eligible,Just vid <- [friendly]]
    _ -> pure Nothing

instance HasAbilities LeaderRewardUpgrade where
  getAbilities (LeaderRewardUpgrade (a `With` energy)) = case toCardCode a of
    "56135" -> [windowAbility a 1 (W.AttachmentCountersPlaced W.After $ AttachmentWithId channel) Response NoCost (runAbility a 1)
      & \ability -> ability {abilityCriteria = FriendlyLeaderExists (Just "56092") <> AllyExists (AllyMatches [AllyWithId ally,AllyWithDamage $ GreaterThan $ Static 0])}
      | Just channel <- [energy],Just ally <- [upgradeAttachedAlly a]]
    "56213" -> [windowAbility a 2 (W.EnemyAttackedAnyTarget W.After $ EnemyWithId enemy) ForcedResponse NoCost (runAbility a 2)
      | Just enemy <- [upgradeAttachedEnemy a]]
    _ -> []

instance RunMessage LeaderRewardUpgrade where
  runMessage msg e@(LeaderRewardUpgrade (a `With` energy)) = runQueueT $ case msg of
    CheckWindows windows | toCardCode a == "56135" -> do
      friendly <- friendlyLeaderFor (upgradeController a)
      matching <- filterM (\aid -> do
        host <- getAttachedCharacter aid
        named <- getsGame $ \g -> maybe False ((== "56098") . toCardCode) $ abilityCardDef g (AttachmentRef aid)
        pure $ named && host == (VillainCharacter <$> friendly) && isJust friendly)
        [aid | W.Window W.After (W.AttachmentCounterAmountChanged aid old new) <- windows,new > old]
      updated <- runMessage msg a
      pure $ LeaderRewardUpgrade (updated `With` (headMay matching <|> energy))
    UpgradeMessage uid (PlayedUpgrade target) | uid == upgradeId a -> do
      updated <- runMessage msg a
      case toCardCode a of
        "56135" | isNothing target -> do
          allies <- filterM (\ally -> (\mods -> all (`notElem` mods) [CannotHaveAttachedCards,CannotHaveAttachedPlayerCards]) <$> getModifiers ally) =<< selectList AnyAlly
          if null allies then removeFromPlay a else chooseOne (upgradeController a)
            [targeting ally $ upgradeStep uid (UpgradeAttachedToAlly ally) | ally <- allies]
        "56213" -> do
          let named = case target of Just (VillainRef vid) -> Just vid; _ -> Nothing
          friendly <- maybe (friendlyLeaderFor $ upgradeController a) (pure . Just) named
          eligible <- friendlyLeaderExists (upgradeController a) (Just "56168")
          if not eligible || isNothing friendly then removeFromPlay a else for_ friendly $ \vid -> do
            when (isNothing target) $ upgradeStep uid (UpgradeAttachedToEnemy $ EnemyVillainId vid)
            giveStatus vid ToughStatus
        _ -> pure ()
      pure $ LeaderRewardUpgrade (updated `With` energy)
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      for_ (upgradeAttachedAlly a) (`heal` 1)
      pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> do
      for_ (upgradeAttachedEnemy a) $ \enemy -> do
        finesse <- inAnyArea $ selectList $ AttachmentOnEnemy enemy <> AttachmentIs C.cw56174
        traverse_ (`placeCounters` 2) finesse
      removeFromPlay a
      pure e
    _ -> (\updated -> LeaderRewardUpgrade (updated `With` energy)) <$> runMessage msg a
