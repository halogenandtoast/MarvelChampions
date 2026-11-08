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
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Resource.Types (Resource (Energy,Mental))
import Marvel.Trait qualified as T
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

newtype LeaderRewardUpgrade = LeaderRewardUpgrade (Attrs Upgrade `With` Maybe AttachmentId)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

leaderRewardUpgrades :: [UpgradeCard LeaderRewardUpgrade]
leaderRewardUpgrades = map (upgrade $ \a -> LeaderRewardUpgrade (a `With` Nothing)) (filter ((/= "57034") . toCardCode) R.rewardUpgrades)

instance IsUpgrade LeaderRewardUpgrade where
  toUpgradeAttrs (LeaderRewardUpgrade (a `With` _)) = a
  upgradeAttachmentTraits (LeaderRewardUpgrade (a `With` _)) =
    if toCardCode a == "56131" then Just (fromList [T.Tech]) else Nothing
  upgradeTargets (LeaderRewardUpgrade (a `With` _)) = case toCardCode a of
    "56135" -> do
      allies <- selectList AnyAlly
      eligible <- filterM (\ally -> (\mods -> all (`notElem` mods) [CannotHaveAttachedCards,CannotHaveAttachedPlayerCards,CannotHaveAttachedUpgrades]) <$> getModifiers ally) allies
      pure $ Just $ map toTarget eligible
    code | code `elem` ["56131","56213"] -> do
      eligible <- friendlyLeaderExists (upgradeController a) (Just $ if code == "56131" then "56059" else "56168")
      friendly <- friendlyLeaderFor (upgradeController a)
      pure $ Just [VillainRef vid | eligible,Just vid <- [friendly]]
    _ -> pure Nothing

instance HasModifiersFor LeaderRewardUpgrade where
  getModifiersFor _ target (LeaderRewardUpgrade (a `With` _))
    | toCardCode a == "56131",isTarget a target = pure [TraitModifier T.Tech]
  getModifiersFor _ _ _ = pure []

instance HasAbilities LeaderRewardUpgrade where
  getAbilities (LeaderRewardUpgrade (a `With` energy)) = case toCardCode a of
    code | code `elem` ["56133","56207"] ->
      [windowAbility a 4 (W.EnemyWouldAttack VillainEnemy $ IdentityWithId $ upgradeController a) ForcedInterrupt NoCost (runAbility a 4)
        & \ab -> ab {abilityCriteria = OwnsThis <> FriendlyLeaderExists (Just $ if code == "56133" then "56092" else "56137")}]
    "56131" -> [ability a 3 HeroAction ForAnyPlayer (MultiResourceCost [Just Energy,Just Mental]) (runAbility a 3)
      | isJust (upgradeAttachedEnemy a)]
    "56135" -> [windowAbility a 1 (W.AttachmentCountersPlaced W.After $ AttachmentWithId channel) Response NoCost (runAbility a 1)
      & \ability -> ability {abilityCriteria = FriendlyLeaderExists (Just "56092") <> CharacterExists (AllyCharacters (AllyWithId ally) <> healableCharacter)}
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
          allies <- filterM (\ally -> (\mods -> all (`notElem` mods) [CannotHaveAttachedCards,CannotHaveAttachedPlayerCards,CannotHaveAttachedUpgrades]) <$> getModifiers ally) =<< selectList AnyAlly
          if null allies then removeFromPlay a else chooseOne (upgradeController a)
            [targeting ally $ upgradeStep uid (UpgradeAttachedToAlly ally) | ally <- allies]
        "56131" -> when (isNothing target) $ do
          friendly <- friendlyLeaderFor (upgradeController a)
          eligible <- friendlyLeaderExists (upgradeController a) (Just "56059")
          if eligible then for_ friendly (upgradeStep uid . UpgradeAttachedToEnemy . EnemyVillainId) else removeFromPlay a
        "56213" -> do
          let named = case target of Just (VillainRef vid) -> Just vid; _ -> Nothing
          friendly <- maybe (friendlyLeaderFor $ upgradeController a) (pure . Just) named
          eligible <- friendlyLeaderExists (upgradeController a) (Just "56168")
          if not eligible || isNothing friendly then removeFromPlay a else for_ friendly $ \vid -> do
            when (isNothing target) $ upgradeStep uid (UpgradeAttachedToEnemy $ EnemyVillainId vid)
            giveStatus vid ToughStatus
        _ -> pure ()
      pure $ LeaderRewardUpgrade (updated `With` energy)
    RanAbility _ (isTarget a -> True) 4 windows _ -> do
      friendly <- friendlyLeaderFor (upgradeController a)
      let attackers = [vid | W.EnemyAttack (EnemyVillainId vid) you <- windows,you == upgradeController a]
      for_ (headMay attackers) $ \attacker -> for_ friendly $ \target -> do
        if toCardCode a == "56133"
          then capAttackDamageToEnemy (EnemyVillainId attacker) (EnemyVillainId target) 3
          else firstPlayerAtLeader target >>= (`returnCaptainAmericaShield` target)
        redirectVillainAttackToCharacter attacker (VillainCharacter target)
      removeFromPlay a
      pure e
    RanAbility _ (isTarget a -> True) 3 _ _ -> removeFromPlay a >> pure e
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
