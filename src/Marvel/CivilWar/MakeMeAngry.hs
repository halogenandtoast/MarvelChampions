module Marvel.CivilWar.MakeMeAngry where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.CivilWar.Helpers
import Marvel.CivilWar.RewardCards qualified as C
import Marvel.Cost
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Upgrade.Types
import Marvel.Window qualified as W

newtype MakeMeAngry = MakeMeAngry (Attrs Upgrade `With` (Bool,Bool))
  deriving anyclass HasModifiersFor
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

makeMeAngry :: UpgradeCard MakeMeAngry
makeMeAngry = upgrade (\a -> MakeMeAngry (a `With` (False,False))) C.makeMeAngry

instance IsUpgrade MakeMeAngry where
  toUpgradeAttrs (MakeMeAngry (a `With` _)) = a
  upgradeTargets (MakeMeAngry (a `With` _)) = do
    eligible <- friendlyLeaderExists (upgradeController a) (Just "57001")
    leader <- friendlyLeaderFor (upgradeController a)
    targets <- filterM (\vid -> (\mods -> all (`notElem` mods) [CannotHaveAttachedCards,CannotHaveAttachedPlayerCards]) <$> getModifiers vid)
      [vid | eligible,Just vid <- [leader]]
    pure $ Just $ map VillainRef targets

instance HasAbilities MakeMeAngry where
  getAbilities (MakeMeAngry (a `With` (pending,_))) =
    [windowAbility a 1 (W.EnemyWasAttackedByHeroOrLeader W.After $ EnemyWithId enemy) ForcedResponse NoCost (runAbility a 1)
    | not pending,Just enemy <- [upgradeAttachedEnemy a]]

instance RunMessage MakeMeAngry where
  runMessage msg e@(MakeMeAngry (a `With` (pending,performed))) = runQueueT $ case msg of
    UpgradeMessage uid (PlayedUpgrade Nothing) | uid == upgradeId a -> do
      eligible <- friendlyLeaderExists (upgradeController a) (Just "57001")
      leader <- friendlyLeaderFor (upgradeController a)
      if eligible then traverse_ (upgradeStep uid . UpgradeAttachedToEnemy . EnemyVillainId) leader else removeFromPlay a
      pure e
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      let heroes = [IdentityCharacter you | W.PlayerAttackResolved source you _ <- windows,case source of AllyRef _ -> False; _ -> True]
          leaders = [VillainCharacter vid | W.EnemyAttackOnEnemy (EnemyVillainId vid) _ <- windows]
      for_ [(vid,attacker) | Just (EnemyVillainId vid) <- [upgradeAttachedEnemy a],attacker <- take 1 $ heroes <> leaders] $ \(vid,attacker) -> do
        let complete = cardSpecific a "angryComplete" ()
        case attacker of
          IdentityCharacter you -> villainAttacksCharacterThen vid you attacker complete
          VillainCharacter other -> leaderAttacksLeaderThen vid other complete
          _ -> pure ()
      pure $ MakeMeAngry (a `With` (True,False))
    CheckWindows windows | pending -> do
      let attacked = any (\case
            W.Window W.After (W.EnemyAttack enemy _) -> Just enemy == upgradeAttachedEnemy a
            W.Window W.After (W.EnemyAttackAlly enemy _ _) -> Just enemy == upgradeAttachedEnemy a
            W.Window W.After (W.EnemyAttackOnEnemy enemy _) -> Just enemy == upgradeAttachedEnemy a
            _ -> False) windows
      updated <- runMessage msg a
      pure $ MakeMeAngry (updated `With` (pending,performed || attacked))
    CardSpecific (isTarget a -> True) "angryComplete" _ -> do
      when performed $ removeFromPlay a
      pure $ MakeMeAngry (a `With` (False,False))
    _ -> (\updated -> MakeMeAngry (updated `With` (pending,performed))) <$> runMessage msg a
