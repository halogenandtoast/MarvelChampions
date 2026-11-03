module Marvel.Shadowland.Corruption where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Shadowland.Cards qualified as Cards
import Marvel.Trait qualified as T
import Marvel.Upgrade.Types

corruption :: HasGame m => IdentityId -> m (Maybe UpgradeId)
corruption you = selectOne (UpgradeControlledBy (IdentityWithId you) <> UpgradeNamed "Creeping Corruption")

corrupt :: ReverseQueue m => IdentityId -> Natural -> m ()
corrupt you n = corruption you >>= traverse_ (`placeCounters` n)

purify :: ReverseQueue m => IdentityId -> Natural -> m ()
purify you n = corruption you >>= traverse_ (`removeCounters` n)

overpowered :: HasGame m => IdentityId -> m Bool
overpowered you = selectAny (UpgradeControlledBy (IdentityWithId you) <> UpgradeWithTrait T.Overpowering <> UpgradeNamed "Creeping Corruption")

corruptionChoice :: ReverseQueue m => IdentityId -> Natural -> m () -> m ()
corruptionChoice you n other = do
  present <- isJust <$> corruption you
  chooseOne you $ [labeled "$label.slCorrupt" (corrupt you n) | present] <> [labeled "$label.slResist" other]

newtype CreepingCorruption = CreepingCorruption (Attrs Upgrade)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass (IsUpgrade, HasModifiersFor)
creepingCorruption :: [UpgradeCard CreepingCorruption]
creepingCorruption = map (upgrade CreepingCorruption) [Cards.street65072a,Cards.street65072b]
instance HasAbilities CreepingCorruption where
  getAbilities (CreepingCorruption a) = [ability a 1 Action OwnsThis ExhaustCost (runAbility a 1)]
instance RunMessage CreepingCorruption where
  runMessage msg e@(CreepingCorruption a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      placeCounters a 1
      drawCards you (if toCardCode a == "65072a" then 1 else 2)
      when (toCardCode a == "65072b") $ discardFromHand you 1
      pure e
    UpgradeMessage uid (AddUpgradeUses n) | uid == upgradeId a -> do
      let total = upgradeUses a + n
      if total >= 5 then
        if toCardCode a == "65072a" then pure $ CreepingCorruption a {upgradeUses = 0,upgradeCardDef = Cards.street65072b}
        else defeatIdentity (upgradeController a) >> pure (CreepingCorruption a {upgradeUses = total})
      else pure $ CreepingCorruption a {upgradeUses = total}
    _ -> CreepingCorruption <$> runMessage msg a
