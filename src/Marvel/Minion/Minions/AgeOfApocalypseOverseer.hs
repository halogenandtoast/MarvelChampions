module Marvel.Minion.Minions.AgeOfApocalypseOverseer where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Ability (HasAbilities)
import Marvel.AgeOfApocalypse.MissionTypes
import Marvel.Game.Source
import Marvel.Hp
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner ()
import Marvel.Minion.Types
import Marvel.Modifier
import Marvel.Stats

newtype AoaOverseerMinion = AoaOverseerMinion (Attrs Minion)
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

misterSinisterMinion :: MinionCard AoaOverseerMinion
misterSinisterMinion = minion AoaOverseerMinion Cards.misterSinisterMinion (Sch 0) (Atk 0) (HP 5)

theShadowKingMinion :: MinionCard AoaOverseerMinion
theShadowKingMinion = minion AoaOverseerMinion Cards.theShadowKingMinion (Sch 0) (Atk 0) (HP 5)

abyssMinion :: MinionCard AoaOverseerMinion
abyssMinion = minion AoaOverseerMinion Cards.abyssMinion (Sch 0) (Atk 0) (HP 5)

sugarManMinion :: MinionCard AoaOverseerMinion
sugarManMinion = minion AoaOverseerMinion Cards.sugarManMinion (Sch 0) (Atk 0) (HP 5)

mikhailRasputinMinion :: MinionCard AoaOverseerMinion
mikhailRasputinMinion = minion AoaOverseerMinion Cards.mikhailRasputinMinion (Sch 0) (Atk 0) (HP 5)

instance HasModifiersFor AoaOverseerMinion where
  getModifiersFor _ target (AoaOverseerMinion a) | target == toRef a = do
    count <- fromIntegral <$> getStartingPlayerCount
    mission <- getMission
    others <- case mission of
      Nothing -> pure False
      Just m -> anyM (\mid -> if mid == minionId a then pure False else isJust <$> getsGame (\g -> cardInPlay g (MinionRef mid))) m.missionMinions
    pure $ [CannotActivate, HitPointModifier (5 * (count - 1))] <> [CannotTakeDamage | others]
  getModifiersFor _ _ _ = pure []

instance RunMessage AoaOverseerMinion where
  runMessage msg (AoaOverseerMinion a) = AoaOverseerMinion <$> runMessage msg a

instance IsMinion AoaOverseerMinion where
  minionBaseHitPoints _ = perPlayerMinionBaseHitPoints 5
