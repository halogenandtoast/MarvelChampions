module Marvel.Villain.Villains.AgeOfApocalypseDarkBeast where

import Marvel.Prelude
import Marvel.Card ()
import Marvel.Id (EnemyId (..))
import Marvel.Entity
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Ability
import Marvel.AgeOfApocalypse.Settings
import Marvel.Cost
import Marvel.Criteria
import Marvel.Difficulty
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype AoaDarkBeastVillain = AoaDarkBeastVillain (Attrs Villain)
  deriving anyclass (IsVillain, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

darkBeast1 :: VillainCard AoaDarkBeastVillain
darkBeast1 = villainWith AoaDarkBeastVillain Cards.darkBeast1 (Sch 2) (Atk 2) (HP $ PerPlayer 15) (stageL .~ 1)

darkBeast2 :: VillainCard AoaDarkBeastVillain
darkBeast2 = villainWith AoaDarkBeastVillain Cards.darkBeast2 (Sch 2) (Atk 2) (HP $ PerPlayer 18) (stageL .~ 2)

darkBeast3 :: VillainCard AoaDarkBeastVillain
darkBeast3 = villainWith AoaDarkBeastVillain Cards.darkBeast3 (Sch 3) (Atk 2) (HP $ PerPlayer 22) (stageL .~ 3)

instance HasAbilities AoaDarkBeastVillain where
  getAbilities a@(AoaDarkBeastVillain attrs) =
    [ windowAbility a 1 (W.VillainRevealed W.When (VillainWithId $ villainId attrs) W.RevealedFromVillain)
        WhenRevealed NoCost (runAbility a 1)
    , limitedWindowAbility a 2 (W.EnemyAttacked W.When (EnemyWithId $ EnemyVillainId $ villainId attrs) AnyIdentity)
        ForcedInterrupt NoCriteria NoCost (runAbility a 2)
    ]

instance RunMessage AoaDarkBeastVillain where
  runMessage msg e@(AoaDarkBeastVillain attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      cardSpecific GameRef "aoaRecruitSetting" ()
      when (villainStage attrs >= 2) $ getPlayers >>= traverse_ dealEncounterCard
      pure e
    RanAbility _ (isTarget attrs -> True) 2 windows _ -> do
      for_ ([you | W.EnemyAttack _ you <- windows] <> [you | W.EnemyAttackAlly _ _ you <- windows]) resolveSettingSpecial
      pure e
    VillainMessage vid VillainDefeated | vid == villainId attrs -> do
      difficulty <- getDifficulty
      case (difficulty, villainStage attrs) of
        (_, 1) -> advanceVillain vid >> pure (advanceVillainTo darkBeast2 attrs)
        (Expert, 2) -> advanceVillain vid >> pure (advanceVillainTo darkBeast3 attrs)
        _ -> AoaDarkBeastVillain <$> runMessage msg attrs
    _ -> AoaDarkBeastVillain <$> runMessage msg attrs
