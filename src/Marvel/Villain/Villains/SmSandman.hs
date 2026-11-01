module Marvel.Villain.Villains.SmSandman where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attack
import Marvel.Cost
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Environment.Environments.SmSandman (surgingSands, cityStreets)
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Stats
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window
import Marvel.Window qualified as W ()

newtype SmSandmanVillain = SmSandmanVillain (Attrs Villain)
  deriving anyclass (IsVillain)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

smSandman1, smSandman2, smSandman3 :: VillainCard SmSandmanVillain
smSandman1 = villainWith SmSandmanVillain Cards.smSandman1 (Sch 1) (Atk 2) (HP $ PerPlayer 16) (stageL .~ 1)
smSandman2 = villainWith SmSandmanVillain Cards.smSandman2 (Sch 1) (Atk 3) (HP $ PerPlayer 18) (stageL .~ 2)
smSandman3 = villainWith SmSandmanVillain Cards.smSandman3 (Sch 1) (Atk 3) (HP $ PerPlayer 19) (stageL .~ 3)

instance HasModifiersFor SmSandmanVillain where
  getModifiersFor _ target (SmSandmanVillain attrs)
    | target == toRef attrs, villainStage attrs >= 3 = pure [KeywordModifier Overkill]
  getModifiersFor _ _ _ = pure []

instance HasAbilities SmSandmanVillain where
  getAbilities (SmSandmanVillain a) =
    [windowAbility a 1 (VillainRevealed When (VillainWithId vid) RevealedFromVillain) WhenRevealed NoCost (runAbility a 1) | villainStage a >= 2]
      <> [windowAbility a 2 (EnemyAttackedAndDamaged (EnemyWithId $ EnemyVillainId vid) $ IdentityCharacters AnyIdentity) ForcedResponse NoCost (runAbility a 2)]
   where vid = villainId a

instance RunMessage SmSandmanVillain where
  runMessage msg e@(SmSandmanVillain attrs) = runQueueT $ case msg of
    VillainMessage vid VillainDefeated | vid == villainId attrs -> do
      difficulty <- getDifficulty
      case (difficulty, villainStage attrs) of
        (Normal, 1) -> advanceVillain vid >> pure (advanceVillainTo smSandman2 attrs)
        (Expert, 2) -> advanceVillain vid >> pure (advanceVillainTo smSandman3 attrs)
        _ -> SmSandmanVillain <$> runMessage msg attrs
    VillainMessage vid step | vid == villainId attrs, attacking step -> do
      attrs' <- runMessage msg attrs
      pure . SmSandmanVillain $ attrs' & attackingL . _Just %~ \attack ->
        if villainStage attrs >= 3 then gainKeyword Overkill attack else attack {attackIndirect = True}
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      when (villainStage attrs >= 3) $ cityStreets >>= traverse_ (`placeCounters` 1)
      surgingSands
      pure e
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> e <$ surgingSands
    _ -> SmSandmanVillain <$> runMessage msg attrs
   where
    attacking = \case
      VillainAttacks _ -> True
      VillainAttacksWithoutBoost _ -> True
      VillainAttacksGaining _ _ -> True
      VillainBeginAttack _ -> True
      VillainBeginAttackWithoutBoost _ -> True
      _ -> False
