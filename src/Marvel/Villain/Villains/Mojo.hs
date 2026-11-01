module Marvel.Villain.Villains.Mojo where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Game.Source
import Marvel.Cost
import Marvel.Difficulty
import Marvel.EncounterSet qualified as Set
import Marvel.Entity
import Marvel.GameValue
import Marvel.Hp
import Marvel.Stats
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window qualified as W

newtype MojoVillain = MojoVillain (Attrs Villain)
  deriving anyclass (IsVillain, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

mojo39022, mojo39023, mojo39024 :: VillainCard MojoVillain
mojo39022 = villainWith MojoVillain Cards.mojo39022 (Sch 2) (Atk 1) (HP $ PerPlayer 16) (stageL .~ 1)
mojo39023 = villainWith MojoVillain Cards.mojo39023 (Sch 3) (Atk 1) (HP $ PerPlayer 18) (stageL .~ 2)
mojo39024 = villainWith MojoVillain Cards.mojo39024 (Sch 4) (Atk 2) (HP $ PerPlayer 25) (stageL .~ 3)

instance HasAbilities MojoVillain where
  getAbilities (MojoVillain a) =
    [windowAbility a 1 (W.TurnEnds W.After HeroIdentity) ForcedResponse NoCost (runAbility a 1)]
    <> [windowAbility a 2 (W.VillainRevealed W.When (VillainWithId $ villainId a) W.RevealedFromVillain) WhenRevealed NoCost (runAbility a 2) | villainStage a > 1]

instance RunMessage MojoVillain where
  runMessage msg e@(MojoVillain a) = runQueueT $ case msg of
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      let player = fromMaybe you $ headMay [p | W.TurnEnded p <- windows]
      discardTopOfEncounterDeckThen (villainStage a + 2) $ \cards ->
        placeCharacterThreat player (fromIntegral $ length $ filter ((/= Just Set.Mojo) . cdEncounterSet . getCardDef) cards)
      pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> do
      characters <- selectList (CharacterControlledBy AnyIdentity)
      traverse_ (\c -> placeCharacterThreat c (villainStage a)) characters
      pure e
    VillainMessage vid VillainDefeated | vid == villainId a -> do
      difficulty <- getDifficulty
      case villainStage a of
        1 -> advanceVillain vid >> pure (advanceVillainTo mojo39023 a)
        2 | difficulty == Expert -> advanceVillain vid >> pure (advanceVillainTo mojo39024 a)
        _ -> MojoVillain <$> runMessage msg a
    _ -> MojoVillain <$> runMessage msg a
