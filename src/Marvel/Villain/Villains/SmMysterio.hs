module Marvel.Villain.Villains.SmMysterio where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost.Types
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Stats
import Marvel.Trait (Trait (Illusion))
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window qualified as W

newtype SmMysterioVillain = SmMysterioVillain (Attrs Villain)
  deriving anyclass (IsVillain, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

smMysterio1 :: VillainCard SmMysterioVillain
smMysterio1 = villainWith SmMysterioVillain Cards.smMysterio1 (Sch 2) (Atk 1) (HP $ PerPlayer 15) (stageL .~ 1)

smMysterio2 :: VillainCard SmMysterioVillain
smMysterio2 = villainWith SmMysterioVillain Cards.smMysterio2 (Sch 2) (Atk 2) (HP $ PerPlayer 17) (stageL .~ 2)

smMysterio3 :: VillainCard SmMysterioVillain
smMysterio3 = villainWith SmMysterioVillain Cards.smMysterio3 (Sch 2) (Atk 2) (HP $ PerPlayer 16) (stageL .~ 3)

instance HasAbilities SmMysterioVillain where
  getAbilities a@(SmMysterioVillain attrs) =
    [windowAbility a 1 (W.VillainRevealed W.When (VillainWithId $ villainId attrs) W.RevealedFromVillain)
      WhenRevealed NoCost (runAbility a 1) | villainStage attrs >= 2]

instance RunMessage SmMysterioVillain where
  runMessage msg v@(SmMysterioVillain attrs) = runQueueT $ case msg of
    VillainMessage vid VillainDefeated | vid == villainId attrs -> do
      difficulty <- getDifficulty
      case (difficulty, villainStage attrs) of
        (Normal, 1) -> do
          advanceVillain vid
          pure $ advanceVillainTo smMysterio2 attrs
        (Expert, 2) -> do
          advanceVillain vid
          pure $ advanceVillainTo smMysterio3 attrs
        _ -> SmMysterioVillain <$> runMessage msg attrs
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      players <- getPlayers
      if villainStage attrs == 2
        then for_ players (`shuffleTopEncounterCardsIntoPlayerDeck` 1)
        else for_ players (`discardTopOfDeck` 5)
      pure v
    BoostCardResolved enemy card
      | enemy == EnemyVillainId (villainId attrs), cardMatch (CardWithTrait Illusion) card -> do
          you <- getBoostedPlayer enemy
          case villainStage attrs of
            1 -> discardBoostToPlayerDiscard card you
            2 -> putBoostOnBottomOfPlayerDeck card you
            _ -> putBoostOnTopOfPlayerDeck card you
          pure v
    _ -> SmMysterioVillain <$> runMessage msg attrs
