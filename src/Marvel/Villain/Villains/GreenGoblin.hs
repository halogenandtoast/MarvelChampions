-- | Mutagen Formula's villain.
module Marvel.Villain.Villains.GreenGoblin where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Id (EnemyId (..))
import Marvel.Question (runAbility)
import Marvel.Cost
import Marvel.Damage
import Marvel.Difficulty
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Stats
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Window qualified as W

newtype GreenGoblin = GreenGoblin (Attrs Villain)
  deriving anyclass (IsVillain, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

greenGoblin1, greenGoblin2, greenGoblin3 :: VillainCard GreenGoblin
greenGoblin1 = villain GreenGoblin Cards.greenGoblinMutagen1 (Sch 1) (Atk 2) (HP $ PerPlayer 16)
greenGoblin2 = villainWith GreenGoblin Cards.greenGoblinMutagen2 (Sch 2) (Atk 2) (HP $ PerPlayer 18) (stageL .~ 2)
greenGoblin3 = villainWith GreenGoblin Cards.greenGoblinMutagen3 (Sch 2) (Atk 3) (HP $ PerPlayer 20) (stageL .~ 3)

instance HasAbilities GreenGoblin where
  getAbilities (GreenGoblin a) =
    -- "After Green Goblin attacks and damages you, place threat on the main scheme"
    windowAbility a 1 (W.TookDamage You FromAttack self) ForcedResponse NoCost (runAbility a 1)
      : [ windowAbility a 2 (W.VillainRevealed W.When (VillainWithId $ villainId a) W.RevealedFromVillain) WhenRevealed NoCost $
            runAbility a 2
        | villainStage a > 1
        ]
   where
    self = EnemyWithId $ EnemyVillainId $ villainId a

instance RunMessage GreenGoblin where
  runMessage msg e@(GreenGoblin attrs) = runQueueT $ case msg of
    VillainMessage ident VillainDefeated | ident == villainId attrs -> do
      difficulty <- getDifficulty
      case (difficulty, villainStage attrs) of
        (_, 1) -> do
          advanceVillain ident
          pure $ advanceVillainTo greenGoblin2 attrs
        (Expert, 2) -> do
          advanceVillain ident
          pure $ advanceVillainTo greenGoblin3 attrs
        _ -> GreenGoblin <$> runMessage msg attrs
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      scheme <- selectJust EncounterMainScheme
      placeThreat scheme (if villainStage attrs == 3 then 2 else 1)
      pure e
    -- "When Revealed: Deal 2 (3) encounter cards to each player"
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> do
      players <- getPlayers
      for_ players \p -> for_ [1 .. if villainStage attrs == 3 then 3 else 2 :: Int] \_ -> dealEncounterCard p
      pure e
    _ -> GreenGoblin <$> runMessage msg attrs
