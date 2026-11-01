module Marvel.Villain.Villains.NextEvolutionNeJuggernaut where

import Marvel.Prelude
import Marvel.Id
import Marvel.Status
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability
import Marvel.Attack
import Marvel.Cost
import Marvel.Count
import Marvel.Criteria
import Marvel.Damage
import Marvel.Difficulty
import Marvel.Game.Source
import Marvel.Identity.Types
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.NextEvolution.Juggernaut
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Resource.Types
import Marvel.Window qualified as W
import Marvel.Card
import Marvel.Entity
import Marvel.Villain.Cards qualified as Cards
import Marvel.Villain.Types
import Marvel.Ability (HasAbilities)
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype NeJuggernautVillain = NeJuggernautVillain (Attrs Villain)
  deriving anyclass (IsVillain)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

ne40118 :: VillainCard NeJuggernautVillain
ne40118 = villainWith NeJuggernautVillain Cards.ne40118 (Sch 1) (Atk 2) (HP $ PerPlayer 18) (stageL .~ 1)

ne40119 :: VillainCard NeJuggernautVillain
ne40119 = villainWith NeJuggernautVillain Cards.ne40119 (Sch 1) (Atk 3) (HP $ PerPlayer 21) (stageL .~ 2)

ne40120 :: VillainCard NeJuggernautVillain
ne40120 = villainWith NeJuggernautVillain Cards.ne40120 (Sch 2) (Atk 4) (HP $ PerPlayer 25) (stageL .~ 3)


instance HasAbilities NeJuggernautVillain where
  getAbilities a@(NeJuggernautVillain attrs) = [windowAbility a 1
    (W.VillainRevealed W.When (VillainWithId $ villainId attrs) W.RevealedFromVillain)
    WhenRevealed NoCost (runAbility a 1)]

instance HasModifiersFor NeJuggernautVillain where
  getModifiersFor _ (VillainRef vid) (NeJuggernautVillain attrs) | vid == villainId attrs = pure [AttackModifier $ villainCounters attrs]
  getModifiersFor _ _ _ = pure []

instance RunMessage NeJuggernautVillain where
  runMessage msg a@(NeJuggernautVillain attrs) = runQueueT $ case msg of
    VillainMessage vid VillainDefeated | vid == villainId attrs -> do
      difficulty <- getDifficulty
      case villainStage attrs of
        1 -> do
          advanceVillain vid
          let NeJuggernautVillain next = advanceVillainTo ne40119 attrs
          pure $ NeJuggernautVillain next {villainCounters = villainCounters attrs}
        2 | difficulty == Expert -> do
          advanceVillain vid
          let NeJuggernautVillain next = advanceVillainTo ne40120 attrs
          pure $ NeJuggernautVillain next {villainCounters = villainCounters attrs}
        _ -> NeJuggernautVillain <$> runMessage msg attrs
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      case villainStage attrs of
        1 -> placeCounters attrs 1 >> giveStatus (EnemyVillainId $ villainId attrs) ToughStatus
        2 -> do
          placeCounters attrs 1
          exposed <- juggernautExposed
          if exposed then restoreJuggernautHelmet else giveStatus (EnemyVillainId $ villainId attrs) ToughStatus
        _ -> do
          first <- selectJust FirstPlayer
          searchAndRevealEncounterCard first (CardWithTitle "Head of Steam")
          cardSpecific attrs "stageThreeHelmet" ()
      pure a
    CardSpecific (isTarget attrs -> True) "stageThreeHelmet" _ -> do
      exposed <- juggernautExposed
      if exposed then restoreJuggernautHelmet else giveStatus (EnemyVillainId $ villainId attrs) ToughStatus
      pure a
    _ -> NeJuggernautVillain <$> runMessage msg attrs
