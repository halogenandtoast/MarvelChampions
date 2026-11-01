module Marvel.Villain.Villains.NextEvolutionMisterSinister where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword qualified as Keyword
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Status
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.NextEvolution.Sinister

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

newtype MisterSinisterVillain = MisterSinisterVillain (Attrs Villain)
  deriving anyclass (IsVillain, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

ne40136 :: VillainCard MisterSinisterVillain
ne40136 = villainWith MisterSinisterVillain Cards.ne40136 (Sch 2) (Atk 1) (HP $ PerPlayer 14) (stageL .~ 1)

ne40137 :: VillainCard MisterSinisterVillain
ne40137 = villainWith MisterSinisterVillain Cards.ne40137 (Sch 2) (Atk 2) (HP $ PerPlayer 17) (stageL .~ 2)

ne40138 :: VillainCard MisterSinisterVillain
ne40138 = villainWith MisterSinisterVillain Cards.ne40138 (Sch 3) (Atk 2) (HP $ PerPlayer 21) (stageL .~ 3)

instance RunMessage MisterSinisterVillain where
  runMessage msg card@(MisterSinisterVillain a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      n <- superpowers (villainId a)
      count <- fromIntegral <$> getStartingPlayerCount
      let amount = villainStage a - 1 + if n < 2 then 1 else 0
      placeThreatOnEach a (amount * count) MainScheme
      pure card
    _ -> do
      a' <- runMessage msg a
      let statuses attrs = fromEnum (villainTough attrs) + fromEnum (villainStunned attrs) + fromEnum (villainConfused attrs)
            + fromEnum (villainAdditionalStun attrs) + fromEnum (villainAdditionalConfuse attrs)
      when (statuses a' > statuses a) $ forcedResponse a $ placeThreatOnEach a (villainStage a) MainScheme
      pure (MisterSinisterVillain a')

instance HasAbilities MisterSinisterVillain where
  getAbilities (MisterSinisterVillain a) =
    [limitedWindowAbility a 1 (W.VillainRevealed W.When (VillainWithId $ villainId a) W.RevealedFromVillain)
      ForcedResponse NoCriteria NoCost (runAbility a 1) | villainStage a >= 2]
