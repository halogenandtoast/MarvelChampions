module Marvel.Environment.Environments.CriminalEnterprise (
  criminalEnterprise,
  stateOfMadness,
  CriminalEnterprise (..),
  placeInfamy,
  infamyOrMadness,
  getInfamy,
  isNormanOsbornSide,
  isGreenGoblinSide,
) where

import Marvel.Prelude

import Marvel.Ability.Types
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Villain.Cards qualified as Villains

-- | Criminal Enterprise and, on its back, State of Madness: each enters play with
-- 2[per player] counters, infamy or madness, and with none left it flips, and the
-- villain with it.
criminalEnterprise :: EnvironmentCard CriminalEnterprise
criminalEnterprise = environment CriminalEnterprise Cards.criminalEnterprise

stateOfMadness :: EnvironmentCard CriminalEnterprise
stateOfMadness = environment CriminalEnterprise Cards.stateOfMadness

newtype CriminalEnterprise = CriminalEnterprise (Attrs Environment)
  deriving anyclass (IsEnvironment, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage CriminalEnterprise where
  runMessage msg e@(CriminalEnterprise attrs) = runQueueT $ case msg of
    EnvironmentMessage eid EnvironmentEntered | eid == environmentId attrs -> do
      entersWithCounters
      pure e
    -- the side turned up enters play with its counters
    EnvironmentMessage eid FlipEnvironment | eid == environmentId attrs -> do
      entersWithCounters
      CriminalEnterprise <$> runMessage msg attrs
    RemoveCounters target n | target == toRef attrs -> do
      when (n > 0 && environmentCounters attrs > 0 && subtractNatural n (environmentCounters attrs) == 0) do
        flipEnvironment (environmentId attrs)
        flipVillain =<< selectJust ActiveVillain
      CriminalEnterprise <$> runMessage msg attrs
    _ -> CriminalEnterprise <$> runMessage msg attrs
   where
    entersWithCounters = placeCounters attrs . fromIntegral =<< fromGameValue (PerPlayer 2)

-- | "Place n infamy counters on Criminal Enterprise": none can be while it is State of
-- Madness. Whether they could be.
placeInfamy :: (ReverseQueue m) => Natural -> m Bool
placeInfamy n =
  selectOne (EnvironmentIs Cards.criminalEnterprise) >>= \case
    Just e -> True <$ placeCounters e n
    Nothing -> pure False

-- | "Place n infamy counters on Criminal Enterprise. If you cannot, remove n madness
-- counters from State of Madness."
infamyOrMadness :: (ReverseQueue m) => Natural -> m ()
infamyOrMadness n = do
  placed <- placeInfamy n
  unless placed $ traverse_ (`removeCounters` n) =<< selectOne (EnvironmentIs Cards.stateOfMadness)

-- | The infamy counters on Criminal Enterprise: none while it is State of Madness.
getInfamy :: (HasGame m) => m Natural
getInfamy = maybe (pure 0) getCountersOn =<< selectOne (EnvironmentIs Cards.criminalEnterprise)

-- | "When Revealed (Norman Osborn)": the villain is on its Norman Osborn side.
isNormanOsbornSide :: (HasGame m) => m Bool
isNormanOsbornSide = anyM (selectAny . EnemyIs) [Villains.normanOsborn1, Villains.normanOsborn2, Villains.normanOsborn3]

-- | "When Revealed (Green Goblin)": the villain is on its Green Goblin side.
isGreenGoblinSide :: (HasGame m) => m Bool
isGreenGoblinSide = anyM (selectAny . EnemyIs) [Villains.greenGoblin1, Villains.greenGoblin2, Villains.greenGoblin3]
