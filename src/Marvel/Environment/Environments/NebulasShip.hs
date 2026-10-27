module Marvel.Environment.Environments.NebulasShip (
  nebulasShip,
  NebulasShip (..),
  placeEvasionCounters,
  getEvasionCounters,
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Phase
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref

nebulasShip :: EnvironmentCard NebulasShip
nebulasShip = environment NebulasShip Cards.nebulasShip

-- | Its counters are evasion counters.
newtype NebulasShip = NebulasShip (Attrs Environment)
  deriving anyclass (IsEnvironment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

-- | Shoot the Thrusters! — First Player Action: exhaust the Milano and spend up to 2
-- resources of any type → remove 1 evasion counter from here for each resource spent.
-- One ability for each number of resources (spending none would remove nothing), labeled
-- by it.
instance HasAbilities NebulasShip where
  getAbilities (NebulasShip a) =
    [ label (countLabel "shootTheThrusters" n) $
        ability a n Action (SelfMatches FirstPlayer) (ExhaustMilanoCost <> MultiResourceCost (replicate (fromIntegral n) Nothing)) $ runAbility a n
    | n <- [1, 2]
    ]

-- | Forced Interrupt: when the villain phase begins, place 1 evasion counter here.
instance RunMessage NebulasShip where
  runMessage msg e@(NebulasShip attrs) = runQueueT $ case msg of
    BeginPhase VillainPhase -> e <$ placeCounters attrs 1
    RanAbility _ (isTarget attrs -> True) n _ _ -> e <$ removeCounters attrs n
    _ -> NebulasShip <$> runMessage msg attrs

-- | "Place n evasion counters on Nebula's Ship": none without it in play.
placeEvasionCounters :: (ReverseQueue m) => Natural -> m ()
placeEvasionCounters n = selectOne (EnvironmentIs Cards.nebulasShip) >>= traverse_ (`placeCounters` n)

-- | The evasion counters on Nebula's Ship: none without it in play.
getEvasionCounters :: (HasGame m) => m Natural
getEvasionCounters = maybe (pure 0) getCountersOn =<< selectOne (EnvironmentIs Cards.nebulasShip)
