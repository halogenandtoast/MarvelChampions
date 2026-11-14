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
import Marvel.Payment.Types (paymentX)
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref ()

nebulasShip :: EnvironmentCard NebulasShip
nebulasShip = environment NebulasShip Cards.nebulasShip

-- | Its counters are evasion counters.
newtype NebulasShip = NebulasShip (Attrs Environment)
  deriving anyclass (IsEnvironment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

-- | Shoot the Thrusters! — First Player Action: exhaust the Milano and spend up to 2
-- resources of any type → remove 1 evasion counter from here for each resource spent.
-- One action, its resources counted as they are paid (spending none would remove nothing).
instance HasAbilities NebulasShip where
  getAbilities (NebulasShip a) =
    [ label "$label.shootTheThrusters" $
        ability a 1 Action (SelfMatches FirstPlayer) (ExhaustMilanoCost <> UpToResourcesCost 2) $ runAbility a 1
    ]

-- | Forced Interrupt: when the villain phase begins, place 1 evasion counter here.
instance RunMessage NebulasShip where
  runMessage msg e@(NebulasShip attrs) = runQueueT $ case msg of
    BeginPhase VillainPhase -> e <$ placeCounters attrs 1
    RanAbility _ (isTarget attrs -> True) 1 _ payment -> e <$ removeCounters attrs (paymentX payment)
    _ -> NebulasShip <$> runMessage msg attrs

-- | "Place n evasion counters on Nebula's Ship": none without it in play.
placeEvasionCounters :: (ReverseQueue m) => Natural -> m ()
placeEvasionCounters n = selectOne (EnvironmentIs Cards.nebulasShip) >>= traverse_ (`placeCounters` n)

-- | The evasion counters on Nebula's Ship: none without it in play.
getEvasionCounters :: (HasGame m) => m Natural
getEvasionCounters = maybe (pure 0) getCountersOn =<< selectOne (EnvironmentIs Cards.nebulasShip)
