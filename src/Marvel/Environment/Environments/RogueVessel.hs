module Marvel.Environment.Environments.RogueVessel (
  rogueVessel,
  RogueVessel (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Phase
import Marvel.Question (runAbility)
import Marvel.Ref ()
import Marvel.Window qualified as W

rogueVessel :: EnvironmentCard RogueVessel
rogueVessel = environment RogueVessel Cards.rogueVessel

-- | Surge (Ship Command), its card's. First Player Action: exhaust the Milano and spend
-- 2 resources of any type → discard this card.
newtype RogueVessel = RogueVessel (Attrs Environment)
  deriving anyclass (IsEnvironment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance HasAbilities RogueVessel where
  getAbilities (RogueVessel a) =
    [ ability a 1 Action (SelfMatches FirstPlayer) (ExhaustMilanoCost <> MultiResourceCost [Nothing, Nothing]) $ runAbility a 1
    , -- Forced Interrupt: when the villain phase ends, deal 1 damage to each player.
      windowAbility a 2 (W.PhaseEnds W.When VillainPhase) ForcedInterrupt NoCost $ runAbility a 2
    ]

instance RunMessage RogueVessel where
  runMessage msg e@(RogueVessel attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> e <$ removeFromPlay attrs
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> do
      players <- getPlayers
      for_ players \p -> damageIdentity p attrs (toDamage 1 FromAbility)
      pure e
    _ -> RogueVessel <$> runMessage msg attrs
