module Marvel.Minion.Minions.SandmanSinister where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Types
import Marvel.Minion.Runner
import Marvel.Damage
import Marvel.Window qualified as W

sandmanSinister :: MinionCard SandmanSinister
sandmanSinister = minion SandmanSinister Cards.sandmanSinister (Sch 1) (Atk 1) (HP 7)

newtype SandmanSinister = SandmanSinister (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SandmanSinister where
  getAbilities a@(SandmanSinister attrs) =
    [ windowAbility a 1 (W.EnemyTookDamage (EnemyWithId $ EnemyMinionId $ minionId attrs) DamageFromAnAttack)
        ForcedResponse NoCost (runAbility a 1)
    ]

instance RunMessage SandmanSinister where
  runMessage msg e@(SandmanSinister attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> e <$ discardTopOfEncounterDeck 7
    Boost (RevealedAsBoost target _) | isTarget attrs target -> e <$ discardTopOfEncounterDeck 7
    _ -> SandmanSinister <$> runMessage msg attrs
