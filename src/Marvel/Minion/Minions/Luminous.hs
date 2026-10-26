module Marvel.Minion.Minions.Luminous (
  luminous,
  Luminous (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Window

luminous :: MinionCard Luminous
luminous = minion Luminous Cards.luminous (Sch 2) (Atk 2) (HP 5)

newtype Luminous = Luminous (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Forced Response: after Luminous activates against you (attacks you, or schemes while
-- engaged with you), discard the top card of the encounter deck; 2 or more boost icons
-- on it, deal yourself 1 encounter card.
instance HasAbilities Luminous where
  getAbilities (Luminous a) =
    [ windowAbility a 1 (EnemyAttacked After (EnemyWithId $ EnemyMinionId $ minionId a) AnyIdentity) ForcedResponse NoCost
        $ RunAbility (toTarget a) 1
    , windowAbility a 2 (MinionHasSchemed After (MinionWithId $ minionId a)) ForcedResponse NoCost
        $ RunAbility (toTarget a) 2
    ]

instance RunMessage Luminous where
  runMessage msg e@(Luminous attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 [EnemyAttack _ you] _ -> do
      discardTopOfEncounterDeckThen 1 (countBoostIcons attrs you)
      pure e
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> do
      discardTopOfEncounterDeckThen 1 (countBoostIcons attrs (minionEngagedIdentity attrs))
      pure e
    WithBoostIcons (isTarget attrs -> True) (fromJSON -> Success you) (sum . map snd -> n) -> do
      when (n >= 2) $ dealEncounterCard you
      pure e
    _ -> Luminous <$> runMessage msg attrs
