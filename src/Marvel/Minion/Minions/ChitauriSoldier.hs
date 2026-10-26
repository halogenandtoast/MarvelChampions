module Marvel.Minion.Minions.ChitauriSoldier (
  chitauriSoldier,
  ChitauriSoldier (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Def
import Marvel.Cost
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Window

chitauriSoldier :: MinionCard ChitauriSoldier
chitauriSoldier = minion ChitauriSoldier Cards.chitauriSoldier (Sch 1) (Atk 1) (HP 3)

newtype ChitauriSoldier = ChitauriSoldier (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | [star] Forced Interrupt: when Chitauri Soldier attacks you, discard the top card of
-- the encounter deck and take indirect damage equal to its boost icons.
instance HasAbilities ChitauriSoldier where
  getAbilities (ChitauriSoldier a) =
    [ windowAbility a 1 (EnemyAttacked When (EnemyWithId $ EnemyMinionId $ minionId a) AnyIdentity) ForcedInterrupt NoCost
        $ RunAbility (toTarget a) 1
    ]

instance RunMessage ChitauriSoldier where
  runMessage msg e@(ChitauriSoldier attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 [EnemyAttack _ you] _ -> do
      discardTopOfEncounterDeckThen 1 (countBoostIcons attrs you)
      pure e
    WithBoostIcons (isTarget attrs -> True) (fromJSON -> Success you) (sum . map snd -> n) -> do
      when (n > 0) $ dealIndirectDamage you attrs n
      pure e
    _ -> ChitauriSoldier <$> runMessage msg attrs
