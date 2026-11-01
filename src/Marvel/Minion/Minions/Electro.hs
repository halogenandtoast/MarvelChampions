module Marvel.Minion.Minions.Electro (
  electro,
  Electro (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Def ()
import Marvel.Cost
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Window

electro :: MinionCard Electro
electro = minion Electro Cards.electro (Sch 2) (Atk 2) (HP 6)

newtype Electro = Electro (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | [star] Forced Response: after Electro attacks you, discard 1 card from the encounter
-- deck and take 1 indirect damage for each boost icon on it.
instance HasAbilities Electro where
  getAbilities (Electro a) =
    [ windowAbility a 1 (EnemyAttacked After (EnemyWithId $ EnemyMinionId $ minionId a) AnyIdentity) ForcedResponse NoCost
        $ RunAbility (toTarget a) 1
    ]

instance RunMessage Electro where
  runMessage msg e@(Electro attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 [EnemyAttack _ you] _ -> do
      discardTopOfEncounterDeckThen 1 (countBoostIcons attrs you)
      pure e
    WithBoostIcons (isTarget attrs -> True) (fromJSON -> Success you) (sum . map snd -> n) -> do
      when (n > 0) $ dealIndirectDamage you attrs n
      pure e
    -- [star] Boost: discard 3 cards from the encounter deck
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      discardTopOfEncounterDeck 3
      pure e
    _ -> Electro <$> runMessage msg attrs
