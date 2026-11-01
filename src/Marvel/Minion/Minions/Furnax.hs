module Marvel.Minion.Minions.Furnax (
  furnax,
  Furnax (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Question ()
import Marvel.Ref ()
import Marvel.Window

furnax :: MinionCard Furnax
furnax = minion Furnax Cards.furnax (Sch 2) (Atk 2) (HP 6)

newtype Furnax = Furnax (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | [star] Forced Response: after Furnax activates (attacks, or schemes), deal 2 indirect
-- damage to each player.
instance HasAbilities Furnax where
  getAbilities (Furnax a) =
    [ windowAbility a 1 (EnemyAttacked After (EnemyWithId $ EnemyMinionId $ minionId a) AnyIdentity) ForcedResponse NoCost $
        runAbility a 1
    , windowAbility a 2 (MinionHasSchemed After (MinionWithId $ minionId a)) ForcedResponse NoCost $
        runAbility a 2
    ]

instance RunMessage Furnax where
  runMessage msg e@(Furnax attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) n _ _ | n `elem` [1, 2] -> do
      players <- getPlayers
      for_ players \p -> dealIndirectDamage p attrs 2
      pure e
    _ -> Furnax <$> runMessage msg attrs
