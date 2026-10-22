module Marvel.Minion.Minions.HydraFlameSoldier (
  hydraFlameSoldier,
  HydraFlameSoldier (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Cost
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Window

hydraFlameSoldier :: MinionCard HydraFlameSoldier
hydraFlameSoldier = minion HydraFlameSoldier Cards.hydraFlameSoldier (Sch 1) (Atk 1) (HP 4)

newtype HydraFlameSoldier = HydraFlameSoldier (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | [star] Forced Response: after Hydra Flame-Soldier makes an undefended attack against
-- you, discard a support you control.
instance HasAbilities HydraFlameSoldier where
  getAbilities (HydraFlameSoldier a) =
    [ windowAbility
        a
        1
        (EnemyAttacked After (EnemyWithId (EnemyMinionId $ minionId a) <> UndefendedEnemy) You)
        ForcedResponse
        NoCost
        $ RunAbility (toTarget a) 1
    ]

-- | The player discards a support of their choice, if they control any.
discardSupport :: (ReverseQueue m) => IdentityId -> m ()
discardSupport you = do
  supports <- selectList $ SupportControlledBy (IdentityWithId you)
  unless (null supports) $ chooseOrRunOne you [targeting s (removeFromPlay s) | s <- supports]

instance RunMessage HydraFlameSoldier where
  runMessage msg e@(HydraFlameSoldier attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      discardSupport you
      pure e
    -- [star] Boost: if this card resolves during an undefended attack, discard a support
    -- you control
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      undefended <- selectAny $ EnemyWithId enemy <> UndefendedEnemy
      when undefended $ discardSupport =<< getBoostedPlayer enemy
      pure e
    _ -> HydraFlameSoldier <$> runMessage msg attrs
