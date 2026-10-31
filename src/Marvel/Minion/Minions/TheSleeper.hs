module Marvel.Minion.Minions.TheSleeper (
  theSleeper,
  TheSleeper (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

theSleeper :: MinionCard TheSleeper
theSleeper = minion TheSleeper Cards.theSleeper (Sch 1) (Atk 3) (HP 5)

newtype TheSleeper = TheSleeper (Attrs Minion)
  deriving anyclass (IsMinion, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | 5 hit points per player.
instance HasModifiersFor TheSleeper where
  getModifiersFor _ (MinionRef mid) (TheSleeper a) | mid == minionId a = do
    n <- length <$> getPlayers
    pure [HitPointModifier (5 * fromIntegral (n - 1)) | n > 1]
  getModifiersFor _ _ _ = pure []

instance RunMessage TheSleeper where
  runMessage msg e@(TheSleeper attrs) = runQueueT $ case msg of
    MinionMessage ident msg' | ident == minionId attrs -> case msg' of
      -- When Revealed: The Sleeper engages the first player, once it has engaged the
      -- player who revealed it
      RevealMinion revealer -> do
        first <- selectJust FirstPlayer
        when (first /= revealer) $
          flip afterMatching (engageMinion ident first) \case
            MinionMessage mid (MinionEngagedIdentity _) -> mid == ident
            _ -> False
        pure e
      -- When Defeated: removed from the game, not discarded
      MinionDefeated -> do
        minionLeavingPlayStep (minionId attrs) "removeDefeatedSleeper"
        pure e
      _ -> TheSleeper <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "removeDefeatedSleeper" _ -> do
      removeMinionFromGameWithoutLeaveWindow (minionId attrs)
      pure e
    _ -> TheSleeper <$> runMessage msg attrs
