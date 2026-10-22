module Marvel.SideScheme.SideSchemes.TheSleeperAwakened (
  theSleeperAwakened,
  TheSleeperAwakened (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

theSleeperAwakened :: SideSchemeCard TheSleeperAwakened
theSleeperAwakened = sideScheme TheSleeperAwakened Cards.theSleeperAwakened (Static 0)

newtype TheSleeperAwakened = TheSleeperAwakened (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | This scheme cannot leave play while The Sleeper is in play: its threat can be
-- removed, but it is not defeated. When Revealed: put The Sleeper into play engaged with
-- the first player. When The Sleeper is defeated, remove this card from the game.
instance RunMessage TheSleeperAwakened where
  runMessage msg s@(TheSleeperAwakened attrs) = runQueueT $ case msg of
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs -> case msg' of
      RevealSideScheme -> do
        first <- selectJust FirstPlayer
        sleeper <- filter (cardMatch (CardIs Minions.theSleeper)) <$> getSetAsideEncounterCards
        for_ (take 1 sleeper) (putEncounterCardIntoPlay first)
        pure s
      SideSchemeThwarted _ n -> do
        sleeperInPlay <- selectAny (MinionIs Minions.theSleeper)
        if sleeperInPlay
          then pure . TheSleeperAwakened $ attrs & threatL %~ subtractNatural n
          else TheSleeperAwakened <$> runMessage msg attrs
      _ -> TheSleeperAwakened <$> runMessage msg attrs
    MinionMessage mid MinionDefeated -> do
      isSleeper <- minionMatches (MinionIs Minions.theSleeper) mid
      when isSleeper $ removeFromGame attrs
      pure s
    _ -> TheSleeperAwakened <$> runMessage msg attrs
