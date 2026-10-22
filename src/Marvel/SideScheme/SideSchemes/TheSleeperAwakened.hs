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
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

theSleeperAwakened :: SideSchemeCard TheSleeperAwakened
theSleeperAwakened = sideScheme (TheSleeperAwakened . (`With` Meta False)) Cards.theSleeperAwakened (Static 0)

-- | Whether The Sleeper has been defeated, so this card is to be removed with him.
newtype Meta = Meta {sleeperDefeated :: Bool}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Show, Eq)

newtype TheSleeperAwakened = TheSleeperAwakened (Attrs SideScheme `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsSideScheme TheSleeperAwakened where
  toSideSchemeAttrs (TheSleeperAwakened (attrs `With` _)) = attrs

-- | "This scheme cannot leave play while The Sleeper is in play": not defeated, discarded
-- or removed, however that would happen. Its threat can still be removed.
instance HasModifiersFor TheSleeperAwakened where
  getModifiersFor _ (SideSchemeRef sid) (TheSleeperAwakened (attrs `With` meta))
    | sid == sideSchemeId attrs && not (sleeperDefeated meta) = do
        sleeperInPlay <- selectAny (MinionIs Minions.theSleeper)
        pure [CannotLeavePlay | sleeperInPlay]
  getModifiersFor _ _ _ = pure []

-- | When Revealed: put The Sleeper into play engaged with the first player. When The
-- Sleeper is defeated, remove this card from the game.
instance RunMessage TheSleeperAwakened where
  runMessage msg s@(TheSleeperAwakened (attrs `With` meta)) = runQueueT $ case msg of
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      first <- selectJust FirstPlayer
      sleeper <- filter (cardMatch (CardIs Minions.theSleeper)) <$> getSetAsideEncounterCards
      for_ (take 1 sleeper) (putEncounterCardIntoPlay first)
      pure s
    MinionMessage mid MinionDefeated -> do
      isSleeper <- minionMatches (MinionIs Minions.theSleeper) mid
      if isSleeper
        then do
          removeFromGame attrs
          pure $ TheSleeperAwakened (attrs `With` Meta True)
        else pure s
    _ -> TheSleeperAwakened . (`With` meta) <$> runMessage msg attrs
