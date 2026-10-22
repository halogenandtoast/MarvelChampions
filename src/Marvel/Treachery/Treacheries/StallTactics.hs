module Marvel.Treachery.Treacheries.StallTactics (
  stallTactics,
  StallTactics (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

stallTactics :: TreacheryCard StallTactics
stallTactics = treachery StallTactics Cards.stallTactics

newtype StallTactics = StallTactics (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | 1 threat on the main scheme for every 2 delay counters on it; none placed, it gains
-- surge. [star] Boost: with 5 or more delay counters, take 1 indirect damage.
instance RunMessage StallTactics where
  runMessage msg t@(StallTactics attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> do
      n <- (`div` 2) <$> getYourMainSchemeCounters
      if n > 0
        then t <$ placeThreatOnEach attrs n MainScheme
        else pure . StallTactics $ attrs & surgeL .~ True
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      delay <- getYourMainSchemeCounters
      when (delay >= 5) do
        you <- getBoostedPlayer enemy
        dealIndirectDamage you attrs 1
      pure t
    _ -> StallTactics <$> runMessage msg attrs
