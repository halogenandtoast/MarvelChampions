module Marvel.Treachery.Treacheries.BuddySystem (
  buddySystem,
  BuddySystem (..),
) where

import Marvel.Prelude

import Control.Monad (replicateM_)
import Data.Maybe (maybeToList)
import Data.Aeson (Result (..), fromJSON)
import Marvel.Attack
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Data.List (nub)
import Marvel.Id
import Marvel.Identity.Types (Field (PlayerIdentityDefeated))
import Marvel.Projection (projectP)
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Status
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

buddySystem :: TreacheryCard BuddySystem
buddySystem = treachery BuddySystem Cards.buddySystem

newtype BuddySystem = BuddySystem (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: choose the villain whose side scheme has the least threat; reveal the
-- top card of his deck, the top 2 if he is the only villain in play.
-- [star] Boost: move the active counter to the villain whose side scheme has the least
-- threat.
instance RunMessage BuddySystem where
  runMessage msg t@(BuddySystem attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      alone <- (== 1) <$> selectListCount AnyVillain
      withVillainBySideSchemeThreat LeastThreat \v ->
        replicateM_ (if alone then 2 else 1) $ fromDeck (VillainDeckKey v) (drawAndRevealEncounterCard you)
      pure t
    Boost (RevealedAsBoost target _) | isTarget attrs target -> t <$ moveActiveCounter LeastThreat
    _ -> BuddySystem <$> runMessage msg attrs
