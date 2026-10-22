module Marvel.Treachery.Treacheries.OversizedHands (
  oversizedHands,
  OversizedHands (..),
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

oversizedHands :: TreacheryCard OversizedHands
oversizedHands = treachery OversizedHands Cards.oversizedHands

newtype OversizedHands = OversizedHands (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: discard the support you control with the highest cost. If no support
-- was discarded this way, place 2 threat on the active villain's scheme.
-- [star] Boost: discard a support you control.
instance RunMessage OversizedHands where
  runMessage msg t@(OversizedHands attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      supports <- highestCostControlled you False True
      if null supports
        then traverse_ (`placeThreat` 2) =<< selectOne (SignatureSchemeOf ActiveVillain)
        else chooseOrRunOne you [targeting s (discardControlled s) | s <- supports]
      pure t
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      supports <- selectList (SupportControlledBy $ IdentityWithId you)
      unless (null supports) $ chooseOne you [targeting s (discardControlled (SupportRef s)) | s <- supports]
      pure t
    _ -> OversizedHands <$> runMessage msg attrs
