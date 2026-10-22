module Marvel.Treachery.Treacheries.LeadingTheCharge (
  leadingTheCharge,
  LeadingTheCharge (..),
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

leadingTheCharge :: TreacheryCard LeadingTheCharge
leadingTheCharge = treachery LeadingTheCharge Cards.leadingTheCharge

newtype LeadingTheCharge = LeadingTheCharge (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Revealed: discard the top X cards of your deck, where X is Bulldozer's ATK. Place
-- 1 threat on Bulldozer's side scheme for each different card type discarded this way.
instance RunMessage LeadingTheCharge where
  runMessage msg t@(LeadingTheCharge attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery you) | ident == treacheryId attrs -> do
      villainNamed "Bulldozer" >>= traverse_ \v -> do
        x <- getVillainAttackValue v
        when (x > 0) $ discardFromDeck you x attrs
      pure t
    WithDiscarded (isTarget attrs -> True) _ cards -> do
      let types = fromIntegral . length . nub $ map (cdCardType . getCardDef) cards
      when (types > 0) $
        villainNamed "Bulldozer" >>= traverse_ \v -> traverse_ (`placeThreat` types) =<< signatureSchemeOf v
      pure t
    _ -> LeadingTheCharge <$> runMessage msg attrs
