module Marvel.Treachery.Treacheries.Trickster (
  trickster,
  Trickster (..),
) where

import Marvel.Prelude

import Data.List (nub)
import Marvel.Card
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Queue
import Marvel.Ref
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

trickster :: TreacheryCard Trickster
trickster = treachery Trickster Cards.trickster

newtype Trickster = Trickster (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | The player revealing it discards the top 3 cards of their deck; 1 threat on the main
-- scheme for each card type among them.
instance RunMessage Trickster where
  runMessage msg t@(Trickster attrs) = case msg of
    TreacheryMessage ident msg' | treacheryId attrs == ident -> case msg' of
      RevealTreachery identityId -> do
        push $ IdentityMessage identityId $ DiscardFrom FromDeck 3 (Just $ toTarget attrs)
        pure t
      _ -> Trickster <$> runMessage msg attrs
    WithDiscarded (isTarget attrs -> True) _ cards -> do
      mainScheme <- selectJust MainScheme
      pushAll $ placeThreatOn mainScheme $ fromIntegral $ length $ nub $ map (cdCardType . getCardDef) cards
      pure t
    _ -> Trickster <$> runMessage msg attrs
