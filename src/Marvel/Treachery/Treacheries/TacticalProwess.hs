module Marvel.Treachery.Treacheries.TacticalProwess (
  tacticalProwess,
  TacticalProwess (..),
) where

import Marvel.Prelude

import Control.Monad ()
import Data.Maybe ()
import Data.Aeson ()
import Marvel.Attack ()
import Marvel.Card
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Data.List ()
import Marvel.Id
import Marvel.Identity.Types ()
import Marvel.Projection ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query ()
import Marvel.Ref
import Marvel.Status ()
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

tacticalProwess :: TreacheryCard TacticalProwess
tacticalProwess = treachery (TacticalProwess . (`With` Meta Nothing False)) Cards.tacticalProwess

data Meta = Meta {movedTo :: Maybe SideSchemeId, triggered :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TacticalProwess = TacticalProwess (Attrs Treachery `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsTreachery TacticalProwess where
  toTreacheryAttrs (TacticalProwess (attrs `With` _)) = attrs

-- | When Revealed: move all threat from the side scheme with the least threat to the side
-- scheme with the most threat. If that scheme's Forced Response is not triggered this
-- way, this card gains surge.
instance RunMessage TacticalProwess where
  runMessage msg t@(TacticalProwess (attrs `With` meta)) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> do
      withSideSchemeByThreat LeastThreat AnySideScheme \from ->
        withSideSchemeByThreat MostThreat (NotSideScheme $ SideSchemeWithId from) \to ->
          cardSpecific attrs "move" (from, to)
      pure t
    CardSpecific (isTarget attrs -> True) "move" (fromJSON -> Success (from, to)) -> do
      n <- sideSchemeThreatOf from
      removeSideSchemeThreat from n
      placeThreat to n
      pure $ TacticalProwess (attrs `With` meta {movedTo = Just to})
    -- the scheme's Forced Response removes all but 3 threat from it as it resolves
    CardSpecific (SideSchemeRef s) "allButThree" _ | movedTo meta == Just s ->
      pure $ TacticalProwess (attrs `With` meta {triggered = True})
    TreacheryMessage ident (ResolvedTreachery _) | ident == treacheryId attrs && not (triggered meta) ->
      TacticalProwess . (`With` meta) <$> runMessage msg (attrs & surgeL .~ True)
    _ -> TacticalProwess . (`With` meta) <$> runMessage msg attrs
