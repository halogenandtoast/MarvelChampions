module Marvel.SideScheme.SideSchemes.HydraPrison (
  hydraPrison,
  HydraPrison (..),
) where

import Marvel.Prelude

import Marvel.Ally.Cards (heroSpecificAllies)
import Marvel.Card
import Marvel.Deck
import Marvel.Discard
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Identity.Types (Field (..))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

hydraPrison :: SideSchemeCard HydraPrison
hydraPrison = sideScheme HydraPrison Cards.hydraPrison (Static 1)

newtype HydraPrison = HydraPrison (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed: each player puts a hero-specific ally of theirs from their deck,
-- discard pile or hand facedown beneath this (one of their choice, if they have any);
-- then X threat is placed here, X the total cost of the allies beneath it, and each
-- player shuffles their deck. When Defeated: it is removed from the game, and each ally
-- beneath it returns to its owner's hand.
instance RunMessage HydraPrison where
  runMessage msg s@(HydraPrison attrs) = runQueueT $ case msg of
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs -> case msg' of
      RevealSideScheme -> do
        players <- getPlayers
        for_ players \p -> do
          hand <- projectMap PlayerIdentityHand unHand p
          deck <- projectMap PlayerIdentityDeck unDeck p
          discard <- projectMap PlayerIdentityDiscard unDiscard p
          let
            allies = filter (cardMatch $ CardMatchesAny $ map CardIs heroSpecificAllies)
            places = [("$game.piles.deck", allies deck), ("$game.piles.discard", allies discard), ("$game.hand", allies hand)]
            found = concatMap snd places
          unless (null found) $
            chooseOneSearchedIn p "$label.imprisonAlly" (map (second (map PlayerCard)) places) [targeting c (cardSpecific attrs "imprison" c) | c <- found]
        sideSchemeRevealedThen ident
        pure s
      SideSchemeRevealedThen -> do
        placeThreat ident $ sum [fromIntegral (fromMaybe 0 $ cdCost $ getCardDef c) | c <- sideSchemeHeldCards attrs]
        getPlayers >>= traverse_ shuffleDeck
        pure s
      DefeatSideScheme -> do
        removeFromGame attrs
        for_ [(owner, c) | c <- sideSchemeHeldCards attrs, Just owner <- [pcOwner c]] (uncurry addToHand)
        pure s
      _ -> HydraPrison <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "imprison" (fromJSON -> Success card) -> do
      removeFromGame (CardIdRef $ pcCardId card)
      cardSpecific attrs "holdTakenCards" [card]
      pure s
    CardSpecific (isTarget attrs -> True) "holdTakenCards" (fromJSON -> Success cards) ->
      pure . HydraPrison $ attrs & heldCardsL <>~ cards
    _ -> HydraPrison <$> runMessage msg attrs
