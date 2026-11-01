module Marvel.SideScheme.SideSchemes.MarkedForDeath (
  markedForDeath,
  MarkedForDeath (..),
) where

import Marvel.Prelude

import Marvel.Ally.Cards qualified as Allies
import Marvel.Card
import Marvel.Deck
import Marvel.Discard
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero.Cards qualified as Heroes
import Marvel.Id
import Marvel.Identity.Types (Field (..))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

markedForDeath :: SideSchemeCard MarkedForDeath
markedForDeath = sideScheme MarkedForDeath Cards.markedForDeath (Static 5)

newtype MarkedForDeath = MarkedForDeath (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed (as errata'd): the Clint Barton player searches their hand, deck,
-- discard pile, and play area for Mockingbird and tucks her faceup beneath this card;
-- their own copy, another player's not being theirs to tuck (FAQ). When this scheme is
-- defeated, the tucked Mockingbird returns to her owner's hand.
instance RunMessage MarkedForDeath where
  runMessage msg s@(MarkedForDeath attrs) = runQueueT $ case msg of
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs -> case msg' of
      RevealSideScheme -> do
        players <- getPlayers
        clints <- filterM (identityMatches (IdentityIs Heroes.hawkeye)) players
        tucked <- for clints \p -> do
          held <- concat <$> sequence
            [ unHand <$> projectMap PlayerIdentityHand id p
            , unDeck <$> projectMap PlayerIdentityDeck id p
            , unDiscard <$> projectMap PlayerIdentityDiscard id p
            ]
          inPlay <- for [Allies.mockingbird, Allies.mockingbirdBobbiMorse] \def ->
            map (def,) <$> selectList (AllyIs def <> AllyControlledBy (IdentityWithId p))
          let fromPlay = [MkPlayerCard (CardId $ unAllyId aid) def (Just p) (Just p) | (def, aid) <- concat inPlay]
              cards = take 1 $ filter isMockingbird held <> fromPlay
          for_ (concat inPlay) \(_, aid) -> removeFromGame (AllyRef aid)
          for_ cards \c -> removeFromGame (CardIdRef $ pcCardId c)
          shuffleDeck p
          pure cards
        cardSpecific attrs "holdMockingbirds" (concat tucked)
        pure s
      DefeatSideScheme -> do
        for_ [(owner, c) | c <- sideSchemeHeldCards attrs, Just owner <- [pcOwner c]] (uncurry addToHand)
        pure s
      _ -> MarkedForDeath <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "holdMockingbirds" (fromJSON -> Success cards) ->
      pure . MarkedForDeath $ attrs & heldCardsL <>~ cards
    _ -> MarkedForDeath <$> runMessage msg attrs
   where
    isMockingbird = cardMatch (CardMatchesAny [CardIs Allies.mockingbird, CardIs Allies.mockingbirdBobbiMorse])
