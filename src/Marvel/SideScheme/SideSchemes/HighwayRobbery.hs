module Marvel.SideScheme.SideSchemes.HighwayRobbery (
  highwayRobbery,
  HighwayRobbery (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Message
import Marvel.Modifier
import Marvel.Queue
import Marvel.Hand
import Marvel.Identity.Types (Field (..))
import Marvel.Projection
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import System.Random.Shuffle (shuffleM)

highwayRobbery :: SideSchemeCard HighwayRobbery
highwayRobbery = sideScheme HighwayRobbery Cards.highwayRobbery (PerPlayer 3)

newtype HighwayRobbery = HighwayRobbery (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

getCardOwner :: PlayerCard -> IdentityId
getCardOwner pc = case pcOwner pc of
  Nothing -> error "invalid state, card must have owner"
  Just ownerId -> ownerId

instance RunMessage HighwayRobbery where
  runMessage msg s@(HighwayRobbery attrs) = case msg of
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs -> case msg' of
      -- the cards are held here facedown: out of their hands, not discarded
      RevealSideScheme -> do
        players <- getPlayers
        taken <- for players \identityId ->
          headMay <$> (shuffleM =<< projectMap PlayerIdentityHand unHand identityId)
        let cards = catMaybes taken
        pushAll [RemoveFromGame (CardIdRef $ pcCardId c) | c <- cards]
        pure . HighwayRobbery $ attrs & heldCardsL <>~ cards
      DefeatSideScheme -> do
        pushAll $
          map
            (\c -> IdentityMessage (getCardOwner c) (AddToHand c))
            (sideSchemeHeldCards attrs)
        pure s
      _ -> HighwayRobbery <$> runMessage msg attrs
    _ -> HighwayRobbery <$> runMessage msg attrs
