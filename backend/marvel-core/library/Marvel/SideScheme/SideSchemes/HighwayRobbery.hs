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
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

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
      RevealSideScheme -> do
        players <- getPlayers
        pushAll $
          map
            ( \identityId ->
                IdentityMessage identityId $
                  DiscardFor (toRef attrs) RandomFromHand 1 1
            )
            players
        pure s
      DefeatSideScheme -> do
        pushAll $
          map
            (\c -> IdentityMessage (getCardOwner c) (AddToHand c))
            (sideSchemeHeldCards attrs)
        pure s
      _ -> HighwayRobbery <$> runMessage msg attrs
    WithDiscarded target _ cards | isTarget attrs target -> do
      pure . HighwayRobbery $ attrs & heldCardsL <>~ onlyPlayerCards cards
    _ -> HighwayRobbery <$> runMessage msg attrs
