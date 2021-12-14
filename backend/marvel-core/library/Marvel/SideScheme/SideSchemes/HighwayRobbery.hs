module Marvel.SideScheme.SideSchemes.HighwayRobbery
  ( highwayRobbery
  , HighwayRobbery(..)
  ) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.Message
import Marvel.Queue
import Marvel.SideScheme.Attrs
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.Source
import Marvel.Target

highwayRobbery :: SideSchemeCard HighwayRobbery
highwayRobbery =
  sideScheme HighwayRobbery Cards.highwayRobbery (PerPlayer 3)

newtype HighwayRobbery = HighwayRobbery SideSchemeAttrs
  deriving anyclass IsSideScheme
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

getCardOwner :: PlayerCard -> IdentityId
getCardOwner pc = case pcOwner pc of
  Nothing -> error "invalid state, card must have owner"
  Just ownerId -> ownerId

instance RunMessage HighwayRobbery where
  runMessage msg s@(HighwayRobbery attrs) = case msg of
    SideSchemeMessage sideSchemeId msg' | sideSchemeId == toId attrs ->
      case msg' of
        RevealSideScheme -> do
          players <- getPlayers
          pushAll $ map
            (\identityId -> IdentityMessage identityId
              $ DiscardFor (toTarget attrs) RandomFromHand 1 1
            )
            players
          pure s
        DefeatSideScheme -> do
          pushAll $ map
            (\c -> IdentityMessage (getCardOwner c) (AddToHand c))
            (sideSchemeHeldCards attrs)
          pure s
        _ -> HighwayRobbery <$> runMessage msg attrs
    WithDiscarded target _ cards | isTarget attrs target -> do
      pure . HighwayRobbery $ attrs & heldCardsL <>~ onlyPlayerCards cards
    _ -> HighwayRobbery <$> runMessage msg attrs
