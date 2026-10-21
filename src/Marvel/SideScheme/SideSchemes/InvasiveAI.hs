module Marvel.SideScheme.SideSchemes.InvasiveAI (
  invasiveAI,
  InvasiveAI (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Message
import Marvel.Modifier
import Marvel.Queue
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

invasiveAI :: SideSchemeCard InvasiveAI
invasiveAI = sideScheme InvasiveAI Cards.invasiveAI (PerPlayer 3)

newtype InvasiveAI = InvasiveAI (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage InvasiveAI where
  runMessage msg s@(InvasiveAI attrs) = case msg of
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      players <- getPlayers
      pushAll [IdentityMessage p $ DiscardFrom FromDeck 3 Nothing | p <- players]
      pure s
    _ -> InvasiveAI <$> runMessage msg attrs
