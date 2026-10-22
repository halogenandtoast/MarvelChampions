module Marvel.SideScheme.SideSchemes.OpenTheDarkDimension (
  openTheDarkDimension,
  OpenTheDarkDimension (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Invocation
import Marvel.Message
import Marvel.Modifier
import Marvel.Queue
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

openTheDarkDimension :: SideSchemeCard OpenTheDarkDimension
openTheDarkDimension = sideScheme OpenTheDarkDimension Cards.openTheDarkDimension (Static 3)

newtype OpenTheDarkDimension = OpenTheDarkDimension (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed: the top card of the Invocation deck goes facedown under it (each
-- player is asked: only Doctor Strange has one). When Defeated: that card is shuffled
-- back into the Invocation deck.
instance RunMessage OpenTheDarkDimension where
  runMessage msg s@(OpenTheDarkDimension attrs) = case msg of
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs -> case msg' of
      RevealSideScheme -> do
        players <- getPlayers
        pushAll [invocation p (PlaceTopInvocationUnder $ toTarget attrs) | p <- players]
        pure s
      DefeatSideScheme -> do
        pushAll
          [ invocation owner (ShuffleIntoInvocationDeck c)
          | c <- sideSchemeHeldCards attrs
          , Just owner <- [pcOwner c]
          ]
        pure s
      _ -> OpenTheDarkDimension <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "invocationUnder" (fromJSON -> Success card) ->
      pure . OpenTheDarkDimension $ attrs & heldCardsL <>~ [card]
    _ -> OpenTheDarkDimension <$> runMessage msg attrs
