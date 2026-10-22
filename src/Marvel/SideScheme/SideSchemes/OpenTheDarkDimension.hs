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
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

openTheDarkDimension :: SideSchemeCard OpenTheDarkDimension
openTheDarkDimension = sideScheme OpenTheDarkDimension Cards.openTheDarkDimension (PerPlayer 3)

newtype OpenTheDarkDimension = OpenTheDarkDimension (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed: the top card of the Invocation deck goes facedown under it (each
-- player is asked: only Doctor Strange has one). When Defeated: that card is shuffled
-- back into the Invocation deck.
instance RunMessage OpenTheDarkDimension where
  runMessage msg s@(OpenTheDarkDimension attrs) = runQueueT $ case msg of
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs -> case msg' of
      RevealSideScheme -> do
        players <- getPlayers
        for_ players \p -> invoke p (PlaceTopInvocationUnder $ toTarget attrs)
        pure s
      DefeatSideScheme -> do
        for_ [(owner, c) | c <- sideSchemeHeldCards attrs, Just owner <- [pcOwner c]] \(owner, c) ->
          invoke owner (ShuffleIntoInvocationDeck c)
        pure s
      _ -> OpenTheDarkDimension <$> runMessage msg attrs
    CardSpecific (isTarget attrs -> True) "invocationUnder" (fromJSON -> Success card) ->
      pure . OpenTheDarkDimension $ attrs & heldCardsL <>~ [card]
    _ -> OpenTheDarkDimension <$> runMessage msg attrs
