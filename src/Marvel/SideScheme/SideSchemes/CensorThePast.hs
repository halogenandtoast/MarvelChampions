module Marvel.SideScheme.SideSchemes.CensorThePast (
  censorThePast,
  CensorThePast (..),
) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

censorThePast :: SideSchemeCard CensorThePast
censorThePast = sideScheme CensorThePast Cards.censorThePast (PerPlayer 3)

newtype CensorThePast = CensorThePast (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Defeated: each player chooses up to 3 cards in their discard pile and shuffles
-- them into their deck.
instance RunMessage CensorThePast where
  runMessage msg s@(CensorThePast attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      players <- getPlayers
      for_ players \p -> chooseFromDiscard p attrs AnyCards 0 3
      pure s
    WithChosen (isTarget attrs -> True) _ (onlyPlayerCards -> cards) -> do
      for_ (headMay cards >>= pcOwner) \owner -> shuffleIntoDeck owner cards
      pure s
    _ -> CensorThePast <$> runMessage msg attrs
