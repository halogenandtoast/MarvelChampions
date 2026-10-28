-- | Tower Defense's side scheme (The Mad Titan's Shadow).
module Marvel.SideScheme.SideSchemes.CityUnderAttack (
  cityUnderAttack,
  CityUnderAttack (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

-- | 2 threat, fixed; Hinder 1 per player is its card's.
cityUnderAttack :: SideSchemeCard CityUnderAttack
cityUnderAttack = sideSchemeWith CityUnderAttack Cards.cityUnderAttack (Static 2) (crisisL .~ True)

-- | A crisis icon.
newtype CityUnderAttack = CityUnderAttack (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Defeated: the player who defeated this scheme draws 1 card.
instance RunMessage CityUnderAttack where
  runMessage msg (CityUnderAttack attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      you <- getDefeatedBy attrs
      drawCards you 1
      CityUnderAttack <$> runMessage msg attrs
    _ -> CityUnderAttack <$> runMessage msg attrs
