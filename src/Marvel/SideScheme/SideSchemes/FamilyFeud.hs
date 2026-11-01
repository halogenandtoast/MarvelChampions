module Marvel.SideScheme.SideSchemes.FamilyFeud (
  familyFeud,
  FamilyFeud (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Id ()
import Marvel.Game.Source
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Trait

familyFeud :: SideSchemeCard FamilyFeud
familyFeud = sideScheme FamilyFeud Cards.familyFeud (Static 2)

newtype FamilyFeud = FamilyFeud (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | When Revealed: 1 more threat for each Asgard card in play, of whatever type.
instance RunMessage FamilyFeud where
  runMessage msg s@(FamilyFeud attrs) = runQueueT $ case msg of
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs -> case msg' of
      RevealSideScheme -> do
        n <- countCardsInPlayWithTrait Asgard
        placeThreat (sideSchemeId attrs) n
        pure s
      _ -> FamilyFeud <$> runMessage msg attrs
    _ -> FamilyFeud <$> runMessage msg attrs
