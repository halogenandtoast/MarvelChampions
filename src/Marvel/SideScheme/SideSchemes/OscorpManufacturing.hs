module Marvel.SideScheme.SideSchemes.OscorpManufacturing (
  oscorpManufacturing,
  OscorpManufacturing (..),
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
import Marvel.Environment.Environments.CriminalEnterprise (isNormanOsbornSide)

oscorpManufacturing :: SideSchemeCard OscorpManufacturing
oscorpManufacturing = sideScheme OscorpManufacturing Cards.oscorpManufacturing (PerPlayer 2)

newtype OscorpManufacturing = OscorpManufacturing (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage OscorpManufacturing where
  runMessage msg s@(OscorpManufacturing attrs) = runQueueT $ case msg of
    -- When Revealed (Norman Osborn): an additional 1[per player] threat here
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      norman <- isNormanOsbornSide
      when norman $ placeThreat ident . fromIntegral =<< fromGameValue (PerPlayer 1)
      pure s
    _ -> OscorpManufacturing <$> runMessage msg attrs
