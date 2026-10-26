module Marvel.SideScheme.SideSchemes.ExtortionOfSeismicProportion (
  extortionOfSeismicProportion,
  ExtortionOfSeismicProportion (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

-- | Crisis.
extortionOfSeismicProportion :: SideSchemeCard ExtortionOfSeismicProportion
extortionOfSeismicProportion =
  sideSchemeWith ExtortionOfSeismicProportion Cards.extortionOfSeismicProportion (PerPlayer 2) (crisisL .~ True)

newtype ExtortionOfSeismicProportion = ExtortionOfSeismicProportion (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage ExtortionOfSeismicProportion where
  runMessage msg (ExtortionOfSeismicProportion attrs) = runQueueT $ ExtortionOfSeismicProportion <$> runMessage msg attrs
