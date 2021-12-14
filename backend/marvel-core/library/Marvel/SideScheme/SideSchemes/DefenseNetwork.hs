module Marvel.SideScheme.SideSchemes.DefenseNetwork
  ( defenseNetwork
  , DefenseNetwork(..)
  )
where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message
import Marvel.Modifier
import Marvel.SideScheme.Attrs
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.Source
import Marvel.Target

defenseNetwork :: SideSchemeCard DefenseNetwork
defenseNetwork = sideSchemeWith DefenseNetwork Cards.defenseNetwork (Static 2) (crisisL .~ True)

newtype DefenseNetwork = DefenseNetwork SideSchemeAttrs
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage DefenseNetwork where
  runMessage msg (DefenseNetwork attrs) = case msg of
    SideSchemeMessage sideSchemeId msg' | sideSchemeId == toId attrs ->
      case msg' of
        RevealSideScheme -> do
          n <- fromIntegral <$> fromGameValue (PerPlayer 1)
          pure . DefenseNetwork $ attrs & threatL +~ n
        _ -> DefenseNetwork <$> runMessage msg attrs
    _ -> DefenseNetwork <$> runMessage msg attrs
