module Marvel.SideScheme.SideSchemes.TheImmortalKlaw (
  theImmortalKlaw,
  TheImmortalKlaw (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

theImmortalKlaw :: SideSchemeCard TheImmortalKlaw
theImmortalKlaw =
  sideScheme TheImmortalKlaw Cards.theImmortalKlaw (PerPlayer 1)

newtype TheImmortalKlaw = TheImmortalKlaw (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance HasModifiersFor TheImmortalKlaw where
  getModifiersFor _ (VillainRef _) (TheImmortalKlaw _) =
    pure [HitPointModifier 10]
  getModifiersFor _ _ _ = pure []

instance RunMessage TheImmortalKlaw where
  runMessage msg (TheImmortalKlaw attrs) = case msg of
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs -> case msg' of
      RevealSideScheme -> do
        n <- fromIntegral <$> fromGameValue (PerPlayer 1)
        pure . TheImmortalKlaw $ attrs & threatL +~ n
      _ -> TheImmortalKlaw <$> runMessage msg attrs
    _ -> TheImmortalKlaw <$> runMessage msg attrs
