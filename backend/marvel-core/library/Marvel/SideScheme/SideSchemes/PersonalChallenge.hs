module Marvel.SideScheme.SideSchemes.PersonalChallenge (
  personalChallenge,
  PersonalChallenge (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

personalChallenge :: SideSchemeCard PersonalChallenge
personalChallenge =
  sideSchemeWith
    PersonalChallenge
    Cards.personalChallenge
    (Static 3)
    (crisisL .~ True)

newtype PersonalChallenge = PersonalChallenge (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage PersonalChallenge where
  runMessage msg (PersonalChallenge attrs) = case msg of
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs -> case msg' of
      RevealSideScheme -> do
        n <- fromIntegral <$> fromGameValue (PerPlayer 1)
        pure . PersonalChallenge $ attrs & threatL +~ n
      _ -> PersonalChallenge <$> runMessage msg attrs
    _ -> PersonalChallenge <$> runMessage msg attrs
