module Marvel.SideScheme.SideSchemes.PersonalChallenge
  ( personalChallenge
  , PersonalChallenge(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message
import Marvel.SideScheme.Attrs
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.Source
import Marvel.Target

personalChallenge :: SideSchemeCard PersonalChallenge
personalChallenge = sideSchemeWith
  PersonalChallenge
  Cards.personalChallenge
  (Static 3)
  (crisisL .~ True)

newtype PersonalChallenge = PersonalChallenge SideSchemeAttrs
  deriving anyclass IsSideScheme
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage PersonalChallenge where
  runMessage msg (PersonalChallenge attrs) = case msg of
    SideSchemeMessage sideSchemeId msg' | sideSchemeId == toId attrs ->
      case msg' of
        RevealSideScheme -> do
          n <- fromIntegral <$> fromGameValue (PerPlayer 1)
          pure . PersonalChallenge $ attrs & threatL +~ n
        _ -> PersonalChallenge <$> runMessage msg attrs
    _ -> PersonalChallenge <$> runMessage msg attrs
