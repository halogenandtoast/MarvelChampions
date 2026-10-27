module Marvel.SideScheme.SideSchemes.BuddingCrimeSyndicate (
  buddingCrimeSyndicate,
  BuddingCrimeSyndicate (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

-- | Base threat 2; Hinder 2 per player is its keyword.
buddingCrimeSyndicate :: SideSchemeCard BuddingCrimeSyndicate
buddingCrimeSyndicate = sideScheme BuddingCrimeSyndicate Cards.buddingCrimeSyndicate (Static 2)

newtype BuddingCrimeSyndicate = BuddingCrimeSyndicate (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage BuddingCrimeSyndicate where
  runMessage msg (BuddingCrimeSyndicate attrs) = runQueueT $ BuddingCrimeSyndicate <$> runMessage msg attrs
