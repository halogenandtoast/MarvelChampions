module Marvel.SideScheme.SideSchemes.BombScare
  ( bombScare
  , BombScare(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.SideScheme.Attrs
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.Source
import Marvel.Target

bombScare :: SideSchemeCard BombScare
bombScare = sideScheme BombScare Cards.bombScare

newtype BombScare = BombScare SideSchemeAttrs
  deriving anyclass IsSideScheme
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage BombScare where
  runMessage msg (BombScare attrs) = BombScare <$> runMessage msg attrs
