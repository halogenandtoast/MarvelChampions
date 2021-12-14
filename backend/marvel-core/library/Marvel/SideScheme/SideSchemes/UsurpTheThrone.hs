module Marvel.SideScheme.SideSchemes.UsurpTheThrone
  ( usurpTheThrone
  , UsurpTheThrone(..)
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

usurpTheThrone :: SideSchemeCard UsurpTheThrone
usurpTheThrone =
  sideScheme UsurpTheThrone Cards.usurpTheThrone (PerPlayer 3)

newtype UsurpTheThrone = UsurpTheThrone SideSchemeAttrs
  deriving anyclass IsSideScheme
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage UsurpTheThrone where
  runMessage msg (UsurpTheThrone attrs) =
    UsurpTheThrone <$> runMessage msg attrs
