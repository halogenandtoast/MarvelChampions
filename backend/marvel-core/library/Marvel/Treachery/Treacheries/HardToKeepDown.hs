module Marvel.Treachery.Treacheries.HardToKeepDown where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Attrs
import Marvel.Treachery.Cards qualified as Cards

hardToKeepDown :: TreacheryCard HardToKeepDown
hardToKeepDown = treachery HardToKeepDown Cards.hardToKeepDown

newtype HardToKeepDown = HardToKeepDown TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage HardToKeepDown where
  runMessage msg (HardToKeepDown attrs) =
    HardToKeepDown <$> runMessage msg attrs
