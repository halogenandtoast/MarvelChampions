module Marvel.Treachery.Treacheries.FalseAlarm
  ( falseAlarm
  , FalseAlarm(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Attrs
import Marvel.Treachery.Cards qualified as Cards

falseAlarm :: TreacheryCard FalseAlarm
falseAlarm = treachery FalseAlarm Cards.falseAlarm

newtype FalseAlarm = FalseAlarm TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage FalseAlarm where
  runMessage msg (FalseAlarm attrs) = FalseAlarm <$> runMessage msg attrs
