module Marvel.Treachery.Treacheries.ImTough
  ( imTough
  , ImTough(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Attrs
import Marvel.Treachery.Cards qualified as Cards

imTough :: TreacheryCard ImTough
imTough = treachery ImTough Cards.imTough

newtype ImTough = ImTough TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage ImTough where
  runMessage msg (ImTough attrs) = ImTough <$> runMessage msg attrs
