module Marvel.Treachery.Treacheries.Explosion
  ( explosion
  , Explosion(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Attrs
import Marvel.Treachery.Cards qualified as Cards

explosion :: TreacheryCard Explosion
explosion = treachery Explosion Cards.explosion

newtype Explosion = Explosion TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage Explosion where
  runMessage msg (Explosion attrs) = Explosion <$> runMessage msg attrs
