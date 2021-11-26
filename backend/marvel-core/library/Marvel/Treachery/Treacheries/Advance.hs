module Marvel.Treachery.Treacheries.Advance
  ( advance
  , Advance(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Attrs
import Marvel.Treachery.Cards qualified as Cards

advance :: TreacheryCard Advance
advance = treachery Advance Cards.advance

newtype Advance = Advance TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage Advance where
  runMessage msg t@(Advance attrs) = case msg of
    TreacheryMessage treacheryId msg' | toId attrs == treacheryId ->
      case msg' of
        RevealTreachery _ -> do
          villain <- selectJust ActiveVillain
          push $ VillainMessage villain VillainSchemes
          pure t
        _ -> Advance <$> runMessage msg attrs
    _ -> Advance <$> runMessage msg attrs
