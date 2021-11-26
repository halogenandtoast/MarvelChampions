module Marvel.Treachery.Treacheries.ShadowOfThePast
  ( shadowOfThePast
  , ShadowOfThePast(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Attrs
import Marvel.Treachery.Cards qualified as Cards

shadowOfThePast :: TreacheryCard ShadowOfThePast
shadowOfThePast = treachery ShadowOfThePast Cards.shadowOfThePast

newtype ShadowOfThePast = ShadowOfThePast TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage ShadowOfThePast where
  runMessage msg t@(ShadowOfThePast attrs) = case msg of
    TreacheryMessage treacheryId msg' | treacheryId == toId attrs ->
      case msg' of
        RevealTreachery _ -> pure t
    _ -> ShadowOfThePast <$> runMessage msg attrs
