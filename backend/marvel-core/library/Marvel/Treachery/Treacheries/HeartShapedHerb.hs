module Marvel.Treachery.Treacheries.HeartShapedHerb
  ( heartShapedHerb
  , HeartShapedHerb(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message
import Marvel.Source
import Marvel.Target
import Marvel.Treachery.Attrs
import Marvel.Treachery.Cards qualified as Cards

heartShapedHerb :: TreacheryCard HeartShapedHerb
heartShapedHerb = treachery HeartShapedHerb Cards.heartShapedHerb

newtype HeartShapedHerb = HeartShapedHerb TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage HeartShapedHerb where
  runMessage msg t@(HeartShapedHerb attrs) = case msg of
    TreacheryMessage treacheryId msg' | toId attrs == treacheryId ->
      case msg' of
        RevealTreachery _ -> pure t
        _ -> HeartShapedHerb <$> runMessage msg attrs
    _ -> HeartShapedHerb <$> runMessage msg attrs
