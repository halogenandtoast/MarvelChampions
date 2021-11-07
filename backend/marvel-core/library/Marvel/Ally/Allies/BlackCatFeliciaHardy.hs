module Marvel.Ally.Allies.BlackCatFeliciaHardy where

import Marvel.Prelude

import Marvel.Ally.Attrs
import qualified Marvel.Ally.Cards as Cards
import Marvel.Card.Code

blackCatFeliciaHardy :: AllyCard BlackCatFeliciaHardy
blackCatFeliciaHardy =
  ally BlackCatFeliciaHardy Cards.blackCatFeliciaHardy (Thw 1) (Atk 1)

newtype BlackCatFeliciaHardy = BlackCatFeliciaHardy AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)
