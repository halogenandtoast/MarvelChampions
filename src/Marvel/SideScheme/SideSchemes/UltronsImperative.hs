module Marvel.SideScheme.SideSchemes.UltronsImperative (
  ultronsImperative,
  UltronsImperative (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Queue
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

ultronsImperative :: SideSchemeCard UltronsImperative
ultronsImperative = sideScheme UltronsImperative Cards.ultronsImperative (PerPlayer 2)

newtype UltronsImperative = UltronsImperative (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage UltronsImperative where
  runMessage msg s@(UltronsImperative attrs) = case msg of
    -- the first player puts the top 2 cards of their deck into play as Drones
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      firstPlayer <- selectJust FirstPlayer
      pushAll $ replicate 2 (PutTopOfDeckIntoPlayAsDrone firstPlayer)
      pure s
    _ -> UltronsImperative <$> runMessage msg attrs
