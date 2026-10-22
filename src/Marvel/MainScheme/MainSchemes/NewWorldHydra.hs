module Marvel.MainScheme.MainSchemes.NewWorldHydra where

import Marvel.Prelude

import Marvel.Card
import Marvel.Entity
import Marvel.GameValue
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.MainSchemes.TheRiseOfRedSkull (revealTopSideScheme)
import Marvel.MainScheme.Types
import Marvel.Message.Lifted

newWorldHydra :: MainSchemeCard NewWorldHydra
newWorldHydra =
  mainScheme NewWorldHydra Cards.newWorldHydra (PerPlayer 11) (PerPlayer 1) (PerPlayer 1)

newtype NewWorldHydra = NewWorldHydra (Attrs MainScheme)
  deriving anyclass (IsMainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage NewWorldHydra where
  runMessage msg ms@(NewWorldHydra attrs) = runQueueT $ case msg of
    -- 2A: reveal the top card of the side-scheme deck and put it into play
    MainSchemeMessage ident RevealMainScheme | ident == mainSchemeId attrs -> ms <$ revealTopSideScheme
    -- Forced Response: after step one of the villain phase
    AccelerateMainScheme -> do
      ms' <- NewWorldHydra <$> runMessage msg attrs
      revealTopSideScheme
      pure ms'
    AdvanceMainScheme -> ms <$ gameOver Lost
    _ -> NewWorldHydra <$> runMessage msg attrs
