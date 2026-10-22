module Marvel.SideScheme.SideSchemes.GenerationWhy (
  generationWhy,
  GenerationWhy (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Trait

generationWhy :: SideSchemeCard GenerationWhy
generationWhy = sideScheme GenerationWhy Cards.generationWhy (PerPlayer 2)

newtype GenerationWhy = GenerationWhy (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Each player discards a card from their deck for each ally and Persona support in
-- play, all of them counted.
instance RunMessage GenerationWhy where
  runMessage msg (GenerationWhy attrs) = runQueueT $ case msg of
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      allies <- selectList AnyAlly
      personas <- selectList (SupportWithTrait Persona)
      let n = fromIntegral (length allies + length personas)
      players <- getPlayers
      when (n > 0) $ for_ players \p -> discardTopOfDeck p n
      GenerationWhy <$> runMessage msg attrs
    _ -> GenerationWhy <$> runMessage msg attrs
