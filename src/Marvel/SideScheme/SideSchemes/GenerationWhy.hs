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
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Queue
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
  runMessage msg (GenerationWhy attrs) = case msg of
    SideSchemeMessage ident RevealSideScheme | ident == sideSchemeId attrs -> do
      allies <- selectList AnyAlly
      personas <- selectList (SupportWithTrait Persona)
      let n = fromIntegral (length allies + length personas)
      players <- getPlayers
      when (n > 0) $
        pushAll [IdentityMessage p $ DiscardFrom FromDeck n Nothing | p <- players]
      GenerationWhy <$> runMessage msg attrs
    _ -> GenerationWhy <$> runMessage msg attrs
