module Marvel.Treachery.Treacheries.Harvest (
  harvest,
  Harvest (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Trait
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

harvest :: TreacheryCard Harvest
harvest = treachery Harvest Cards.harvest

newtype Harvest = Harvest (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Only a Persona support that was ready is exhausted "this way".
instance RunMessage Harvest where
  runMessage msg t@(Harvest attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery _) | ident == treacheryId attrs -> do
      personas <- selectList $ SupportMatches [SupportWithTrait Persona, UnexhaustedSupport]
      villain <- selectJust ActiveVillain
      for_ personas exhaust
      when (notNull personas) $ heal villain (fromIntegral $ length personas)
      pure $ if null personas then Harvest (attrs & surgeL .~ True) else t
    _ -> Harvest <$> runMessage msg attrs
