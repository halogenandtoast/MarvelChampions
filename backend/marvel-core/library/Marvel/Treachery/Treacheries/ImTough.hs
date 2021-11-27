module Marvel.Treachery.Treacheries.ImTough
  ( imTough
  , ImTough(..)
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

imTough :: TreacheryCard ImTough
imTough = treachery ImTough Cards.imTough

newtype ImTough = ImTough TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage ImTough where
  runMessage msg t@(ImTough attrs) = case msg of
    TreacheryMessage tid msg' | tid == toId attrs -> case msg' of
      RevealTreachery _ -> do
        isTough <- selectAny $ ActiveVillain <> VillainWithToughStatus
        if not isTough
          then do
            villainId <- selectJust ActiveVillain
            push $ VillainMessage villainId VillainBecomeTough
            pure t
          else pure . ImTough $ attrs & surgeL .~ True
      _ -> ImTough <$> runMessage msg attrs
    _ -> ImTough <$> runMessage msg attrs
