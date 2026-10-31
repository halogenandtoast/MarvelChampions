module Marvel.Minion.Minions.WadeCole where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Trait qualified as Trait
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Attachment.Cards qualified as Attachments

wadeCole :: MinionCard WadeCole
wadeCole = minion WadeCole Cards.wadeCole (Sch 1) (Atk 2) (HP 3)

newtype WadeCole = WadeCole (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities WadeCole where
  getAbilities (WadeCole attrs) = []

instance RunMessage WadeCole where
  runMessage msg m@(WadeCole attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      key <- getDeckKeyFor you
      pile <- getPile key
      let searched = pileDeck pile <> pileDiscard pile
          copies = filter (cardMatch $ CardIs Attachments.cyberneticEnhancements) searched
      unless (null copies) $ chooseOrRunOneSearched you (map EncounterCard searched)
        [targeting card (attachEncounterCard card mid) | card <- take 1 copies]
      fromDeck key shuffleEncounterDeck
      WadeCole <$> runMessage msg attrs
    _ -> WadeCole <$> runMessage msg attrs
