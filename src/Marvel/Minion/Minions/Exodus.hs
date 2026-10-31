module Marvel.Minion.Minions.Exodus where

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
import Marvel.Resource
import Marvel.Window qualified as W
import Marvel.Trait qualified as Trait
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

exodus :: MinionCard Exodus
exodus = minion Exodus Cards.exodus (Sch 1) (Atk 1) (HP 6)

newtype Exodus = Exodus (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Exodus
instance RunMessage Exodus where
  runMessage msg m@(Exodus attrs) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId attrs -> do
      key <- getDeckKeyFor you
      pile <- getPile key
      let searched = pileDeck pile <> pileDiscard pile
          shields = filter (cardMatch $ CardIs Attachments.psionicShield) searched
      unless (null shields) $ chooseOrRunOneSearched you (map EncounterCard searched)
        [targeting card (attachEncounterCard card mid) | card <- take 1 shields]
      fromDeck key shuffleEncounterDeck
      Exodus <$> runMessage msg attrs
    _ -> Exodus <$> runMessage msg attrs
