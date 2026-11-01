module Marvel.Minion.Minions.RogueMystique where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Trait qualified as Trait ()
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner
import Marvel.Treachery.Cards qualified as Treacheries

rogueMystique :: MinionCard RogueMystique
rogueMystique = minion RogueMystique Cards.rogueMystique (Sch 1) (Atk 1) (HP 6)

newtype RogueMystique = RogueMystique (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities RogueMystique where
  getAbilities (RogueMystique attrs) = [windowAbility attrs 1 (W.EngagesMinion W.After AnyIdentity $ MinionWithId $ minionId attrs) ForcedResponse NoCost (runAbility attrs 1)]

instance RunMessage RogueMystique where
  runMessage msg m@(RogueMystique attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      let you = minionEngagedIdentity attrs
      key <- getDeckKeyFor you
      pile <- getPile key
      aside <- getSetAsideEncounterCards
      let searched = pileDeck pile <> pileDiscard pile <> aside
          copies = filter (cardMatch $ CardIs Treacheries.misled) searched
      unless (null copies) $ chooseOrRunOneSearched you (map EncounterCard searched)
        [targeting card do
          removeFromGame card
          shuffleIntoDeck you [encounterCardInPlayerDeck you card]
        | card <- take 1 copies]
      fromDeck key shuffleEncounterDeck
      pure m
    _ -> RogueMystique <$> runMessage msg attrs
