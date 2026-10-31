module Marvel.SideScheme.SideSchemes.MystiquesManipulations where

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
import Marvel.GameValue
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Treachery.Cards qualified as Treacheries

mystiquesManipulations :: SideSchemeCard MystiquesManipulations
mystiquesManipulations = sideScheme MystiquesManipulations Cards.mystiquesManipulations (PerPlayer 2)

newtype MystiquesManipulations = MystiquesManipulations (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage MystiquesManipulations where
  runMessage msg s@(MystiquesManipulations attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      you <- maybe getYou pure (sideSchemeDefeatedBy attrs)
      key <- getDeckKeyFor you
      pile <- getPile key
      let searched = pileDeck pile <> pileDiscard pile
          copies = filter (cardMatch $ CardIs Treacheries.misled) searched
      unless (null copies) $ chooseOrRunOneSearched you (map EncounterCard searched)
        [targeting card do
          removeFromGame card
          shuffleIntoDeck you [encounterCardInPlayerDeck you card]
        | card <- take 1 copies]
      fromDeck key shuffleEncounterDeck
      pure s
    _ -> MystiquesManipulations <$> runMessage msg attrs
