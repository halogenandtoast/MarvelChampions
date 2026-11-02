module Marvel.SideScheme.SideSchemes.GoingUndercoverShuri where
import Marvel.Prelude
import Marvel.Id ()
import Data.List (delete)
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Matchers ()
import Marvel.Question ()
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.GameValue

goingUndercoverShuri :: SideSchemeCard GoingUndercoverShuri
goingUndercoverShuri = sideScheme GoingUndercoverShuri Cards.goingUndercoverShuri (Static 4)
newtype GoingUndercoverShuri = GoingUndercoverShuri (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage GoingUndercoverShuri where
  runMessage msg u@(GoingUndercoverShuri attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      lookAtTopOfEncounterDeck 5 attrs
      pure u
    SearchFoundCards (isTarget attrs -> True) (onlyEncounterCards -> cards) -> do
      let you = fromMaybe (error "defeated player side scheme has no defeating player") (sideSchemeDefeatedBy attrs)
      eligible <- filterM (\card -> not <$> isScenarioSpecificEncounterCard card) cards
      lookingAt you (map EncounterCard cards) $ chooseOne you (labeled "$label.done" (cardSpecific attrs "order" ([] :: [EncounterCard],[] :: [EncounterCard],cards)) : [targeting card (addCardToVictoryDisplay card >> cardSpecific attrs "order" ([] :: [EncounterCard],[] :: [EncounterCard],delete card cards)) | card <- eligible])
      pure u
    CardSpecific (isTarget attrs -> True) "order" (fromJSON -> Success (top :: [EncounterCard],bottom :: [EncounterCard],rest :: [EncounterCard])) -> do
      let you = fromMaybe (error "defeated player side scheme has no defeating player") (sideSchemeDefeatedBy attrs)
      if null rest then putOnTopOfEncounterDeck top >> putOnBottomOfEncounterDeck bottom else chooseOne you ([targeting card (chooseOne you [labeled "$label.placeOnTop" (cardSpecific attrs "order" (top <> [card],bottom,delete card rest)),labeled "$label.placeOnBottom" (cardSpecific attrs "order" (top,bottom <> [card],delete card rest))]) | card <- rest])
      pure u
    _ -> GoingUndercoverShuri <$> runMessage msg attrs
