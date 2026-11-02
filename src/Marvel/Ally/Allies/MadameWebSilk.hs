module Marvel.Ally.Allies.MadameWebSilk where
import Marvel.Prelude
import Marvel.Query ()
import Data.List (delete)
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers ()
import Marvel.Question ()
import Marvel.Window qualified as W
import Marvel.Trait qualified as T
import Marvel.Damage ()
import Marvel.Game.Source
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Silk.Cards qualified as Cards
import Marvel.Ally.Runner

madameWebSilk :: AllyCard MadameWebSilk
madameWebSilk = ally MadameWebSilk Cards.madameWebSilk (Thw 2, 1) (Atk 1, 1) (HP 2)
newtype MadameWebSilk = MadameWebSilk (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage MadameWebSilk where
  runMessage msg u@(MadameWebSilk attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      n <- countControlledCardsWithTrait you T.WebWarrior
      lookAtTopOfEncounterDeck n attrs
      pure u
    SearchFoundCards (isTarget attrs -> True) (onlyEncounterCards -> cards) -> do
      let you = allyController attrs
      lookingAt you (map EncounterCard cards) $ chooseOne you (labeled "$label.done" (cardSpecific attrs "order" ([] :: [EncounterCard],cards)) : [targeting card (discardCard card >> cardSpecific attrs "order" ([] :: [EncounterCard],delete card cards)) | card <- cards])
      pure u
    CardSpecific (isTarget attrs -> True) "order" (fromJSON -> Success (chosen :: [EncounterCard],rest :: [EncounterCard])) -> do
      if length rest <= 1 then putOnTopOfEncounterDeck (chosen <> rest) else chooseOne (allyController attrs) [targeting card (cardSpecific attrs "order" (chosen <> [card],delete card rest)) | card <- rest]
      pure u
    _ -> MadameWebSilk <$> runMessage msg attrs

instance HasAbilities MadameWebSilk where
  getAbilities u@(MadameWebSilk _attrs) = [limitedWindowAbility u 1 W.EntersPlayThis Response OwnsThis NoCost (runAbility u 1)]
