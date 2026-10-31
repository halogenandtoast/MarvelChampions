module Marvel.Event.Events.PsychicRapport where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Stats
import Marvel.Status
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types

psychicRapport :: EventCard PsychicRapport
psychicRapport = event PsychicRapport Cards.psychicRapport

newtype PsychicRapport = PsychicRapport (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PsychicRapport where
  runMessage msg e@(PsychicRapport attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ _) | eid == eventId attrs -> do
      for_ ["Cyclops", "Phoenix"] \title -> do
        selectList (IdentityWithTitle title) >>= traverse_ ready
        selectList (AllyWithTitle title) >>= traverse_ ready
      cardSpecific attrs "chooseRapport" you
      pure e
    CardSpecific (isTarget attrs -> True) "chooseRapport" (fromJSON -> Success you) -> do
      cards <- filter cyclopsCard <$> selectList (InDiscardOf (IdentityWithId you) $ BasicCardMatches HeroSpecificCard)
      force <- selectList (UpgradeNamed "Phoenix Force")
      let choices =
            [labeled "$label.psychicRapportReturnCard" $ chooseOne you [targeting c (addToHand you c) | c <- cards] | notNull cards]
            <> [labeled "$label.psychicRapportPower" $ for_ force (\uid -> getSourceController (UpgradeRef uid) >>= traverse_ (\p -> addPhoenixPower p 2)) | notNull force]
      unless (null choices) $ chooseOne you choices
      pure e
    _ -> PsychicRapport <$> runMessage msg attrs

cyclopsCard :: PlayerCard -> Bool
cyclopsCard c = canonicalCardCode (getCardDef c) `elem` ["33002", "33003", "33004", "33005", "33006", "33007", "33008", "33009", "33010"]
