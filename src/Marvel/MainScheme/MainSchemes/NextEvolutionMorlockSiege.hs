module Marvel.MainScheme.MainSchemes.NextEvolutionMorlockSiege where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Card
import Marvel.Entity
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Difficulty
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Environment.Cards qualified as Environments
import Marvel.Game.Source
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Phase
import Marvel.Query
import Marvel.Trait (Trait (Morlock))
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype MorlockSiegeMainScheme = MorlockSiegeMainScheme (Attrs MainScheme)
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40077 :: MainSchemeCard MorlockSiegeMainScheme
ne40077 = mainScheme MorlockSiegeMainScheme Cards.ne40077 (PerPlayer 6) (PerPlayer 1) (PerPlayer 1)

ne40078 :: MainSchemeCard MorlockSiegeMainScheme
ne40078 = mainScheme MorlockSiegeMainScheme Cards.ne40078 (PerPlayer 8) (Static 0) (PerPlayer 1)

instance IsMainScheme MorlockSiegeMainScheme where
  mainSchemeAbilities (MorlockSiegeMainScheme a) =
    [ability a 1 Action (ForAnyPlayer <> EncounterDiscardHas (CardWithTitle "Hide!"))
      (ExhaustAlliesWithTraitCost Morlock 1 (Just 1)) (runAbility a 1)
    | toCardCode a == "40078"]

instance RunMessage MorlockSiegeMainScheme where
  runMessage msg e@(MorlockSiegeMainScheme a) = runQueueT $ case msg of
    SetupMainScheme | toCardCode a == "40077" -> do
      first <- selectJust FirstPlayer
      difficulty <- getDifficulty
      putEncounterCardIntoPlay first =<< genEncounterCard
        (if difficulty == Expert then Environments.ne40081b else Environments.ne40081a)
      pure e
    VillainPhaseStep StepEnemiesActivate | toCardCode a == "40077" -> do
      placeCounters (mainSchemeId a) 1
      when (mainSchemeCounters a + 1 >= 3) $
        cardSpecific GameRef "neMorlockAdvanceWithTough" (mainSchemeId a)
      pure e
    MainSchemeMessage mid RevealMainScheme | mid == mainSchemeId a, toCardCode a == "40078" -> do
      players <- getPlayers
      let recipients = if length players == 1 then players <> players else players
      for_ recipients (cardSpecific a "nePutMorlock")
      cardSpecific a "neShuffleHide" ()
      pure e
    CardSpecific target "nePutMorlock" (fromJSON -> Success you) | target == toRef a -> do
      cards <- filter ((== "40079") . toCardCode) <$> getSetAsideEncounterCards
      for_ (take 1 cards) (putEncounterCardIntoPlay you)
      pure e
    CardSpecific target "neShuffleHide" _ | target == toRef a -> do
      cards <- filter ((== "40080") . toCardCode) <$> getSetAsideEncounterCards
      shuffleIntoEncounterDeck cards
      pure e
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      cards <- filter ((== "40080") . toCardCode) <$> getEncounterDiscard
      shuffleIntoEncounterDeck cards
      pure e
    -- Departures are checked after the current message has removed the ally. Damage
    -- defeat remains possible despite Morlocks' card-ability departure protection.
    RemoveFromPlay _ | toCardCode a == "40078" -> e <$ cardSpecific a "neCheckMorlocks" ()
    RemoveFromGame _ | toCardCode a == "40078" -> e <$ cardSpecific a "neCheckMorlocks" ()
    DiscardedCard _ | toCardCode a == "40078" -> e <$ cardSpecific a "neCheckMorlocks" ()
    AddToVictoryDisplay _ | toCardCode a == "40078" -> e <$ cardSpecific a "neCheckMorlocks" ()
    ReturnToHand _ | toCardCode a == "40078" -> e <$ cardSpecific a "neCheckMorlocks" ()
    CardSpecific target "neCheckMorlocks" _ | target == toRef a, toCardCode a == "40078" -> do
      anyMorlocks <- selectAny (AllyWithTrait Morlock)
      unless anyMorlocks $ gameOver Lost
      pure e
    AdvanceMainScheme mid | mid == mainSchemeId a -> do
      if toCardCode a == "40077" then nextMainScheme mid else gameOver Lost
      pure e
    _ -> MorlockSiegeMainScheme <$> runMessage msg a
