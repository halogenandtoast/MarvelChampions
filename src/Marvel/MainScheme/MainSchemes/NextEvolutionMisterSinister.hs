module Marvel.MainScheme.MainSchemes.NextEvolutionMisterSinister where

import Marvel.Ally.Cards qualified as Allies
import Marvel.EncounterSet qualified as Set
import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword qualified as Keyword
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Status
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.NextEvolution.Sinister

import Marvel.Card
import Marvel.Entity
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Ability (HasAbilities)
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype MisterSinisterMainScheme = MisterSinisterMainScheme (Attrs MainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40139 :: MainSchemeCard MisterSinisterMainScheme
ne40139 = mainScheme MisterSinisterMainScheme Cards.ne40139 (PerPlayer 0) (PerPlayer 0) (PerPlayer 0)

ne40140 :: MainSchemeCard MisterSinisterMainScheme
ne40140 = mainScheme MisterSinisterMainScheme Cards.ne40140 (PerPlayer 5) (PerPlayer 1) (PerPlayer 2)

ne40141 :: MainSchemeCard MisterSinisterMainScheme
ne40141 = mainScheme MisterSinisterMainScheme Cards.ne40141 (PerPlayer 5) (PerPlayer 1) (PerPlayer 2)

ne40142 :: MainSchemeCard MisterSinisterMainScheme
ne40142 = mainScheme MisterSinisterMainScheme Cards.ne40142 (PerPlayer 5) (PerPlayer 1) (PerPlayer 2)

ne40143 :: MainSchemeCard MisterSinisterMainScheme
ne40143 = mainScheme MisterSinisterMainScheme Cards.ne40143 (PerPlayer 7) (Static 0) (PerPlayer 1)

instance IsMainScheme MisterSinisterMainScheme where
  mainSchemeAbilities = getAbilities

instance RunMessage MisterSinisterMainScheme where
  runMessage msg card@(MisterSinisterMainScheme a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      villain <- sinister
      hopes <- selectList (AllyWithTitle "Hope Summers")
      minions <- selectList (MinionIs Allies.ne40130)
      let characters = map AllyCharacter hopes <> map MinionCharacter minions
      for_ villain \vid -> for_ (headMay characters) (redirectVillainAttackToCharacter vid)
      pure card
    SetupMainScheme | toCardCode a == "40139" -> do
      first <- selectJust FirstPlayer
      searchEverywhereAndPutIntoPlayEncounterCard first (CardIs Allies.ne40130)
      pure card
    MainSchemeMessage mid RevealMainScheme | mid == mainSchemeId a -> do
      case toCardCode a of
        "40139" -> advanceMainScheme mid
        "40140" -> addPower Set.FlightSet "40151"
        "40141" -> addPower Set.SuperStrengthSet "40155"
        "40142" -> addPower Set.TelepathySet "40159"
        "40143" -> traverse_ dealEncounterCard =<< getPlayers
        _ -> pure ()
      pure card
    AdvanceMainScheme mid | mid == mainSchemeId a -> card <$
      if toCardCode a == "40143" then gameOver Lost else nextMainScheme mid
    _ -> MisterSinisterMainScheme <$> runMessage msg a

instance HasAbilities MisterSinisterMainScheme where
  getAbilities (MisterSinisterMainScheme a) =
    [limitedWindowAbility a 1 (W.EnemyWouldAttack (EnemyWithTitle "Mister Sinister") AnyIdentity) ForcedInterrupt NoCriteria NoCost (runAbility a 1) | toCardCode a == "40143"]

addPower :: ReverseQueue m => Set.EncounterSet -> CardCode -> m ()
addPower set attachment = do
  cards <- filter ((== Just set) . cdEncounterSet . getCardDef) <$> getSetAsideEncounterCards
  villain <- sinister
  for_ villain \vid -> for_ (filter ((== attachment) . toCardCode) cards) (\c -> attachEncounterCard c vid)
  shuffleIntoEncounterDeck [c | c <- cards, toCardCode c /= attachment]
