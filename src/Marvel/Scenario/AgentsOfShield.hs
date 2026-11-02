{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Scenario.AgentsOfShield where

import Marvel.Prelude
import Control.Monad (replicateM)
import Data.List (partition)
import Data.HashSet qualified as HashSet
import Marvel.AgentsOfShield.EncounterCards qualified as Cards
import Marvel.AgentsOfShield.Helpers
import Marvel.Card
import Marvel.Difficulty
import Marvel.EncounterCard (gatherEncounterSet)
import Marvel.EncounterSet qualified as Set
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Scenario.Types
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import System.Random.Shuffle (shuffleM)

newtype AosScenario = AosScenario (Attrs Scenario `With` [EncounterCard])
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsScenario AosScenario where
  toScenarioAttrs (AosScenario (a `With` _)) = a
  overScenarioAttrs f (AosScenario (a `With` cells)) = AosScenario (f a `With` cells)

aosScenario :: Attrs Scenario -> AosScenario
aosScenario = AosScenario . (`With` [])

aosBlackWidowScenario, aosBatrocScenario, aosModokScenario, aosThunderboltsScenario, aosBaronZemoScenario :: ScenarioCard AosScenario
aosBlackWidowScenario = scenario aosScenario "50064" ("50064","50065") ["50067"] [Set.AosBlackWidow]
aosBatrocScenario = scenario aosScenario "50086a" ("50086a","50086b") ["50087","50088","50089"] [Set.AosBatroc]
aosModokScenario = scenario aosScenario "50103a" ("50103a","50103b") ["50104"] [Set.AosModok]
aosThunderboltsScenario = scenario aosScenario "50129a" ("50129a","50129b") ["50130"] [Set.AosThunderbolts]
aosBaronZemoScenario = scenario aosScenario "50165a" ("50165a","50166a") ["50167","50168","50169"] [Set.AosBaronZemo,Set.AosExecutiveBoard]

thunderboltSets :: [Set.EncounterSet]
thunderboltSets = [Set.AosGravitationalPull,Set.AosHardSound,Set.AosPaleLittleSpider,Set.AosPowerOfTheAtom,Set.AosSupersonic,Set.AosTheLeaper]

instance RunMessage AosScenario where
  runMessage msg e@(AosScenario (a `With` cells)) = runQueueT $ case msg of
    StartScenario | scenarioId a == "50086a" -> do
      def <- lookupPlayerCardInGame "50091"
      captives <- replicateM 4 $ do
        cid <- getRandom
        pure $ PlayerCard $ MkPlayerCard cid def Nothing Nothing
      prepared <- runMessage msg a {scenarioSetAsideCards=scenarioSetAsideCards a <> captives}
      pure $ AosScenario (prepared {scenarioEncounterDeck=filter ((/= "50091") . toCardCode) (scenarioEncounterDeck prepared)} `With` cells)
    StartScenario | scenarioId a == "50103a" -> do
      cards <- gatherEncounterSet Set.AosModok
      let isCell c = toCardCode c `elem` ["50105a","50106a","50107a","50108a"]
          captiveFace c = toCardCode c `elem` ["50105a","50105b","50106a","50106b","50107a","50107b","50108a","50108b"]
          upgrade c = cdCardType (getCardDef c) == EnvironmentType && not (isCell c)
      shuffled <- shuffleM (filter isCell cards)
      let remaining = filter (\c -> not $ captiveFace c || upgrade c) cards
          prepared = a {scenarioEncounterSets=HashSet.delete Set.AosModok (scenarioEncounterSets a),scenarioEncounterDeck=scenarioEncounterDeck a <> remaining,scenarioSetAsideCards=scenarioSetAsideCards a <> map EncounterCard (filter upgrade cards)}
      AosScenario . (`With` shuffled) <$> runMessage msg prepared
    StartScenario | scenarioId a == "50129a" -> do
      players <- getPlayers
      let selected = filter (`HashSet.member` scenarioEncounterSets a) thunderboltSets
      missing <- shuffleM $ filter (`notElem` selected) thunderboltSets
      let chosen = selected <> take (max 0 $ length players+1-length selected) missing
      a' <- runMessage msg a {scenarioEncounterSets=scenarioEncounterSets a <> HashSet.fromList chosen}
      let elite c = cardMatch (CardWithType MinionType <> CardWithTrait Trait.Thunderbolt <> CardWithTrait Trait.Elite) c
          (setAsideMinions,deck) = partition elite (scenarioEncounterDeck a')
      pure $ AosScenario (a' {scenarioEncounterDeck=deck,scenarioSetAsideCards=scenarioSetAsideCards a' <> map EncounterCard setAsideMinions} `With` cells)
    CardSpecific GameRef "aosInitialBoardSecrets" _ -> do
      campaign <- getCampaign
      when (isNothing campaign) do
        selectRefs (EnvironmentWithTrait Trait.BoardMember) >>= traverse_ (`placeCounters` 2)
      pure e
    CardSpecific GameRef "aosSetupModok" _ -> do
      first <- selectJust FirstPlayer
      difficulty <- getDifficulty
      let count = if difficulty == Expert then 2 else 1
      upgrades <- shuffleM [c | EncounterCard c <- scenarioSetAsideCards a, cardMatch (CardWithType EnvironmentType <> CardWithTrait Trait.Adaptoid) c]
      traverse_ (putEncounterCardIntoPlay first) (take count upgrades)
      cardSpecific GameRef "aosNextHoldingCell" ()
      getPlayers >>= traverse_ (\you -> searchAndRevealEncounterCard you $ CardIs Cards.aos50113)
      pure e
    CardSpecific GameRef "aosNextHoldingCell" _ -> do
      case cells of
        [] -> pure e
        cell:rest -> do
          first <- selectJust FirstPlayer
          putEncounterCardIntoPlay first cell
          pure $ AosScenario (a `With` rest)
    CardSpecific GameRef "aosRescueCell" (fromJSON -> Success (eid,code :: CardCode)) -> do
      def <- lookupPlayerCardInGame $ flipCardCode code
      first <- selectJust FirstPlayer
      players <- getPlayers
      let cid = CardId $ unEnvironmentId eid
          card = MkPlayerCard cid def Nothing Nothing
      removeFromGame (EnvironmentRef eid)
      chooseEffect first [targeting you (putIntoPlay you card {pcController=Just you}) | you <- players]
      cardSpecific GameRef "aosNextHoldingCell" ()
      pure e
    CardSpecific GameRef "aosReturnRescuedInhuman" (fromJSON -> Success (card :: PlayerCard)) -> do
      removeFromGame (CardIdRef $ pcCardId card)
      let def = case toCardCode card of
            "50105b" -> Cards.aos50105a
            "50106b" -> Cards.aos50106a
            "50107b" -> Cards.aos50107a
            _ -> Cards.aos50108a
          returned = MkEncounterCard (pcCardId card) def
      present <- selectAny (EnvironmentWithTitle "Holding Cell")
      unless present $ cardSpecific GameRef "aosNextHoldingCell" ()
      pure $ AosScenario (a `With` (cells <> [returned]))
    CardSpecific GameRef "aosRecruitAdaptoidEnvironment" _ -> do
      first <- selectJust FirstPlayer
      upgrades <- shuffleM [c | EncounterCard c <- scenarioSetAsideCards a, cardMatch (CardWithType EnvironmentType <> CardWithTrait Trait.Adaptoid) c]
      for_ (headMay upgrades) (putEncounterCardIntoPlay first)
      when (length upgrades <= 1) $ gameOver Lost
      discard <- getEncounterDiscard
      shuffleIntoEncounterDeck $ filter (cardMatch $ CardIs Cards.aos50113) discard
      pure e
    CardSpecific GameRef "aosRevealThunderbolts" (fromJSON -> Success eid) -> do
      first <- selectJust FirstPlayer
      players <- getPlayers
      difficulty <- getDifficulty
      minions <- shuffleM [c | EncounterCard c <- scenarioSetAsideCards a, cardMatch (CardWithType MinionType <> CardWithTrait Trait.Thunderbolt <> CardWithTrait Trait.Elite) c]
      for_ (zip players minions) $ \(you,card) -> do
        revealEncounterCardFrom W.RevealedFromSetAside you card
        when (difficulty == Expert) $ giveStatus (MinionId $ unCardId $ ecCardId card) ToughStatus
      for_ (headMay $ drop (length players) minions) $ \card -> do
        putEncounterCardIntoPlay first card
        let mid = MinionId $ unCardId $ ecCardId card
        cardSpecific GameRef "aosBackupMinionInitial" (eid,mid)
        when (difficulty == Expert) $ giveStatus mid ToughStatus
      flipEnvironment eid
      pure e
    _ -> AosScenario . (`With` cells) <$> runMessage msg a
