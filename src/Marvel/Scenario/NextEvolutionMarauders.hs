module Marvel.Scenario.NextEvolutionMarauders (runMaraudersScenarioMessage) where

import Marvel.Prelude
import Data.HashMap.Strict qualified as HashMap
import Marvel.Campaign
import Marvel.Card
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Keyword (Keyword (Permanent))
import Marvel.Difficulty
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Environment.Cards qualified as Environments
import Marvel.Game.Source
import Marvel.Id
import Marvel.Trait (Trait (Morlock))
import Marvel.Status (StatusCard (ToughStatus))
import Text.Read (readMaybe)
import Data.Text qualified as T
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Scenario.Types
import Marvel.Villain.Cards qualified as Villains
import System.Random.Shuffle (shuffleM)

marauderCodes :: Bool -> [CardCode]
marauderCodes expert = [CardCode (tshow n <> if expert then "b" else "a") | n <- [40070 .. 40076 :: Int]]

villainDef :: CardCode -> CardDef
villainDef code = fromMaybe (error "missing NeXt Evolution Marauder") $ HashMap.lookup code Villains.allVillainsMap

runMaraudersScenarioMessage :: ReverseQueue m => Message -> Attrs Scenario -> m (Attrs Scenario)
runMaraudersScenarioMessage msg a = runQueueT $ case msg of
  StartScenario -> do
    campaign <- getCampaign
    let expert = scenarioDifficulty a == Expert
        excluded = if scenarioId a == "40103"
          then maybe [] (map toBaseCardCode . (.campaignLog.nextEvolution.maraudersDefeated)) campaign
          else []
        candidates = filter ((`notElem` excluded) . toBaseCardCode) (marauderCodes expert)
    shuffled <- shuffleM candidates
    case shuffled of
      first : rest -> do
        villainCards <- traverse (genEncounterCard . villainDef) rest
        started <- runMessage StartScenario a
          {scenarioVillains = [first], scenarioVictoryCondition = Just 0}
        let setAsideCodes = ["40079","40080"]
            withheld = [card | card <- scenarioEncounterDeck started, toCardCode card `elem` setAsideCodes]
            matchingMinion = CardCode $ tshow (40094 + fromMaybe 0 ((subtract 40070) <$> readMaybeCode first))
            deck = if scenarioId a == "40077"
              then filter ((`notElem` setAsideCodes) . toCardCode) (scenarioEncounterDeck started)
              else filter ((/= matchingMinion) . toCardCode) (scenarioEncounterDeck started)
        pure started
          {scenarioEncounterDeck = deck
          ,scenarioSetAsideCards = scenarioSetAsideCards started
             <> [EncounterCard card | card <- villainCards, scenarioId a == "40077"]
             <> [EncounterCard card | card <- withheld, scenarioId a == "40077"]}
      [] -> error "No Marauder villain remains for On the Run"
  ResolveVillainDefeat vid | scenarioId a == "40077" -> do
    def <- getsGame (`abilityCardDef` VillainRef vid)
    routed <- selectList (EnvironmentWithTitle "Routed")
    for_ def \definition -> for_ (take 1 routed) \eid -> do
      let card = MkEncounterCard (case vid of VillainId uuid -> CardId uuid) definition
          count = fromMaybe 0 (scenarioVictoryCondition a) + 1
      removeFromPlay vid
      cardSpecific eid "neHoldDefeatedVillain" card
      cardSpecific GameRef "neAfterRoutingVillain" (count,vid)
    pure a {scenarioVictoryCondition = Just $ fromMaybe 0 (scenarioVictoryCondition a) + 1}
  CardSpecific GameRef "neAfterRoutingVillain" (fromJSON -> Success (count,oldVillain)) | scenarioId a == "40077" -> do
    if (count :: Natural) >= 3 then gameOver Won >> pure a else
      case [card | EncounterCard card <- scenarioSetAsideCards a, cdCardType (getCardDef card) == VillainType] of
        next : _ -> do
          let name = cdName $ getCardDef next
          minions <- selectList AnyMinion
          matching <- filterM (\mid -> getsGame $ \g -> maybe False ((== name) . cdName) (abilityCardDef g $ MinionRef mid)) minions
          traverse_ removeFromPlay matching
          addVillain (toCardCode next)
          cardSpecific GameRef "neActivateNewMarauder" (oldVillain :: VillainId)
          pure a {scenarioSetAsideCards = filter (/= EncounterCard next) (scenarioSetAsideCards a)}
        [] -> error "Marauder villain deck exhausted before three defeats"
  CardSpecific GameRef "neActivateNewMarauder" (fromJSON -> Success oldVillain) | scenarioId a == "40077" -> do
    villains <- selectList AnyVillain
    for_ (take 1 villains) \vid -> do
      setActiveVillain vid
      -- Permanent modular attachments survive a departing villain and attach to
      -- the new villain instead (FFG answer, 2024-05-10).
      attachments <- selectList (AttachmentOnEnemy $ EnemyVillainId oldVillain)
      for_ attachments \aid -> do
        def <- getsGame (`abilityCardDef` AttachmentRef aid)
        when (maybe False (elem Permanent . cdKeywords) def) $ attachTo aid vid
      players <- getPlayers
      for_ players (villainActivatesAgainst vid)
    pure a
  CardSpecific GameRef "neMorlockAdvanceWithTough" (fromJSON -> Success mid) | scenarioId a == "40077" -> do
    nextMainScheme (mid :: MainSchemeId)
    cardSpecific GameRef "neToughAllMorlocks" ()
    pure a
  CardSpecific GameRef "neToughAllMorlocks" _ | scenarioId a == "40077" -> do
    morlocks <- selectList (AllyWithTrait Morlock)
    traverse_ (`giveStatus` ToughStatus) morlocks
    pure a
  ResolveVillainDefeat _ | scenarioId a == "40103" -> a <$ gameOver Won
  _ -> runMessage msg a

readMaybeCode :: CardCode -> Maybe Int
readMaybeCode = readMaybe . T.unpack . unCardCode . toBaseCardCode
