module Marvel.Scenario.Shadowland where

import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Prelude
import Marvel.Card
import Marvel.Campaign qualified as Campaign
import Marvel.EncounterCard (genEncounterCard)
import Marvel.EncounterSet qualified as Set
import Marvel.Game.Source
import Marvel.Id (IdentityId, AttachmentId, MinionId(..))
import Marvel.Keyword (Keyword(Guard,Villainous))
import Data.HashSet qualified as HashSet
import Marvel.Name (title)
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Scenario.Types
import Marvel.Shadowland.Identity (heroWithTitleInGame)
import Marvel.Shadowland.Cards qualified as Cards
import Marvel.Trait qualified as T
import System.Random.Shuffle (shuffleM)

-- The chosen protege is persistent state: save/load and undo preserve the draw.
data ShadowlandScenario = ShadowlandScenario (Attrs Scenario) (Maybe CardCode)
  deriving stock (Show, Eq)
instance IsScenario ShadowlandScenario where
  toScenarioAttrs (ShadowlandScenario a _) = a
  overScenarioAttrs f (ShadowlandScenario a chosen) = ShadowlandScenario (f a) chosen
instance ToJSON ShadowlandScenario where
  toJSON (ShadowlandScenario a chosen) = case toJSON a of
    Object o -> Object $ KeyMap.insert "shadowlandChosen" (toJSON chosen) o
    value -> value
instance FromJSON ShadowlandScenario where
  parseJSON = withObject "ShadowlandScenario" $ \o -> ShadowlandScenario <$> parseJSON (Object o) <*> o .:? "shadowlandChosen"

shadowlandScenarios :: [ScenarioCard ShadowlandScenario]
shadowlandScenarios =
  [scenario (`ShadowlandScenario` Nothing) "65001a" ("65012a","65012a") ["65001b","65002b"] [Set.SlShadowsInTheNight]
  ,scenario (`ShadowlandScenario` Nothing) "65038a" ("65037","65037") ["65038b","65039b"] [Set.SlShadowLabyrinth,Set.SlDeadlyContraptions]
  ,scenario (`ShadowlandScenario` Nothing) "65060a" ("65057","65058") ["65060b","65061b"] [Set.SlHeartOfShadow,Set.SlBlackFlower]]

proteges :: [CardDef]
proteges = [Cards.street65012a,Cards.street65013a,Cards.street65014a,Cards.street65015a,Cards.street65016a,Cards.street65017a]

-- The printed links are a many-to-many relation, rather than the ordinary Linked
-- keyword's single card-code dependency.
protegeLinks :: CardCode -> [CardDef]
protegeLinks code = [def | (def,parents) <-
  [(Cards.street65018,["65014","65016","65017"]),(Cards.street65019,["65012","65013","65015"]),
   (Cards.street65020,["65015","65016","65017"]),(Cards.street65021,["65013","65014","65016"]),
   (Cards.street65022,["65012","65015","65017"]),(Cards.street65023,["65012","65013","65014"]),
   (Cards.street65024,["65012","65014","65017"]),(Cards.street65025,["65013","65015","65016"]),
   (Cards.street65026,["65012","65015","65016"]),(Cards.street65027,["65013","65014","65017"])],toBaseCardCode code `elem` parents]

instance RunMessage ShadowlandScenario where
  runMessage msg e@(ShadowlandScenario a chosen) = runQueueT $ case msg of
    StartScenario | scenarioId a `elem` ["65001a","65038a"] -> do
      campaign <- getCampaign
      let removed = map toBaseCardCode $ maybe [] (Campaign.removedProteges . Campaign.shadowland . Campaign.campaignLog) campaign
      candidates <- filterM (\def -> not <$> heroWithTitleInGame (title $ cdName def)) (filter ((`notElem` removed) . toBaseCardCode . toCardCode) proteges)
      shuffled <- shuffleM candidates
      case headMay shuffled of
        Nothing -> pure e
        Just pick -> do
          let code = toCardCode pick
              attrs = if scenarioId a == "65001a" then a {scenarioVillains = [code]} else a
              campaignAttrs = case campaign of
                Just c | Campaign.kind c == Campaign.ShadowlandCampaign -> attrs {scenarioEncounterSets = HashSet.delete Set.SlBlackFlower $ scenarioEncounterSets attrs}
                _ -> attrs
          links <- traverse genEncounterCard (protegeLinks code)
          a' <- runMessage msg campaignAttrs {scenarioEncounterDeck = scenarioEncounterDeck campaignAttrs <> links}
          chosenCard <- genEncounterCard pick
          pure $ ShadowlandScenario (if scenarioId a == "65038a" then a' {scenarioSetAsideCards = EncounterCard chosenCard : scenarioSetAsideCards a'} else a') (Just code)
    CardSpecific GameRef "slCreateTrapDeck" _ -> do
      cards <- getEncounterDeck
      let traps = filter (cardMatch $ CardWithTrait T.Trap) cards
      if any ((== "65040") . toCardCode) traps then createDeck (NamedDeckKey "shadowlandTraps") traps
        else do
          whisper <- genEncounterCard Cards.street65040
          createDeck (NamedDeckKey "shadowlandTraps") (whisper : traps)
      pure e
    CardSpecific GameRef "slRevealTrap" (fromJSON -> Success you) -> fromDeck (NamedDeckKey "shadowlandTraps") (drawAndRevealEncounterCard you) >> pure e
    CardSpecific GameRef "slMixTraps" (fromJSON -> Success (n :: Natural)) -> do
      cards <- take (fromIntegral n) <$> getEncounterDeck
      for_ cards $ cardSpecific GameRef "removeEncounterCardFromPiles"
      fromDeck (NamedDeckKey "shadowlandTraps") $ shuffleIntoEncounterDeck cards >> shuffleEncounterDiscardIntoDeck
      pure e
    CardSpecific GameRef "slWhisperChosen" (fromJSON -> Success (you :: IdentityId,aid :: AttachmentId)) -> do
      let candidates = [card | EncounterCard card <- scenarioSetAsideCards a,Just (toCardCode card) == chosen]
      for_ (headMay candidates) $ \card -> do
        let def = getCardDef card
            converted = def {cdCardType=MinionType,cdTraits=HashSet.insert T.Elite (cdTraits def),cdKeywords=fromList [Guard,Villainous]}
        putEncounterCardIntoPlay you card {ecCardDef=converted}
        attachTo aid (MinionId $ unCardId $ ecCardId card)
      pure e
    CardSpecific GameRef "slForgetChosen" _ -> pure $ ShadowlandScenario a {scenarioSetAsideCards = filter ((/=chosen) . Just . toCardCode) (scenarioSetAsideCards a)} chosen
    CardSpecific GameRef "slRevealChosen" (fromJSON -> Success (you :: IdentityId,hard :: Bool)) -> do
      for_ chosen $ \code -> do
        let def = headMay [d | d <- proteges,toCardCode d == code]
        for_ def $ \original -> do
          minions <- selectList (MinionIs original)
          for_ minions $ \mid -> do
            card <- getsGame $ (`cardInPlay` toRef mid)
            for_ card $ \c -> case c of EncounterCard ec -> setAside [EncounterCard ec {ecCardDef=original}]; _ -> pure ()
      cardSpecific GameRef "slForgetChosen" ()
      for_ chosen $ \code -> addVillain (if hard then flipCardCode code else code)
      when hard $ cardSpecific GameRef "slChosenActivates" you
      pure e
    CardSpecific GameRef "slChosenActivates" _ -> do
      selectOne ActiveVillain >>= traverse_ (\vid -> getAllPlayers >>= traverse_ (villainActivatesAgainst vid))
      pure e
    _ -> (\attrs -> ShadowlandScenario attrs chosen) <$> runMessage msg a
