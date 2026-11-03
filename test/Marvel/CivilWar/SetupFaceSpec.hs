module Marvel.CivilWar.SetupFaceSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as T
import Marvel.Area (DeckKey (..))
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.MainSchemes (civilWarMainSchemeDefinitions,firstStage)
import Marvel.CivilWar.LeaderSpec (withLeader)
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.MainScheme.Types qualified as Main
import Marvel.Minion.Cards qualified as M
import TestImport

isCode :: CardCode -> Choice -> Bool
isCode code = \case
  CardLabel other _ -> code == other
  _ -> False

spec :: Spec
spec = describe "Civil War main-scheme setup faces" do
  for_ [def | def <- civilWarMainSchemeDefinitions,T.isSuffixOf "a" $ unCardCode $ toCardCode def] $ \def ->
    it ("flips the native setup face " <> show (toCardCode def) <> " after finding its leader's side scheme when required") do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- withLeader "56059"
        [found,_] <- stackEncounterDeck [C.cw56072,M.hydraMercenary]
        old <- mainSchemeId
        resolve [ReplaceMainScheme old $ toCardCode def]
        sid <- mainSchemeId
        start [SetupMainScheme]
        when (firstStage def) $ choose "find Stark Tower" (isCode "56072")
        pass
        scheme <- getsGame $ fromMaybe (error "missing flipped scheme") . HashMap.lookup sid . gameMainSchemes
        toCardCode (getCardDef scheme) `shouldBe` flipCardCode (toCardCode def)
        getsGame (HashMap.member (sideSchemeOf found) . gameSideSchemes) `shouldReturn` firstStage def
        getEncounterDiscard >>= (`shouldSatisfy` notElem found)

  it "gives the enemy team the competitive setup search, then places initial threat on the gameplay face" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
          opponent = player2 tg
      _ <- withLeader "56137"
      _ <- addSecondBoard tg
      [found,_] <- stackEncounterDeck [C.cw56206a,M.hydraMercenary]
      start [AddSetupMainScheme "56141a"]
      [(sid,_)] <- getsGame $ filter ((== "56141a") . toCardCode . getCardDef . snd) . HashMap.toList . gameMainSchemes
      choose "begin main scheme setup" (isTargetChoice sid)
      fmap fst <$> openQuestion `shouldReturn` Just opponent
      choose "opponents find Choosing Sides" (isCode "56206a")
      pass
      scheme <- getsGame $ fromMaybe (error "missing gameplay face") . HashMap.lookup sid . gameMainSchemes
      toCardCode (getCardDef scheme) `shouldBe` "56141b"
      Main.mainSchemeThreat (toAttrs scheme) `shouldBe` 0
      sideSchemeThreat (sideSchemeOf found) `shouldReturn` 4
      playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` []

  it "finds only its own deck's set-aside copy of Choosing Sides" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
          opponent = player2 tg
      ownLeader <- villainIdNow
      otherLeader <- addSecondBoard tg
      let ownKey = VillainDeckKey ownLeader
          otherKey = VillainDeckKey otherLeader
      own <- encounterCard C.cw56128a
      other <- encounterCard C.cw56206a
      resolve [CreateDeck ownKey [own],CreateDeck otherKey [other],SetAside $ map EncounterCard [own,other]]
      getSetAsideEncounterCardsForDeck ownKey `shouldReturn` [own]
      getSetAsideEncounterCardsForDeck otherKey `shouldReturn` [other]
      [old] <- getMainSchemesIn =<< getAreaOfTarget you
      resolve [ReplaceMainScheme old "56063a"]
      start [SetupMainScheme]
      fmap fst <$> openQuestion `shouldReturn` Just opponent
      offers (isCode "56128a") `shouldReturn` True
      offers (isCode "56206a") `shouldReturn` False
      choose "find the receiving deck's own permanent card" (isCode "56128a")
      pass
      getsGame (HashMap.member (sideSchemeOf own) . gameSideSchemes) `shouldReturn` True
      getsGame (HashMap.member (sideSchemeOf other) . gameSideSchemes) `shouldReturn` False
      getSetAsideEncounterCardsForDeck otherKey `shouldReturn` [other]
