module Marvel.CivilWar.MainSchemeSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Game.Public (publicGame)
import Marvel.Message.Lifted qualified as L
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (withLeader)
import Marvel.Keyword qualified as K
import Marvel.MainScheme.Types qualified as Main
import Marvel.Minion.Cards qualified as M
import Marvel.Treachery.Cards qualified as T
import Marvel.Modifier
import TestImport

replaceScheme :: CardCode -> GameAppT MainSchemeId
replaceScheme code = do
  old <- mainSchemeId
  resolve [ReplaceMainScheme old code]
  mainSchemeId

spec :: Spec
spec = describe "Civil War main schemes" do
  for_ ["56063b","56172b","56121b","56199b","56122b","57044b","57005b","56200b"] $ \code ->
    for_ [("solo",["spider-man"],0,7),("two heroes",["spider-man","captain-marvel"],4,14)] $ \(description,heroes,initial,target) ->
      it (show code <> " resolves its initial threat and target for " <> description) do
        tg <- testGame heroes
        inGame tg do
          sid <- replaceScheme code
          mainSchemeThreat `shouldReturn` initial
          scheme <- getsGame $ fromMaybe (error "missing scheme") . lookup sid . gameMainSchemes
          Main.getThreshold (toAttrs scheme) `shouldReturn` target

  for_ ["56063b","56172b"] $ \code ->
    it (show code <> " limits its treachery threat to once per player per phase") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        _ <- replaceScheme code
        vid <- withLeader "57040"
        resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 10 FromAbility]
        first <- encounterCard T.hardToKeepDown
        second <- encounterCard T.hardToKeepDown
        resolve [RevealEncounterCard you first,RevealEncounterCard you second]
        mainSchemeThreat `shouldReturn` 1
        _ <- stackEncounterDeck [M.hydraMercenary,M.hydraMercenary]
        endTurn
        pass
        before <- mainSchemeThreat
        third <- encounterCard T.hardToKeepDown
        resolve [RevealEncounterCard you third]
        mainSchemeThreat `shouldReturn` (before + 1)

  for_ ["57005b","56200b"] $ \code ->
    it (show code <> " requires two stuns to cancel the leader's attack and removes both") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        _ <- replaceScheme code
        vid <- withLeader "56059"
        changeForm you
        _ <- stackEncounterDeck [M.hydraMercenary,M.hydraMercenary]
        resolve [VillainMessage vid (VillainStunned GameRef)]
        start [VillainMessage vid $ VillainAttacks you]
        pass
        firstDamage <- damageOf you
        liftIO $ firstDamage `shouldSatisfy` (> 0)
        getCharacterStatusCount (VillainCharacter vid) StunnedStatus `shouldReturn` 1
        resolve [VillainMessage vid (VillainStunned GameRef),VillainMessage vid $ VillainAttacks you]
        damageOf you `shouldReturn` firstDamage
        getCharacterStatusCount (VillainCharacter vid) StunnedStatus `shouldReturn` 0
        resolve [VillainMessage vid (VillainStunned GameRef)]
        start [VillainMessage vid $ VillainAttacks you]
        pass
        damageOf you >>= (\damage -> liftIO $ damage `shouldSatisfy` (> firstDamage))

  for_ ["56121b","56199b"] $ \code ->
    it (show code <> " grants Guard to ordinary minions while it is in play") do
      tg <- testGame ["spider-man"]
      inGame tg do
        sid <- replaceScheme code
        minion <- addMinion (player1 tg) M.weaponsRunner
        getEnemyKeywords (EnemyMinionId minion) >>= (`shouldSatisfy` elem K.Guard)
        resolve [ReplaceMainScheme sid "01097"]
        getEnemyKeywords (EnemyMinionId minion) >>= (`shouldSatisfy` notElem K.Guard)

  for_ ["56124b","57045b"] $ \code ->
    it (show code <> " exhausts allies as they enter play") do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- replaceScheme code
        cat <- putIntoPlay (player1 tg) "01067"
        getsGame (fmap (allyExhausted . toAttrs) . lookup (allyIdOf cat) . gameAllies) `shouldReturn` Just True

  for_ ["56122b","57044b"] $ \code ->
    it (show code <> " discards three cards after the villain phase threat step") do
      tg <- testGame ["spider-man"]
      inGame tg do
        sid <- replaceScheme code
        _ <- withLeader "57040"
        _ <- stackEncounterDeck [M.hydraMercenary,M.hydraMercenary]
        endTurn
        before <- unDeck . playerIdentityDeck <$> playerAttrs (player1 tg)
        mainSchemeThreat `shouldReturn` 1
        choose "resolve the main scheme after threat" (isAbilityOf sid)
        after <- unDeck . playerIdentityDeck <$> playerAttrs (player1 tg)
        liftIO $ after `shouldBe` drop 3 before
        mainSchemeThreat `shouldReturn` 3

  for_ ["56142b","57006b"] $ \code ->
    it (show code <> " increases an undefended enemy leader attack by two") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        _ <- replaceScheme code
        vid <- withLeader "57040"
        changeForm you
        _ <- stackEncounterDeck [M.hydraMercenary]
        start [VillainMessage vid $ VillainAttacks you]
        pass
        damageOf you `shouldReturn` 4
        getModifiers (VillainRef vid) >>= (`shouldSatisfy` notElem (AttackModifier 2))

  for_ ["56142b","57006b"] $ \code ->
    it (show code <> " gives only one extra ATK to a defended attack") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        _ <- replaceScheme code
        vid <- withLeader "57040"
        changeForm you
        _ <- stackEncounterDeck [M.hydraMercenary]
        start [VillainMessage vid $ VillainAttacks you]
        let decline = whenM (offers isNoResponse) $ choose "decline optional responses" isNoResponse >> decline
        decline
        choose "defend with Spider-Man" isDefend
        pass
        damageOf you `shouldReturn` 0

  for_ ["56124b","57045b","56142b","57006b"] $ \code ->
    it (show code <> " loses a cooperative game when its nine-per-player target is reached") do
      tg <- testGame ["spider-man","captain-marvel"]
      inGame tg do
        sid <- replaceScheme code
        resolve [MainSchemeMessage sid $ MainSchemePlaceThreat 17]
        getsGame gameState `shouldReturn` InProgress
        resolve [MainSchemeMessage sid $ MainSchemePlaceThreat 1]
        getsGame gameState `shouldReturn` Finished Lost

  it "captures an ally defeated by an enemy attack as one hidden physical card" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      sid <- replaceScheme "56064b"
      vid <- withLeader "57040"
      changeForm you
      cat <- putIntoPlay you "01067"
      _ <- stackEncounterDeck [M.hydraMercenary]
      start [VillainMessage vid $ VillainAttacks you]
      let decline = whenM (offers isNoResponse) $ choose "decline optional responses" isNoResponse >> decline
      decline
      choose "defend with Black Cat" $ \case AllyDefend aid _ -> aid == allyIdOf cat;_ -> False
      choose "capture the defeated ally" (isAbilityOf sid)
      pass
      L.heldCardsAt sid `shouldReturn` [cat]
      elem "01067" . map toCardCode <$> discardOf you `shouldReturn` False
      scheme <- getsGame $ fromMaybe (error "missing prison") . lookup sid . gameMainSchemes
      Main.getThreshold (toAttrs scheme) `shouldReturn` 8
      view <- liftIO . publicGame =<< getGame
      let prisoners = [(count,cards) | Object root <- [view],Just (Array schemes) <- [KeyMap.lookup "mainSchemes" root],Object schemeView <- toList schemes,Just count <- [KeyMap.lookup "held" schemeView],Just cards <- [KeyMap.lookup "attachedCards" schemeView]]
      liftIO $ prisoners `shouldBe` [(Number 1,toJSON ([] :: [Value]))]
      game <- getGame
      case fromJSON @Game (toJSON game) of
        Error err -> liftIO $ expectationFailure err
        Success restored -> liftIO $ toJSON restored `shouldBe` toJSON game

  it "does not capture an ally defeated by consequential damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      sid <- replaceScheme "56064b"
      cat <- putIntoPlay you "01067"
      resolve [AllyMessage (allyIdOf cat) $ AllyDamaged GameRef $ toDamage 2 FromConsequential]
      L.heldCardsAt sid `shouldReturn` []
      elem "01067" . map toCardCode <$> discardOf you `shouldReturn` True

  it "completes Negative Zone Prison immediately when a captured card lowers its target to its threat" do
    tg <- testGame ["spider-man"]
    inGame tg do
      sid <- replaceScheme "56064b"
      resolve [MainSchemeMessage sid $ MainSchemePlaceThreat 8]
      card <- putInHand (player1 tg) "01067"
      messages <- L.runQueueT $ L.capture $ L.placePlayerCardUnder sid card
      resolve messages
      getsGame gameState `shouldReturn` Finished Lost

  it "Guerilla Warfare adds threat only for status cards actually placed on its enemy leader" do
    tg <- testGame ["spider-man"]
    inGame tg do
      sid <- replaceScheme "56201b"
      vid <- withLeader "57041"
      resolve [VillainMessage vid (VillainStunned GameRef)]
      mainSchemeThreat `shouldReturn` 1
      resolve [VillainMessage vid (VillainStunned GameRef)]
      mainSchemeThreat `shouldReturn` 2
      resolve [VillainMessage vid (VillainStunned GameRef)]
      mainSchemeThreat `shouldReturn` 2
      resolve [VillainMessage vid VillainBecomeTough,VillainMessage vid VillainBecomeTough]
      mainSchemeThreat `shouldReturn` 3
      offers (isAbilityOf sid) `shouldReturn` False

  it "captures the original encounter card when a controlled minion is defeated as an ally" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      sid <- replaceScheme "56064b"
      vid <- withLeader "57040"
      changeForm you
      mid <- addMinion you M.weaponsRunner
      resolve [TakeControlOfMinion you mid]
      let aid = AllyId $ unMinionId mid
      _ <- stackEncounterDeck [M.hydraMercenary]
      start [VillainMessage vid $ VillainAttacks you]
      let decline = whenM (offers isNoResponse) $ choose "decline optional responses" isNoResponse >> decline
      decline
      choose "defend with the controlled minion" $ \case AllyDefend other _ -> other == aid;_ -> False
      choose "capture the defeated controlled minion" (isAbilityOf sid)
      pass
      scheme <- getsGame $ fromMaybe (error "missing prison") . lookup sid . gameMainSchemes
      liftIO $ map toCardCode (Main.mainSchemeHeldEncounterCards $ toAttrs scheme) `shouldBe` [toCardCode M.weaponsRunner]
      L.heldCardsAt sid `shouldReturn` []
      encounterDiscardCodes >>= (`shouldSatisfy` notElem (toCardCode M.weaponsRunner))
      Main.getThreshold (toAttrs scheme) `shouldReturn` 8

  it "Guerilla Warfare ignores a stun blocked by Stalwart" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- replaceScheme "56201b"
      vid <- withLeader "56059"
      helmet <- encounterCard C.cw56067
      resolve [RevealEncounterCard (player1 tg) helmet]
      mainSchemeThreat `shouldReturn` 1
      resolve [VillainMessage vid $ VillainStunned GameRef]
      mainSchemeThreat `shouldReturn` 1

  it "Guerilla Warfare counts the replacement tough from Contaminant Immunity without counting the prevented stun" do
    tg <- testGame ["spider-man"]
    inGame tg do
      _ <- replaceScheme "56201b"
      vid <- withLeader "56168"
      immunity <- encounterCard C.cw56175
      resolve [RevealEncounterCard (player1 tg) immunity,VillainMessage vid $ VillainStunned GameRef]
      mainSchemeThreat `shouldReturn` 1
      getCharacterStatusCount (VillainCharacter vid) StunnedStatus `shouldReturn` 0
      getCharacterStatusCount (VillainCharacter vid) ToughStatus `shouldReturn` 1

  it "loads main schemes saved before captured-card fields and leader metadata were introduced" do
    tg <- testGame ["spider-man"]
    inGame tg do
      sid <- replaceScheme "56121b"
      scheme <- getsGame $ fromMaybe (error "missing main scheme") . lookup sid . gameMainSchemes
      let legacy = case toJSON scheme of
            Object o -> Object $ foldr KeyMap.delete o ["mainSchemeHeldEncounterCards","mainSchemeHeldCardsFacedown","_withMetadata"]
            value -> value
      case fromJSON @Main.MainScheme legacy of
        Error err -> liftIO $ expectationFailure err
        Success restored -> liftIO $ toAttrs restored `shouldBe` toAttrs scheme
