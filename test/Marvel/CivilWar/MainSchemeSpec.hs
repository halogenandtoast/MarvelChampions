module Marvel.CivilWar.MainSchemeSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Data.HashMap.Strict qualified as HashMap
import Marvel.Game.Public (publicGame)
import Marvel.Message.Lifted qualified as L
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (withLeader)
import Marvel.Keyword qualified as K
import Marvel.MainScheme.Types qualified as Main
import Marvel.Minion.Cards qualified as M
import Marvel.Treachery.Cards qualified as T
import Marvel.SideScheme.Cards qualified as S
import Marvel.Trait qualified as Trait
import Marvel.Modifier
import TestImport

replaceScheme :: CardCode -> GameAppT MainSchemeId
replaceScheme code = do
  old <- mainSchemeId
  resolve [ReplaceMainScheme old code]
  mainSchemeId

spec :: Spec
spec = describe "Civil War main schemes" do
  for_ ["56063b","56172b","56121b","56199b","56122b","57044b","57005b","56200b","56096b","56141b"] $ \code ->
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
            Object o -> Object $ foldr KeyMap.delete o ["mainSchemeHeldEncounterCards","mainSchemeHeldCardsFacedown","_withMetadata","cwMainLeader","cwUnregisteredCards"]
            value -> value
      case fromJSON @Main.MainScheme legacy of
        Error err -> liftIO $ expectationFailure err
        Success restored -> liftIO $ toAttrs restored `shouldBe` toAttrs scheme

  for_ ["56096b","56141b"] $ \code ->
    it (show code <> " searches only the deck and deals one chosen minion facedown to each player") do
      tg <- testGame ["spider-man","captain-marvel"]
      inGame tg do
        let p = player1 tg
            q = player2 tg
            selected card = \case CardLabel other _ -> other == toCardCode card;_ -> False
        _ <- setEncounterDeck [M.hydraMercenary,M.weaponsRunner,T.hardToKeepDown] [M.armoredGuard]
        old <- mainSchemeId
        start [ReplaceMainScheme old code]
        offers (selected M.armoredGuard) `shouldReturn` False
        choose "deal Hydra Mercenary to the first player" (selected M.hydraMercenary)
        choose "deal Weapons Runner to the second player" (selected M.weaponsRunner)
        map toCardCode . playerIdentityEncounterCards <$> playerAttrs p `shouldReturn` [toCardCode M.hydraMercenary]
        map toCardCode . playerIdentityEncounterCards <$> playerAttrs q `shouldReturn` [toCardCode M.weaponsRunner]
        getsGame (length . gameMinions) `shouldReturn` 0
        encounterDiscardCodes `shouldReturn` [toCardCode M.armoredGuard]

  for_ ["56123b","56173b"] $ \code ->
    it (show code <> " searches both deck and discard and deals side schemes facedown without revealing them") do
      tg <- testGame ["spider-man","captain-marvel"]
      inGame tg do
        let p = player1 tg
            q = player2 tg
            selected card = \case CardLabel other _ -> other == toCardCode card;_ -> False
        _ <- setEncounterDeck [S.breakinAndTakin,T.hardToKeepDown] [S.crowdControl]
        old <- mainSchemeId
        start [ReplaceMainScheme old code]
        choose "deal the side scheme from the deck" (selected S.breakinAndTakin)
        choose "deal the side scheme from the discard" (selected S.crowdControl)
        map toCardCode . playerIdentityEncounterCards <$> playerAttrs p `shouldReturn` [toCardCode S.breakinAndTakin]
        map toCardCode . playerIdentityEncounterCards <$> playerAttrs q `shouldReturn` [toCardCode S.crowdControl]
        getsGame (length . gameSideSchemes) `shouldReturn` 0
        encounterDiscardCodes `shouldReturn` []

  it "continues without an empty search question when there are fewer minions than players" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      _ <- setEncounterDeck [M.hydraMercenary,T.hardToKeepDown] []
      _ <- replaceScheme "56096b"
      map toCardCode . playerIdentityEncounterCards <$> playerAttrs (player1 tg) `shouldReturn` [toCardCode M.hydraMercenary]
      playerIdentityEncounterCards <$> playerAttrs (player2 tg) `shouldReturn` []

  it "Superhero Jailbreak puts discarded minions into play without resolving their reveal keywords" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      sid <- replaceScheme "56202b"
      _ <- withLeader "57040"
      _ <- setEncounterDeck ([M.weaponsRunner,T.hardToKeepDown] <> replicate 8 M.hydraMercenary) []
      endTurn
      endTurn
      mainSchemeThreat `shouldReturn` 2
      choose "resolve Superhero Jailbreak" (isAbilityOf sid)
      choose "discard for the first player first" (isTargetChoice $ player1 tg)
      minions <- getsGame $ HashMap.elems . gameMinions
      liftIO $ [(toCardCode (getCardDef minion),minionEngagedIdentity $ toAttrs minion) | minion <- minions] `shouldBe` [(toCardCode M.weaponsRunner,player1 tg)]
      playerIdentityEncounterCards <$> playerAttrs (player1 tg) `shouldReturn` []
      playerIdentityEncounterCards <$> playerAttrs (player2 tg) `shouldReturn` []
      encounterDiscardCodes >>= (`shouldSatisfy` elem (toCardCode T.hardToKeepDown))

  it "Superhero Jailbreak completes every player's discard before a discarded Quickstrike minion attacks" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
      changeForm p
      sid <- replaceScheme "56202b"
      _ <- withLeader "57040"
      resolve [IdentityMessage p $ IdentitySetHitPoints 1]
      _ <- setEncounterDeck ([M.vulture,M.hydraMercenary] <> replicate 8 M.weaponsRunner) []
      endTurn
      endTurn
      choose "resolve Superhero Jailbreak" (isAbilityOf sid)
      choose "discard for the first player first" (isTargetChoice $ player1 tg)
      choose "put the first player's minion into play first" (isTargetChoice p)
      encounterDiscardCodes >>= (`shouldSatisfy` elem (toCardCode M.hydraMercenary))
      minions <- getsGame $ HashMap.elems . gameMinions
      liftIO $ map (toCardCode . getCardDef) minions `shouldBe` [toCardCode M.vulture]
      damageOf p `shouldReturn` 9

  it "Superhero Jailbreak lets the first player assign the first discard to another player" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      sid <- replaceScheme "56202b"
      _ <- withLeader "57040"
      _ <- setEncounterDeck ([M.weaponsRunner,T.hardToKeepDown] <> replicate 8 M.hydraMercenary) []
      endTurn
      endTurn
      choose "resolve Superhero Jailbreak" (isAbilityOf sid)
      fmap fst <$> asked `shouldReturn` Just (player1 tg)
      choose "discard for the second player first" (isTargetChoice $ player2 tg)
      minions <- getsGame $ HashMap.elems . gameMinions
      liftIO $ [(toCardCode $ getCardDef minion,minionEngagedIdentity $ toAttrs minion) | minion <- minions] `shouldBe` [(toCardCode M.weaponsRunner,player2 tg)]

  for_ ["56122b","57044b"] $ \code ->
    it (show code <> " lets the first player order the players' deck discards") do
      tg <- testGame ["spider-man","captain-marvel"]
      inGame tg do
        let p = player1 tg
            q = player2 tg
        sid <- replaceScheme code
        _ <- withLeader "57040"
        _ <- setEncounterDeck (replicate 8 M.hydraMercenary) []
        endTurn
        endTurn
        pBefore <- unDeck . playerIdentityDeck <$> playerAttrs p
        qBefore <- unDeck . playerIdentityDeck <$> playerAttrs q
        choose "resolve the main scheme" (isAbilityOf sid)
        fmap fst <$> asked `shouldReturn` Just p
        offers (isTargetChoice p) `shouldReturn` True
        offers (isTargetChoice q) `shouldReturn` True
        choose "discard for the second player first" (isTargetChoice q)
        unDeck . playerIdentityDeck <$> playerAttrs p `shouldReturn` drop 3 pBefore
        unDeck . playerIdentityDeck <$> playerAttrs q `shouldReturn` drop 3 qBefore

  it "Superhero Jailbreak lets the first player choose the entry order after all discards are known" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      changeForm p
      sid <- replaceScheme "56202b"
      _ <- withLeader "57040"
      _ <- setEncounterDeck ([M.vulture,M.hydraMercenary] <> replicate 8 M.weaponsRunner) []
      endTurn
      endTurn
      choose "resolve Superhero Jailbreak" (isAbilityOf sid)
      choose "discard for the first player first" (isTargetChoice p)
      getsGame (length . gameMinions) `shouldReturn` 0
      view <- liftIO . publicGame =<< getGame
      let previews = [code | Object root <- [view],Just (Object questions) <- [KeyMap.lookup "question" root],Object question <- KeyMap.elems questions,Just (Array choices) <- [KeyMap.lookup "choices" question],Object choice <- toList choices,Just code <- [KeyMap.lookup "code" choice]]
      liftIO $ previews `shouldMatchList` [toJSON $ toCardCode M.vulture,toJSON $ toCardCode M.hydraMercenary]
      choose "put the second player's minion into play first" (isTargetChoice q)
      minions <- getsGame $ HashMap.elems . gameMinions
      liftIO $ [(toCardCode $ getCardDef minion,minionEngagedIdentity $ toAttrs minion) | minion <- minions] `shouldMatchList` [(toCardCode M.vulture,p),(toCardCode M.hydraMercenary,q)]

  it "Superhero Jailbreak does not put one physical minion into play twice if a refill discards it for two players" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      sid <- replaceScheme "56202b"
      _ <- withLeader "57040"
      _ <- setEncounterDeck [M.weaponsRunner] []
      endTurn
      endTurn
      choose "resolve Superhero Jailbreak" (isAbilityOf sid)
      choose "discard for the first player first" (isTargetChoice p)
      choose "put the second player's minion into play first" (isTargetChoice q)
      minions <- getsGame $ HashMap.elems . gameMinions
      liftIO $ [(toCardCode $ getCardDef minion,minionEngagedIdentity $ toAttrs minion) | minion <- minions] `shouldBe` [(toCardCode M.weaponsRunner,q)]
      playerIdentityMinions <$> playerAttrs p `shouldReturn` mempty

  it "does not extend a deck-only minion search into cards shuffled from the discard pile after dealing" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      _ <- setEncounterDeck [M.hydraMercenary] [M.weaponsRunner]
      _ <- replaceScheme "56096b"
      map toCardCode . playerIdentityEncounterCards <$> playerAttrs (player1 tg) `shouldReturn` [toCardCode M.hydraMercenary]
      playerIdentityEncounterCards <$> playerAttrs (player2 tg) `shouldReturn` []

  it "allows Aerial Recon to prevent a searched card from being dealt and leaves that card in the deck" do
    tg <- testGame ["falcon"]
    inGame tg do
      let p = player1 tg
      changeForm p
      recon <- putIntoPlay p "53009"
      messages <- L.runQueueT $ L.capture $ L.placeCounters (upgradeIdOf recon) 1
      resolve messages
      _ <- setEncounterDeck [M.hydraMercenary,T.hardToKeepDown] []
      old <- mainSchemeId
      start [ReplaceMainScheme old "56096b"]
      choose "choose the searched minion" $ \case CardLabel code _ -> code == toCardCode M.hydraMercenary;_ -> False
      choose "prevent the facedown deal with Aerial Recon" (isAbilityOf $ upgradeIdOf recon)
      pass
      playerIdentityEncounterCards <$> playerAttrs p `shouldReturn` []
      getCountersOn (upgradeIdOf recon) `shouldReturn` 0
      map toCardCode <$> getEncounterDeck >>= (`shouldSatisfy` elem (toCardCode M.hydraMercenary))

  it "resumes a saved opponent search after its first selection without losing either assignment" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      _ <- setEncounterDeck [M.hydraMercenary,M.weaponsRunner,T.hardToKeepDown] []
      old <- mainSchemeId
      start [ReplaceMainScheme old "56096b"]
      choose "choose the first minion" $ \case CardLabel code _ -> code == toCardCode M.hydraMercenary;_ -> False
      playerIdentityEncounterCards <$> playerAttrs (player1 tg) `shouldReturn` []
      game <- getGame
      case fromJSON @Game (toJSON game) of
        Error err -> liftIO $ expectationFailure err
        Success restored -> withGame_ (const restored)
      choose "choose the second minion after loading" $ \case CardLabel code _ -> code == toCardCode M.weaponsRunner;_ -> False
      map toCardCode . playerIdentityEncounterCards <$> playerAttrs (player1 tg) `shouldReturn` [toCardCode M.hydraMercenary]
      map toCardCode . playerIdentityEncounterCards <$> playerAttrs (player2 tg) `shouldReturn` [toCardCode M.weaponsRunner]

  for_ [C.cw56106,C.cw56114] $ \def ->
    it (show (toCardCode def) <> " grants Unregistered to its owner's identity and can be discarded by paying an identity-specific card in alter-ego") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        card <- encounterCard def
        resolve [RevealEncounterCard you card]
        let oid = ObligationId $ unCardId $ ecCardId card
        getGainedTraits you >>= (`shouldSatisfy` member Trait.Unregistered)
        clearHand you
        basic <- putInHand you "01088"
        offers (isAbilityOf (ObligationRef oid)) `shouldReturn` False
        signature <- putInHand you "01005"
        choose "discard the obligation" (isAbilityOf (ObligationRef oid))
        choose "discard an identity-specific card as the cost" (isTargetChoice signature)
        pass
        handOf you >>= (`shouldSatisfy` elem basic)
        getGainedTraits you >>= (`shouldSatisfy` not . member Trait.Unregistered)
        encounterDiscardCodes >>= (`shouldSatisfy` elem (toCardCode def))

  for_ [C.cw56106,C.cw56114] $ \def ->
    it (show (toCardCode def) <> " makes Hunting Rebel Heroes damage an already-Unregistered hero") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        changeForm you
        _ <- replaceScheme "56097b"
        getGainedTraits you >>= (`shouldSatisfy` member Trait.Unregistered)
        card <- encounterCard def
        resolve [RevealEncounterCard you card]
        damageOf you `shouldReturn` 2
        another <- encounterCard T.hardToKeepDown
        resolve [RevealEncounterCard you another]
        damageOf you `shouldReturn` 2

  it "Hunting Rebel Heroes gives only heroes Unregistered and does not damage an alter-ego for Unregistered Super" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      changeForm p
      _ <- replaceScheme "56097b"
      getGainedTraits p >>= (`shouldSatisfy` member Trait.Unregistered)
      getGainedTraits q >>= (`shouldSatisfy` not . member Trait.Unregistered)
      card <- encounterCard C.cw56106
      resolve [RevealEncounterCard q card]
      getGainedTraits q >>= (`shouldSatisfy` member Trait.Unregistered)
      damageOf p `shouldReturn` 0
      damageOf q `shouldReturn` 0

  it "Government Contractor starts with three fixed threat plus hinder and grants every identity Unregistered" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      changeForm p
      card <- encounterCard C.cw56120
      resolve [RevealEncounterCard p card]
      sideSchemeThreat (SideSchemeId $ unCardId $ ecCardId card) `shouldReturn` 5
      getGainedTraits p >>= (`shouldSatisfy` member Trait.Unregistered)
      getGainedTraits q >>= (`shouldSatisfy` member Trait.Unregistered)

  it "Hunting Rebel Heroes damages each current hero when Government Contractor is revealed" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      changeForm p
      _ <- replaceScheme "56097b"
      card <- encounterCard C.cw56120
      resolve [RevealEncounterCard p card]
      damageOf p `shouldReturn` 2
      damageOf q `shouldReturn` 0

  it "only the owner can pay Unregistered Super's alter-ego action" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      card <- encounterCard C.cw56106
      resolve [RevealEncounterCard p card]
      _ <- putInHand q "01035"
      endTurn
      offers (isAbilityOf $ ObligationRef $ ObligationId $ unCardId $ ecCardId card) `shouldReturn` False
      getGainedTraits p >>= (`shouldSatisfy` member Trait.Unregistered)

  it "each revealed Unregistered Super triggers Hunting Rebel Heroes independently" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- replaceScheme "56097b"
      first <- encounterCard C.cw56106
      second <- encounterCard C.cw56114
      resolve [RevealEncounterCard you first,RevealEncounterCard you second]
      damageOf you `shouldReturn` 4
      getAccelerationCount `shouldReturn` 2

  it "discarding one of two Unregistered Supers preserves the other's trait and acceleration icon" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      first <- encounterCard C.cw56106
      second <- encounterCard C.cw56114
      resolve [RevealEncounterCard you first,RevealEncounterCard you second]
      clearHand you
      signature <- putInHand you "01005"
      choose "discard the first obligation" (isAbilityOf $ ObligationRef $ ObligationId $ unCardId $ ecCardId first)
      choose "pay its identity-specific card cost" (isTargetChoice signature)
      pass
      getGainedTraits you >>= (`shouldSatisfy` member Trait.Unregistered)
      getAccelerationCount `shouldReturn` 1
      encounterDiscardCodes >>= (`shouldSatisfy` elem (toCardCode C.cw56106))
      encounterDiscardCodes >>= (`shouldSatisfy` notElem (toCardCode C.cw56114))

  it "Hunting Rebel Heroes damages both heroes when one Government Contractor is revealed" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      changeForm p
      resolve [IdentityMessage q $ ChangedToForm A]
      _ <- replaceScheme "56097b"
      card <- encounterCard C.cw56120
      resolve [RevealEncounterCard p card]
      damageOf p `shouldReturn` 2
      damageOf q `shouldReturn` 2

  it "putting Government Contractor into play does not count as revealing a trait-granting card" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- replaceScheme "56097b"
      card <- encounterCard C.cw56120
      resolve [PutEncounterCardIntoPlay you card]
      sideSchemeThreat (SideSchemeId $ unCardId $ ecCardId card) `shouldReturn` 4
      damageOf you `shouldReturn` 0
      resolve [RevealEncounterCard you card]
      damageOf you `shouldReturn` 2

  it "preserves Guerilla Warfare's leader reference when loading the earlier optional-metadata format" do
    tg <- testGame ["spider-man"]
    inGame tg do
      sid <- replaceScheme "56201b"
      vid <- villainIdNow
      scheme <- getsGame $ fromMaybe (error "missing Guerilla Warfare") . lookup sid . gameMainSchemes
      let legacy = case toJSON scheme of
            Object o -> Object $ KeyMap.insert "_withMetadata" (toJSON vid) $ KeyMap.delete "cwMainLeader" $ KeyMap.delete "cwUnregisteredCards" o
            value -> value
      case fromJSON @Main.MainScheme legacy of
        Error err -> liftIO $ expectationFailure err
        Success restored -> withGame_ $ entitiesL . mainSchemesL . ix sid .~ restored
      resolve [VillainMessage vid VillainBecomeTough]
      mainSchemeThreat `shouldReturn` 1
