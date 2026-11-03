module Marvel.CivilWar.RewardResourceSpec (spec) where

import Marvel.Area (DeckKey (..),Pile (..))
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (withLeader,attachmentIdOf)
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.MainScheme.Types qualified as Main
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as M
import Marvel.Villain.Types qualified as V
import TestImport

leaders :: TestGame -> CardCode -> GameAppT (VillainId,VillainId)
leaders tg ownCode = do
  enemy <- withLeader "56059"
  own <- addSecondBoard tg
  resolve [ReplaceVillain own ownCode Swapped,VillainMessage own SetVillainHp]
  pure (own,enemy)

buyTenacity :: IdentityId -> CardCode -> GameAppT PlayerCard
buyTenacity you code = do
  resource <- putInHand you code
  purchase <- putInHand you "01093"
  playWith purchase [resource]
  pure resource

spec :: Spec
spec = describe "Civil War leader reward resources" do
  it "Suit Up spends two wild resources and optionally gives the friendly Iron Man tough" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,enemy) <- leaders tg "56059"
      changeForm you
      resource <- buyTenacity you "56132"
      choose "use Suit Up" (isLabel "$label.cwSuitUp")
      pass
      getCharacterStatusCount (VillainCharacter own) ToughStatus `shouldReturn` 1
      getCharacterStatusCount (VillainCharacter enemy) ToughStatus `shouldReturn` 0
      discardOf you >>= (`shouldSatisfy` any ((== pcCardId resource) . pcCardId))
      length <$> upgradesWith "01093" `shouldReturn` 1

  it "the spender may decline Suit Up without undoing the two resources spent" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,_) <- leaders tg "56059"
      changeForm you
      resource <- buyTenacity you "56132"
      choose "decline the response" (isLabel "$label.noResponse")
      pass
      getCharacterStatusCount (VillainCharacter own) ToughStatus `shouldReturn` 0
      discardOf you >>= (`shouldSatisfy` any ((== pcCardId resource) . pcCardId))
      length <$> upgradesWith "01093" `shouldReturn` 1

  for_ [("56132","56059","$label.cwSuitUp"),("56136","56092","$label.cwRawPower")
       ,("56210","56137","$label.cwPeoplesHero"),("56214","56168","$label.cwSecretContact")] $ \(code,leader,label) ->
    it (show code <> " still provides two wild resources in alter-ego form without its Hero Response") do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let you = player1 tg
        _ <- leaders tg leader
        energy <- encounterCard C.cw56098
        finesse <- encounterCard C.cw56174
        when (code == "56136") $ resolve [RevealEncounterCard (player2 tg) energy]
        when (code == "56214") $ resolve [RevealEncounterCard (player2 tg) finesse]
        _ <- buyTenacity you code
        offers (isLabel label) `shouldReturn` False
        pass
        length <$> upgradesWith "01093" `shouldReturn` 1

  it "does not offer Suit Up when Iron Man is only the enemy leader" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- leaders tg "56137"
      changeForm you
      _ <- buyTenacity you "56132"
      offers (isLabel "$label.cwSuitUp") `shouldReturn` False
      pass
      length <$> upgradesWith "01093" `shouldReturn` 1

  it "does not offer Suit Up when the friendly leader already has its maximum tough statuses" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,_) <- leaders tg "56059"
      resolve [GiveStatus (VillainRef own) ToughStatus]
      changeForm you
      _ <- buyTenacity you "56132"
      offers (isLabel "$label.cwSuitUp") `shouldReturn` False
      pass
      getCharacterStatusCount (VillainCharacter own) ToughStatus `shouldReturn` 1

  it "Raw Power places two energy counters on the friendly Captain Marvel's Energy Channel" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- leaders tg "56092"
      energy <- encounterCard C.cw56098
      resolve [RevealEncounterCard (player2 tg) energy]
      changeForm you
      _ <- buyTenacity you "56136"
      choose "use Raw Power" (isLabel "$label.cwRawPower")
      pass
      getCountersOn (attachmentIdOf energy) `shouldReturn` 2

  it "does not offer Raw Power without the referenced attachment" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- leaders tg "56092"
      changeForm you
      _ <- buyTenacity you "56136"
      offers (isLabel "$label.cwRawPower") `shouldReturn` False
      pass
      length <$> upgradesWith "01093" `shouldReturn` 1

  it "The People's Hero gives the friendly leader a facedown card from that leader's encounter deck" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,enemy) <- leaders tg "56137"
      boost <- encounterCard M.hydraMercenary
      filler <- encounterCard M.shocker
      other <- encounterCard M.weaponsRunner
      resolve [CreateDeck (VillainDeckKey own) [boost,filler],CreateDeck (VillainDeckKey enemy) [other]]
      changeForm you
      _ <- buyTenacity you "56210"
      choose "use The People's Hero" (isLabel "$label.cwPeoplesHero")
      pass
      getsGame (maybe [] (V.villainBoostCards . toAttrs) . lookup own . gameVillains) `shouldReturn` [boost]
      pileDeck <$> getPile (VillainDeckKey own) `shouldReturn` [filler]
      pileDeck <$> getPile (VillainDeckKey enemy) `shouldReturn` [other]

  it "Secret Contact respects Finesse's maximum of three skill counters" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- leaders tg "56168"
      finesse <- encounterCard C.cw56174
      resolve [RevealEncounterCard (player2 tg) finesse,PlaceCounters (AttachmentRef $ attachmentIdOf finesse) 2]
      changeForm you
      _ <- buyTenacity you "56214"
      choose "use Secret Contact" (isLabel "$label.cwSecretContact")
      pass
      getCountersOn (attachmentIdOf finesse) `shouldReturn` 3

  it "does not offer Secret Contact when Finesse already has three counters" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- leaders tg "56168"
      finesse <- encounterCard C.cw56174
      resolve [RevealEncounterCard (player2 tg) finesse,PlaceCounters (AttachmentRef $ attachmentIdOf finesse) 3]
      changeForm you
      _ <- buyTenacity you "56214"
      offers (isLabel "$label.cwSecretContact") `shouldReturn` False
      pass
      getCountersOn (attachmentIdOf finesse) `shouldReturn` 3

  it "offers the Hero Response when the resource pays an ability cost" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,_) <- leaders tg "56059"
      tenacity <- putIntoPlay you "01093"
      changeForm you
      L.runQueueT (L.capture $ L.exhaust you) >>= resolve
      resource <- putInHand you "56132"
      choose "use Tenacity" (isAbilityOfCard tenacity)
      payWith [resource]
      choose "use Suit Up after spending for an ability" (isLabel "$label.cwSuitUp")
      pass
      getCharacterStatusCount (VillainCharacter own) ToughStatus `shouldReturn` 1
      isExhaustedNow you `shouldReturn` False
      upgradesWith "01093" `shouldReturn` []

  for_ [False,True] $ \hero ->
    it ("Legal Counsel moves one threat only as a Hero Response: hero " <> show hero) do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let you = player1 tg
        (own,_) <- leaders tg "57001"
        [local] <- getMainSchemesIn =<< getAreaOfTarget you
        [other] <- getMainSchemesIn =<< getAreaOfTarget own
        L.runQueueT (L.capture $ L.placeThreatOn GameRef (SchemeMainSchemeId local) 3) >>= resolve
        before <- getsGame $ maybe 0 (Main.mainSchemeThreat . toAttrs) . lookup other . gameMainSchemes
        when hero $ changeForm you
        _ <- buyTenacity you "57035"
        offers (isLabel "$label.cwLegalCounsel") `shouldReturn` hero
        when hero $ choose "use Legal Counsel" (isLabel "$label.cwLegalCounsel")
        pass
        getsGame (maybe 0 (Main.mainSchemeThreat . toAttrs) . lookup local . gameMainSchemes) `shouldReturn` if hero then 2 else 3
        getsGame (maybe 0 (Main.mainSchemeThreat . toAttrs) . lookup other . gameMainSchemes) `shouldReturn` (before + if hero then 1 else 0)
        length <$> upgradesWith "01093" `shouldReturn` 1

  it "Legal Counsel has no response without threat but still pays two resources" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- leaders tg "57001"
      changeForm you
      _ <- buyTenacity you "57035"
      offers (isLabel "$label.cwLegalCounsel") `shouldReturn` False
      pass
      length <$> upgradesWith "01093" `shouldReturn` 1

  it "Legal Counsel can be declined without moving threat or undoing payment" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- leaders tg "57001"
      [local] <- getMainSchemesIn =<< getAreaOfTarget you
      L.runQueueT (L.capture $ L.placeThreatOn GameRef (SchemeMainSchemeId local) 2) >>= resolve
      changeForm you
      _ <- buyTenacity you "57035"
      choose "decline Legal Counsel" (isLabel "$label.noResponse")
      pass
      getsGame (maybe 0 (Main.mainSchemeThreat . toAttrs) . lookup local . gameMainSchemes) `shouldReturn` 2
      length <$> upgradesWith "01093" `shouldReturn` 1

  for_ [C.cw57046a,C.cw57046b] $ \def ->
    it ("Android Assistance flips the friendly Vision's existing mass form: " <> show (toCardCode def)) do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let you = player1 tg
        (own,_) <- leaders tg "57040"
        form <- encounterCard def
        resolve [RevealEncounterCard (player2 tg) form]
        changeForm you
        _ <- buyTenacity you "57077"
        choose "use Android Assistance" (isLabel "$label.cwAndroidAssistance")
        pass
        getAttachedCharacter (attachmentIdOf form) `shouldReturn` Just (VillainCharacter own)
        getsGame (fmap (toCardCode . getCardDef) . lookup (attachmentIdOf form) . gameAttachments) `shouldReturn`
          Just (if toCardCode def == "57046a" then "57046b" else "57046a")
        length <$> upgradesWith "01093" `shouldReturn` 1

  it "Android Assistance has no interrupt in alter ego" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- leaders tg "57040"
      form <- encounterCard C.cw57046a
      resolve [RevealEncounterCard (player2 tg) form]
      _ <- buyTenacity you "57077"
      offers (isLabel "$label.cwAndroidAssistance") `shouldReturn` False
      pass
      getsGame (fmap (toCardCode . getCardDef) . lookup (attachmentIdOf form) . gameAttachments) `shouldReturn` Just "57046a"

  it "Android Assistance can be declined" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- leaders tg "57040"
      form <- encounterCard C.cw57046a
      resolve [RevealEncounterCard (player2 tg) form]
      changeForm you
      _ <- buyTenacity you "57077"
      choose "decline Android Assistance" (isLabel "$label.noResponse")
      pass
      getsGame (fmap (toCardCode . getCardDef) . lookup (attachmentIdOf form) . gameAttachments) `shouldReturn` Just "57046a"

  it "Android Assistance triggers while paying an ability cost before that ability resolves" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- leaders tg "57040"
      form <- encounterCard C.cw57046a
      resolve [RevealEncounterCard (player2 tg) form]
      tenacity <- putIntoPlay you "01093"
      changeForm you
      L.runQueueT (L.capture $ L.exhaust you) >>= resolve
      resource <- putInHand you "57077"
      choose "use Tenacity" (isAbilityOfCard tenacity)
      payWith [resource]
      isExhaustedNow you `shouldReturn` True
      choose "flip Vision before Tenacity readies the hero" (isLabel "$label.cwAndroidAssistance")
      pass
      isExhaustedNow you `shouldReturn` False
      getsGame (fmap (toCardCode . getCardDef) . lookup (attachmentIdOf form) . gameAttachments) `shouldReturn` Just "57046b"

  for_ [("57035","$label.cwLegalCounsel"),("57077","$label.cwAndroidAssistance")] $ \(code,label) ->
    it (show code <> " still pays two resources without a friendly leader") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        changeForm you
        _ <- buyTenacity you code
        offers (isLabel label) `shouldReturn` False
        pass
        length <$> upgradesWith "01093" `shouldReturn` 1

  it "Android Assistance interrupts before Legal Counsel responds, regardless of spend order" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- leaders tg "57040"
      form <- encounterCard C.cw57046a
      resolve [RevealEncounterCard (player2 tg) form]
      [local] <- getMainSchemesIn =<< getAreaOfTarget you
      L.runQueueT (L.capture $ L.placeThreatOn GameRef (SchemeMainSchemeId local) 2) >>= resolve
      changeForm you
      purchase <- putInHand you "01091"
      legal <- putInHand you "57035"
      android <- putInHand you "57077"
      playWith purchase [legal,android]
      offers (isLabel "$label.cwAndroidAssistance") `shouldReturn` True
      offers (isLabel "$label.cwLegalCounsel") `shouldReturn` False
      choose "interrupt spending first" (isLabel "$label.cwAndroidAssistance")
      offers (isLabel "$label.cwLegalCounsel") `shouldReturn` True
      choose "respond to spending afterward" (isLabel "$label.cwLegalCounsel")
      pass
      getsGame (fmap (toCardCode . getCardDef) . lookup (attachmentIdOf form) . gameAttachments) `shouldReturn` Just "57046b"
      getsGame (maybe 0 (Main.mainSchemeThreat . toAttrs) . lookup local . gameMainSchemes) `shouldReturn` 1
      discardOf you >>= (`shouldSatisfy` \cards -> legal `elem` cards && android `elem` cards)
