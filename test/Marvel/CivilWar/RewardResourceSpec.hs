module Marvel.CivilWar.RewardResourceSpec (spec) where

import Marvel.Area (DeckKey (..),Pile (..))
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (withLeader,attachmentIdOf)
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
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
