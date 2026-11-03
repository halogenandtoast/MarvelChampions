module Marvel.CivilWar.ShieldOpsSpec (spec) where

import Marvel.Area (AreaId (..))
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (withLeader)
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.Message.Lifted qualified as L
import Marvel.EncounterCard (gatherEncounterSet)
import Marvel.EncounterSet qualified as E
import Marvel.Keyword qualified as K
import Marvel.Minion.Cards qualified as M
import Marvel.MainScheme.Types qualified as Main
import Test.Hspec qualified as H
import TestImport

spec :: Spec
spec = describe "Synthezoid S.H.I.E.L.D. Ops" do
  it "gathers five physical cards, including both Operator copies" do
    cards <- gatherEncounterSet E.CwShieldOps
    length cards `H.shouldBe` 5
    length (filter ((== "57013") . toCardCode) cards) `H.shouldBe` 2

  it "Operator places two threat when revealed and has patrol" do
    tg <- testGame ["spider-man"]
    inGame tg do
      card <- encounterCard C.cw57013
      resolve [RevealEncounterCard (player1 tg) card]
      mainSchemeThreat `shouldReturn` 2
      let mid = MinionId $ unCardId $ ecCardId card
      getEnemyKeywords (EnemyMinionId mid) >>= (`shouldSatisfy` elem K.Patrol)

  it "Strategic Overwatch toughens the enemy leader and every minion" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      minion <- addMinion (player1 tg) M.hydraMercenary
      card <- encounterCard C.cw57014
      resolve [RevealEncounterCard (player1 tg) card]
      getCharacterStatusCount (VillainCharacter vid) ToughStatus `shouldReturn` 1
      getCharacterStatusCount (MinionCharacter minion) ToughStatus `shouldReturn` 1

  it "Strategic Overwatch's boost toughens its activating enemy" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "57040"
      _ <- stackEncounterDeck [C.cw57014]
      resolve [VillainMessage vid VillainSchemes]
      getCharacterStatusCount (VillainCharacter vid) ToughStatus `shouldReturn` 1

  for_ [False,True] $ \hero ->
    it ("Rapid Response uses the discarded minion's printed " <> if hero then "ATK" else "SCH") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        when hero $ changeForm you
        _ <- setEncounterDeck [C.cw56089,M.hydraSoldier,M.hydraMercenary] []
        card <- encounterCard C.cw57015
        resolve [RevealEncounterCard you card]
        damageOf you `shouldReturn` if hero then 2 else 0
        mainSchemeThreat `shouldReturn` if hero then 0 else 1

  it "Homeland Security grants surge only to the first minion each round across players" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let one = player1 tg
          two = player2 tg
      side <- encounterCard C.cw57016
      resolve [RevealEncounterCard one side]
      sideSchemeThreat (SideSchemeId $ unCardId $ ecCardId side) `shouldReturn` 5
      _ <- stackEncounterDeck $ replicate 6 M.hydraMercenary
      first <- encounterCard C.cw57013
      resolve [RevealEncounterCard one first]
      length <$> dealtTo one `shouldReturn` 1
      second <- encounterCard C.cw57013
      resolve [RevealEncounterCard two second]
      dealtTo two `shouldReturn` []
      resolve [EndRound]
      third <- encounterCard C.cw57013
      resolve [RevealEncounterCard two third]
      length <$> dealtTo two `shouldReturn` 1

  it "Strategic Overwatch leaves the opposing board's leader and minions untouched" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let one = player1 tg
          two = player2 tg
      own <- villainIdNow
      other <- addSecondBoard tg
      local <- addMinion one M.hydraMercenary
      foreignMinion <- addMinion two M.hydraMercenary
      card <- encounterCard C.cw57014
      resolve [RevealEncounterCard one card]
      getCharacterStatusCount (VillainCharacter own) ToughStatus `shouldReturn` 1
      getCharacterStatusCount (MinionCharacter local) ToughStatus `shouldReturn` 1
      getCharacterStatusCount (VillainCharacter other) ToughStatus `shouldReturn` 0
      getCharacterStatusCount (MinionCharacter foreignMinion) ToughStatus `shouldReturn` 0

  it "Operator places threat only on its revealing player's main scheme" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let two = player2 tg
      _ <- addSecondBoard tg
      card <- encounterCard C.cw57013
      resolve [RevealEncounterCard two card]
      [local] <- getMainSchemesIn (AreaId 0)
      [foreignScheme] <- getMainSchemesIn (AreaId 1)
      getsGame (fmap Main.getMainSchemeThreat . lookup local . gameMainSchemes) `shouldReturn` Just 0
      getsGame (fmap Main.getMainSchemeThreat . lookup foreignScheme . gameMainSchemes) `shouldReturn` Just 2

  it "an opposing minion reveal neither surges nor consumes Homeland Security" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let one = player1 tg
          two = player2 tg
      own <- villainIdNow
      other <- addSecondBoard tg
      filler <- encounterCard M.hydraMercenary
      foreignFiller <- encounterCard M.hydraMercenary
      piles <- L.runQueueT $ L.capture $ do
        L.createDeck (L.VillainDeckKey own) [filler]
        L.createDeck (L.VillainDeckKey other) [foreignFiller]
      resolve piles
      side <- encounterCard C.cw57016
      resolve [RevealEncounterCard one side]
      foreignMinion <- encounterCard C.cw57013
      resolve [RevealEncounterCard two foreignMinion]
      dealtTo two `shouldReturn` []
      local <- encounterCard C.cw57013
      resolve [RevealEncounterCard one local]
      dealtTo one `shouldReturn` [filler]
