module Marvel.CivilWar.TaskmasterSpec (spec) where

import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (attachmentIdOf,withLeader)
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.Damage
import Marvel.Keyword qualified as K
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as M
import Marvel.Modifier
import Marvel.EncounterCard (gatherEncounterSet)
import Marvel.EncounterSet qualified as E
import Test.Hspec qualified as H
import TestImport

spec :: Spec
spec = describe "Synthezoid Taskmaster" do
  it "Taskmaster enters tough with retaliate and schemes with a villainous boost" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- stackEncounterDeck [M.hydraSoldier,M.hydraMercenary]
      mid <- addMinion you C.cw57022
      mainSchemeThreat `shouldReturn` 2
      getCharacterStatusCount (MinionCharacter mid) ToughStatus `shouldReturn` 1
      getEnemyKeywords (EnemyMinionId mid) >>= (`shouldSatisfy` elem (K.Retaliate 1))
      resolve [GiveStatus (MinionRef mid) ConfusedStatus]
      getCharacterStatusCount (MinionCharacter mid) ConfusedStatus `shouldReturn` 1

  it "Taskmaster attacks the revealing hero with a villainous boost" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- stackEncounterDeck [M.hydraSoldier,M.hydraMercenary]
      _ <- addMinion you C.cw57022
      damageOf you `shouldReturn` 2

  for_ [C.cw57023,C.cw57024] $ \def -> do
    it (show (toCardCode def) <> " attaches to Taskmaster in preference to the leader") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        _ <- stackEncounterDeck [M.hydraSoldier,M.hydraMercenary]
        mid <- addMinion you C.cw57022
        card <- encounterCard def
        resolve [RevealEncounterCard you card]
        getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (MinionCharacter mid)
        modifiers <- getModifiers (MinionRef mid)
        modifiers `shouldSatisfy` elem (if toCardCode def == "57023" then AttackModifier 1 else SchemeModifier 1)

    it (show (toCardCode def) <> " attaches to the enemy leader without Taskmaster") do
      tg <- testGame ["spider-man"]
      inGame tg do
        vid <- villainIdNow
        card <- encounterCard def
        resolve [RevealEncounterCard (player1 tg) card]
        getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (VillainCharacter vid)


  for_ [(C.cw57023,"01089"),(C.cw57024,"01089")] $ \(def,resource) ->
    it (show (toCardCode def) <> " is discarded by paying its two specified resources in hero form") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        clearHand you
        first <- putInHand you resource
        second <- putInHand you "01090"
        card <- encounterCard def
        resolve [RevealEncounterCard you card]
        offers (isAbilityOf $ attachmentIdOf card) `shouldReturn` False
        changeForm you
        choose "discard the equipment" (isAbilityOf $ attachmentIdOf card)
        payWith [first,second]
        getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing
        getEncounterDiscard >>= (`shouldSatisfy` elem card)


  it "contains five physical cards" do
    cards <- gatherEncounterSet E.CwTaskmasterModular
    length cards `H.shouldBe` 5

  it "the Sword grants piercing and its printed attack modifier" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      own <- withLeader "57040"
      card <- encounterCard C.cw57023
      resolve [RevealEncounterCard you card,GiveStatus (IdentityRef you) ToughStatus]
      changeForm you
      _ <- stackEncounterDeck [M.hydraMercenary,M.hydraSoldier]
      resolve [VillainMessage own $ VillainAttacks you]
      damageOf you `shouldReturn` 3
      getCharacterStatusCount (IdentityCharacter you) ToughStatus `shouldReturn` 0

  it "the Shield reduces each instance of ability damage by one" do
    tg <- testGame ["spider-man"]
    inGame tg do
      own <- withLeader "57040"
      card <- encounterCard C.cw57024
      resolve [RevealEncounterCard (player1 tg) card]
      before <- getCharacterRemainingHitPoints (VillainCharacter own)
      resolve [VillainMessage own $ VillainDamaged GameRef $ toDamage 1 FromAbility]
      getCharacterRemainingHitPoints (VillainCharacter own) `shouldReturn` before
      resolve [VillainMessage own $ VillainDamaged GameRef $ toDamage 3 FromAbility]
      getCharacterRemainingHitPoints (VillainCharacter own) `shouldReturn` (before - 2)

  it "Mimicked Move toughens and gives the enemy leader a facedown boost without Taskmaster" do
    tg <- testGame ["spider-man"]
    inGame tg do
      own <- withLeader "57040"
      [extra,base,next] <- stackEncounterDeck [M.hydraSoldier,M.hydraMercenary,M.hydraSoldier]
      card <- encounterCard C.cw57025
      resolve [RevealEncounterCard (player1 tg) card]
      getCharacterStatusCount (VillainCharacter own) ToughStatus `shouldReturn` 1
      headMay <$> getEncounterDeck `shouldReturn` Just base
      resolve [VillainMessage own VillainSchemes]
      mainSchemeThreat `shouldReturn` 3
      headMay <$> getEncounterDeck `shouldReturn` Just next
      getEncounterDiscard >>= (`shouldSatisfy` elem extra)

  it "Mimicked Move's boost resolves another boost card" do
    tg <- testGame ["spider-man"]
    inGame tg do
      own <- withLeader "57040"
      [_,_,next] <- stackEncounterDeck [C.cw57025,M.hydraSoldier,M.hydraMercenary]
      resolve [VillainMessage own VillainSchemes]
      mainSchemeThreat `shouldReturn` 3
      headMay <$> getEncounterDeck `shouldReturn` Just next

  it "Mimicked Move makes Taskmaster attack the friendly leader with a villainous boost" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      own <- withLeader "57040"
      friendly <- addSecondBoard tg
      _ <- stackEncounterDeck [M.hydraMercenary,M.hydraSoldier,M.hydraMercenary]
      _ <- addMinion you C.cw57022
      before <- getCharacterRemainingHitPoints (VillainCharacter friendly)
      card <- encounterCard C.cw57025
      resolve [RevealEncounterCard you card]
      getCharacterRemainingHitPoints (VillainCharacter friendly) `shouldReturn` (before - 2)
      getCharacterStatusCount (VillainCharacter own) ToughStatus `shouldReturn` 0
      damageOf you `shouldReturn` 0

  it "Academy rewards its own board's leader when defeated by an opposing player" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      own <- withLeader "57040"
      other <- addSecondBoard tg
      card <- encounterCard C.cw57026
      resolve [RevealEncounterCard you card]
      let sid = SideSchemeId $ unCardId $ ecCardId card
      sideSchemeThreat sid `shouldReturn` 4
      _ <- stackEncounterDeck [M.hydraMercenary,M.hydraSoldier]
      L.runQueueT (L.capture $ L.removeSchemeThreat (player2 tg) (SchemeSideSchemeId sid) 4) >>= resolve
      getCharacterStatusCount (VillainCharacter own) ToughStatus `shouldReturn` 1
      getCharacterStatusCount (VillainCharacter other) ToughStatus `shouldReturn` 0
