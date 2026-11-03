module Marvel.CivilWar.MoonKnightSpec (spec) where

import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (attachmentIdOf,withLeader)
import Marvel.EncounterCard (gatherEncounterSet)
import Marvel.EncounterSet qualified as E
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Message.Lifted qualified as L
import Marvel.Treachery.Cards qualified as T
import Marvel.Minion.Cards qualified as M
import Test.Hspec qualified as H
import TestImport

spec :: Spec
spec = describe "Synthezoid Moon Knight" do
  it "gathers five physical cards including two Crescent Darts" do
    cards <- gatherEncounterSet E.CwMoonKnightModular
    length cards `H.shouldBe` 5
    length (filter ((== "57066") . toCardCode) cards) `H.shouldBe` 2

  it "Moon Knight enters tough with retaliate two and has no effect without a treachery discard" do
    tg <- testGame ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) C.cw57065
      getCharacterStatusCount (MinionCharacter mid) ToughStatus `shouldReturn` 1
      getEnemyKeywords (EnemyMinionId mid) >>= (`shouldSatisfy` elem (K.Retaliate 2))
      damageOf (player1 tg) `shouldReturn` 0

  it "Moon Knight resolves the topmost treachery without discarding it a second time" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      old <- encounterCard C.cw57067
      newer <- encounterCard C.cw57014
      resolve [DiscardedCard $ EncounterCard old,DiscardedCard $ EncounterCard newer]
      own <- villainIdNow
      _ <- addMinion you C.cw57065
      damageOf you `shouldReturn` 0
      getCharacterStatusCount (VillainCharacter own) ToughStatus `shouldReturn` 1
      discard <- getEncounterDiscard
      length (filter (== newer) discard) `shouldBe` 1
      length (filter (== old) discard) `shouldBe` 1

  it "a repeated treachery retains its gained surge" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      repeatCard <- encounterCard C.cw57030
      clearHand you
      resolve [DiscardedCard $ EncounterCard repeatCard]
      [next] <- stackEncounterDeck [M.hydraMercenary]
      _ <- addMinion you C.cw57065
      dealtTo you `shouldReturn` [next]

  for_ [False,True] $ \boost ->
    it ("Fist of Khonshu exhausts the identity, dealing damage only when revealed: boost " <> show boost) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        if boost then do
          own <- withLeader "57040"
          _ <- stackEncounterDeck [C.cw57067,M.hydraMercenary]
          resolve [VillainMessage own VillainSchemes]
        else do
          card <- encounterCard C.cw57067
          resolve [RevealEncounterCard you card]
        playerIdentityExhausted <$> playerAttrs you `shouldReturn` True
        damageOf you `shouldReturn` if boost then 0 else 2

  for_ [False,True] $ \present ->
    it ("Crescent Dart prefers Moon Knight then falls back to the leader: present " <> show present) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        own <- villainIdNow
        host <- if present then MinionCharacter <$> addMinion you C.cw57065 else pure $ VillainCharacter own
        card <- encounterCard C.cw57066
        resolve [RevealEncounterCard you card]
        getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just host

  it "Crescent Dart makes one leader attack piercing and then discards" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      own <- withLeader "57040"
      card <- encounterCard C.cw57066
      resolve [RevealEncounterCard you card,GiveStatus (IdentityRef you) ToughStatus]
      changeForm you
      L.runQueueT (L.capture $ L.lastingModifiers GameRef (SourceEntity $ IdentityRef you) [KeywordModifier $ K.Retaliate 1] False) >>= resolve
      before <- getCharacterRemainingHitPoints (VillainCharacter own)
      _ <- stackEncounterDeck [M.hydraMercenary,M.hydraMercenary]
      resolve [VillainMessage own $ VillainAttacks you]
      getCharacterRemainingHitPoints (VillainCharacter own) `shouldReturn` before
      damageOf you `shouldReturn` 2
      getCharacterStatusCount (IdentityCharacter you) ToughStatus `shouldReturn` 0
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing
      getEncounterDiscard >>= (`shouldSatisfy` elem card)

  it "Khonshu's Avatar discards through a treachery and resolves only its reveal ability" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      [minion,treachery,next] <- stackEncounterDeck [M.hydraSoldier,C.cw57067,M.hydraMercenary]
      card <- encounterCard C.cw57068
      resolve [RevealEncounterCard you card]
      let sid = SideSchemeId $ unCardId $ ecCardId card
      sideSchemeThreat sid `shouldReturn` 4
      L.runQueueT (L.capture $ L.removeSchemeThreat you (SchemeSideSchemeId sid) 4) >>= resolve
      damageOf you `shouldReturn` 2
      playerIdentityExhausted <$> playerAttrs you `shouldReturn` True
      headMay <$> getEncounterDeck `shouldReturn` Just next
      discard <- getEncounterDiscard
      discard `shouldSatisfy` elem minion
      length (filter (== treachery) discard) `shouldBe` 1

  it "a repeated printed Surge resolves once without a second reveal" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      card <- encounterCard T.exhaustion
      resolve [DiscardedCard $ EncounterCard card]
      [next] <- stackEncounterDeck [M.hydraMercenary]
      _ <- addMinion you C.cw57065
      dealtTo you `shouldReturn` [next]
      playerIdentityExhausted <$> playerAttrs you `shouldReturn` True
      discard <- getEncounterDiscard
      length (filter (== card) discard) `shouldBe` 1
