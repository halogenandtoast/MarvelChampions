module Marvel.CivilWar.DeadlyDuoSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (attachmentIdOf,withLeader)
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.EncounterCard (gatherEncounterSet)
import Marvel.EncounterSet qualified as E
import Marvel.Keyword qualified as K
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as M
import Test.Hspec qualified as H
import TestImport

setPlayerDeck :: IdentityId -> [PlayerCard] -> GameAppT ()
setPlayerDeck you cards = withGame_ $ entitiesL . playersL . ix you %~ \(PlayerIdentity a) -> PlayerIdentity a {playerIdentityDeck = Deck cards}

putFree :: IdentityId -> PlayerCard -> GameAppT ()
putFree you card = L.runQueueT (L.capture $ L.putIntoPlay you card) >>= resolve

spec :: Spec
spec = describe "Synthezoid Deadly Duo" do
  it "gathers five physical cards" do
    cards <- gatherEncounterSet E.CwDeadlyDuo
    length cards `H.shouldBe` 5

  for_ [(C.cw57027,True),(C.cw57028,False)] $ \(def,damage) -> do
    for_ [(False,["01088","01089","01090"],1),(False,["01088","01086","01008"],3),(True,["01088","01086","01008"],3)] $ \(hero,codes,count) ->
      it (show (toCardCode def) <> " counts distinct types in either identity form: " <> show (hero,count)) do
        tg <- testGame ["spider-man"]
        inGame tg do
          let you = player1 tg
          when hero $ changeForm you
          cards <- stackDeck you (codes <> ["01002"])
          _ <- addMinion you def
          damageOf you `shouldReturn` if damage then count else 0
          mainSchemeThreat `shouldReturn` if damage then 0 else count
          take 3 <$> discardOf you `shouldReturn` take 3 cards

    it (show (toCardCode def) <> " counts only actual discards from a short deck") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        cards <- stackDeck you ["01088","01086"]
        setPlayerDeck you cards
        _ <- addMinion you def
        damageOf you `shouldReturn` if damage then 2 else 0
        mainSchemeThreat `shouldReturn` if damage then 0 else 2

  it "a repeated Jack reveal discards from the revealing player's deck, not his engaged player's" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let one = player1 tg
          two = player2 tg
      _ <- stackDeck one ["01088","01089","01090","01002"]
      _ <- addMinion one C.cw57028
      before <- mainSchemeThreat
      ownDeck <- unDeck . playerIdentityDeck <$> playerAttrs one
      cards <- stackDeck two ["01088","01086","01008","01002"]
      repeatCard <- encounterCard C.cw57058
      resolve [RevealEncounterCard two repeatCard]
      mainSchemeThreat `shouldReturn` (before + 3)
      unDeck . playerIdentityDeck <$> playerAttrs one `shouldReturn` ownDeck
      take 3 <$> discardOf two `shouldReturn` take 3 cards

  it "Platform gives the opposing team the choice and grants four HP and stalwart" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- addSecondBoard tg
      first <- addMinion you M.hydraSoldier
      second <- addMinion you M.hydraMercenary
      before <- getEnemyRemainingHitPoints (EnemyMinionId second)
      card <- encounterCard C.cw57029
      start [RevealEncounterCard you card]
      fmap fst <$> openQuestion `shouldReturn` Just (player2 tg)
      choose "attach to the mercenary" (isTargetChoice $ EnemyMinionId second)
      pass
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (MinionCharacter second)
      getEnemyRemainingHitPoints (EnemyMinionId second) `shouldReturn` (before + 4)
      getEnemyKeywords (EnemyMinionId second) >>= (`shouldSatisfy` elem K.Stalwart)
      getEnemyKeywords (EnemyMinionId first) >>= (`shouldSatisfy` notElem K.Stalwart)

  it "Platform surges without a minion" do
    tg <- testGame ["spider-man"]
    inGame tg do
      [next] <- stackEncounterDeck [M.hydraMercenary]
      card <- encounterCard C.cw57029
      resolve [RevealEncounterCard (player1 tg) card]
      dealtTo (player1 tg) `shouldReturn` [next]
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Nothing

  it "Platform's boost attaches to the player's engaged minion" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      own <- withLeader "57040"
      mid <- addMinion you M.hydraMercenary
      [card,_] <- stackEncounterDeck [C.cw57029,M.hydraSoldier]
      start [VillainMessage own VillainSchemes]
      choose "attach Platform" (isTargetChoice $ EnemyMinionId mid)
      pass
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (MinionCharacter mid)
      getEnemyKeywords (EnemyMinionId mid) >>= (`shouldSatisfy` elem K.Stalwart)
      getEncounterDiscard >>= (`shouldSatisfy` notElem card)

  it "Yo-Yo discards the highest-cost card, including allies" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      hulk <- putInHand you "01050"
      shooter <- putInHand you "01008"
      putFree you hulk
      putFree you shooter
      card <- encounterCard C.cw57030
      resolve [RevealEncounterCard you card]
      getsGame (HashMap.member (allyIdOf hulk) . gameAllies) `shouldReturn` False
      getsGame (HashMap.member (upgradeIdOf shooter) . gameUpgrades) `shouldReturn` True

  it "Yo-Yo lets the player break a highest-cost tie" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      first <- putInHand you "01008"
      second <- putInHand you "01008"
      putFree you first
      putFree you second
      card <- encounterCard C.cw57030
      start [RevealEncounterCard you card]
      choose "discard second shooter" (isTargetChoice $ upgradeIdOf second)
      pass
      getsGame (HashMap.member (upgradeIdOf first) . gameUpgrades) `shouldReturn` True
      getsGame (HashMap.member (upgradeIdOf second) . gameUpgrades) `shouldReturn` False

  it "Yo-Yo surges without a controlled card" do
    tg <- testGame ["spider-man"]
    inGame tg do
      [next] <- stackEncounterDeck [M.hydraMercenary]
      card <- encounterCard C.cw57030
      resolve [RevealEncounterCard (player1 tg) card]
      dealtTo (player1 tg) `shouldReturn` [next]

  it "Yo-Yo's boost discards an upgrade rather than a more expensive ally" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      own <- withLeader "57040"
      hulk <- putInHand you "01050"
      shooter <- putInHand you "01008"
      putFree you hulk
      putFree you shooter
      _ <- stackEncounterDeck [C.cw57030,M.hydraSoldier]
      resolve [VillainMessage own VillainSchemes]
      getsGame (HashMap.member (allyIdOf hulk) . gameAllies) `shouldReturn` True
      getsGame (HashMap.member (upgradeIdOf shooter) . gameUpgrades) `shouldReturn` False

  it "Deadly Duo discards eight cards from each player on its board, even when an opponent defeats it" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
          other = player2 tg
      _ <- addSecondBoard tg
      cards <- stackDeck you (replicate 9 "01088")
      otherDeck <- unDeck . playerIdentityDeck <$> playerAttrs other
      card <- encounterCard C.cw57031
      resolve [RevealEncounterCard you card]
      let sid = SideSchemeId $ unCardId $ ecCardId card
      sideSchemeThreat sid `shouldReturn` 4
      L.runQueueT (L.capture $ L.removeSchemeThreat other (SchemeSideSchemeId sid) 4) >>= resolve
      take 8 <$> discardOf you `shouldReturn` take 8 cards
      headMay . unDeck . playerIdentityDeck <$> playerAttrs you `shouldReturn` Just (cards !! 8)
      unDeck . playerIdentityDeck <$> playerAttrs other `shouldReturn` otherDeck

  it "Deadly Duo discards eight cards for every teammate" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let one = player1 tg
          two = player2 tg
      first <- stackDeck one (replicate 9 "01088")
      second <- stackDeck two (replicate 9 "01089")
      card <- encounterCard C.cw57031
      resolve [RevealEncounterCard one card]
      let sid = SideSchemeId $ unCardId $ ecCardId card
      sideSchemeThreat sid `shouldReturn` 5
      L.runQueueT (L.capture $ L.removeSchemeThreat one (SchemeSideSchemeId sid) 5) >>= resolve
      take 8 <$> discardOf one `shouldReturn` take 8 first
      take 8 <$> discardOf two `shouldReturn` take 8 second

  it "Yo-Yo can discard a controlled player side scheme without defeating it" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      scheme <- putInHand you "40059"
      putFree you scheme
      card <- encounterCard C.cw57030
      resolve [RevealEncounterCard you card]
      let sid = SideSchemeId $ unCardId $ pcCardId scheme
      getsGame (HashMap.member sid . gameSideSchemes) `shouldReturn` False
      discardOf you >>= (`shouldSatisfy` elem scheme)
