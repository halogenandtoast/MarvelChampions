module Marvel.CivilWar.ResistanceAvengersSpec (spec) where

import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (withLeader)
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.Damage
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as M
import Marvel.Query (selectAny)
import Marvel.Modifier
import Marvel.Villain.Types qualified as V
import TestImport hiding (ExhaustedIdentity)

boostsOf :: VillainId -> GameAppT [EncounterCard]
boostsOf vid = getsGame (maybe [] (V.villainBoostCards . toAttrs) . lookup vid . gameVillains)

spec :: Spec
spec = describe "Civil War resistance Avengers" do
  for_ [C.cw56150,C.cw56151,C.cw56152,C.cw56155,C.cw56156] $ \def ->
    it (show (toCardCode def) <> " becomes the enemy leader's physical boost card when defeated") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        vid <- withLeader "57040"
        mid <- addMinion you def
        let cid = CardId $ unMinionId mid
        L.runQueueT (L.capture $ L.dealDamage mid you $ toDamage 10 FromAbility) >>= resolve
        minionDamageOf mid `shouldReturn` Nothing
        map ecCardId <$> boostsOf vid `shouldReturn` [cid]
        fmap (any ((== cid) . ecCardId)) getEncounterDiscard `shouldReturn` False
        getRemovedFromGameEncounterCards >>= (`shouldSatisfy` all ((/= cid) . ecCardId))
        _ <- stackEncounterDeck [M.hydraMercenary,M.hydraSoldier]
        resolve [VillainMessage vid VillainSchemes]
        fmap (length . filter ((== cid) . ecCardId)) getEncounterDiscard `shouldReturn` 1

  for_ [(C.cw56150,ConfusedStatus),(C.cw56151,StunnedStatus)] $ \(def,status) ->
    for_ [False,True] $ \already ->
      it (show (toCardCode def) <> " gives its status and adds two icons only when already present: " <> show already) do
        tg <- testGame ["spider-man"]
        inGame tg do
          let you = player1 tg
          vid <- withLeader "57040"
          when already $ resolve [GiveStatus (IdentityRef you) status]
          _ <- stackEncounterDeck [def,M.hydraMercenary]
          resolve [VillainMessage vid VillainSchemes]
          getCharacterStatusCount (IdentityCharacter you) status `shouldReturn` 1
          mainSchemeThreat `shouldReturn` if already then 3 else 1

  for_ [False,True] $ \already ->
    it ("Goliath gives the enemy leader tough and adds two icons only when already tough: " <> show already) do
      tg <- testGame ["spider-man"]
      inGame tg do
        vid <- withLeader "57040"
        when already $ resolve [GiveStatus (VillainRef vid) ToughStatus]
        _ <- stackEncounterDeck [C.cw56152,M.hydraMercenary]
        resolve [VillainMessage vid VillainSchemes]
        getCharacterStatusCount (VillainCharacter vid) ToughStatus `shouldReturn` 1
        mainSchemeThreat `shouldReturn` if already then 3 else 1

  it "Black Panther's boost exhausts only the resolving player's upgrades" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
          other = player2 tg
      vid <- withLeader "57040"
      own <- putInHand you "01057"
      otherUpgrade <- putInHand other "01065"
      L.runQueueT (L.capture $ L.putIntoPlay you own >> L.putIntoPlay other otherUpgrade) >>= resolve
      _ <- stackEncounterDeck [C.cw56155,M.hydraMercenary]
      resolve [VillainMessage vid VillainSchemes]
      upgradeIsExhausted (upgradeIdOf own) `shouldReturn` True
      upgradeIsExhausted (upgradeIdOf otherUpgrade) `shouldReturn` False

  it "Spectrum's boost exhausts a controlled character" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      _ <- stackEncounterDeck [C.cw56156,M.hydraMercenary]
      resolve [VillainMessage vid VillainSchemes]
      selectAny (IdentityWithId you <> ExhaustedIdentity) `shouldReturn` True

  it "Spectrum's boost finishes when no controlled character can be exhausted" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      L.runQueueT (L.capture $ L.exhaust you) >>= resolve
      _ <- stackEncounterDeck [C.cw56156,M.hydraMercenary]
      resolve [VillainMessage vid VillainSchemes]
      mainSchemeThreat `shouldReturn` 1

  it "New Avengers shuffles discarded minions into the deck, keeping other discarded cards" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      minion <- encounterCard M.hydraSoldier
      treachery <- encounterCard C.cw56163
      _ <- stackEncounterDeck [M.hydraMercenary,M.shocker]
      resolve [DiscardedCard $ EncounterCard minion,DiscardedCard $ EncounterCard treachery]
      side <- encounterCard C.cw56154
      resolve [RevealEncounterCard you side]
      let sid = SideSchemeId $ unCardId $ ecCardId side
      sideSchemeThreat sid `shouldReturn` 5
      L.runQueueT (L.capture $ L.removeSchemeThreat you (SchemeSideSchemeId sid) 5) >>= resolve
      getEncounterDeck >>= (`shouldSatisfy` elem minion)
      getEncounterDiscard >>= (`shouldSatisfy` notElem minion)
      getEncounterDiscard >>= (`shouldSatisfy` elem treachery)

  it "Secret Avengers gives a selected top-five card to the leader as a physical boost" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      cards <- stackEncounterDeck [M.hydraSoldier,M.hydraMercenary]
      side <- encounterCard C.cw56159
      resolve [RevealEncounterCard you side]
      let sid = SideSchemeId $ unCardId $ ecCardId side
      sideSchemeThreat sid `shouldReturn` 4
      L.runQueueT (L.capture $ L.removeSchemeThreat you (SchemeSideSchemeId sid) 4) >>= resolve
      boosts <- boostsOf vid
      length boosts `shouldBe` 1
      let chosen = testHead boosts
      chosen `shouldSatisfy` (`elem` cards)
      getEncounterDeck >>= (`shouldSatisfy` notElem chosen)
      getRemovedFromGameEncounterCards >>= (`shouldSatisfy` notElem chosen)

  it "Teamwork activates a newly entering Avenger after another Avenger is already in play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- addMinion you C.cw56150
      _ <- addMinion you C.cw56151
      mainSchemeThreat `shouldReturn` 1

  it "Teamwork does not count an Avenger on the opposing board" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      _ <- addSecondBoard tg
      _ <- addMinion (player2 tg) C.cw56150
      _ <- addMinion (player1 tg) C.cw56151
      mainSchemeThreat `shouldReturn` 0

  it "a minion defeated by an opposing player's effect still boosts the leader on its own board" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      own <- withLeader "57040"
      other <- addSecondBoard tg
      mid <- addMinion (player2 tg) C.cw56150
      L.runQueueT (L.capture $ L.dealDamage mid you $ toDamage 10 FromAbility) >>= resolve
      boostsOf own `shouldReturn` []
      map ecCardId <$> boostsOf other `shouldReturn` [CardId $ unMinionId mid]

  it "Secret Avengers searches its own board's deck even when the opposing player defeats it" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
          otherPlayer = player2 tg
      own <- withLeader "57040"
      other <- addSecondBoard tg
      ownCards <- traverse encounterCard [M.hydraSoldier,M.hydraMercenary]
      otherCards <- traverse encounterCard [M.shocker,M.hydraSoldier]
      L.runQueueT (L.capture $ do
        L.createDeck (L.VillainDeckKey own) ownCards
        L.createDeck (L.VillainDeckKey other) otherCards) >>= resolve
      side <- encounterCard C.cw56159
      resolve [RevealEncounterCard you side]
      let sid = SideSchemeId $ unCardId $ ecCardId side
      L.runQueueT (L.capture $ L.removeSchemeThreat otherPlayer (SchemeSideSchemeId sid) 4) >>= resolve
      chosen <- boostsOf own
      length chosen `shouldBe` 1
      testHead chosen `shouldSatisfy` (`elem` ownCards)
      asYou otherPlayer getEncounterDeck `shouldReturn` otherCards
      boostsOf other `shouldReturn` []

  it "the defeated Avenger's attachment and stored boost discard instead of following the host" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      mid <- addMinion you C.cw56150
      attachment <- encounterCard C.cw56191
      stored <- encounterCard M.hydraSoldier
      resolve [RevealEncounterCard you attachment]
      L.runQueueT (L.capture $ L.giveCardAsBoost stored mid) >>= resolve
      L.runQueueT (L.capture $ L.dealDamage mid you $ toDamage 10 FromAbility) >>= resolve
      map ecCardId <$> boostsOf vid `shouldReturn` [CardId $ unMinionId mid]
      getEncounterDiscard >>= (`shouldSatisfy` elem attachment)
      getEncounterDiscard >>= (`shouldSatisfy` elem stored)

  it "blanking suppresses the Avenger's printed When Defeated destination" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      mid <- addMinion you C.cw56150
      L.runQueueT (L.capture $ L.lastingModifiers GameRef (SourceEntity $ MinionRef mid) [Blank] False) >>= resolve
      L.runQueueT (L.capture $ L.dealDamage mid you $ toDamage 10 FromAbility) >>= resolve
      boostsOf vid `shouldReturn` []
      fmap (any ((== CardId (unMinionId mid)) . ecCardId)) getEncounterDiscard `shouldReturn` True

  it "Dangerous Recruits and the printed defeat effect move the Avenger only once and clear their markers" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      side <- encounterCard C.cw56091
      resolve [RevealEncounterCard you side]
      mid <- addMinion you C.cw56150
      L.runQueueT (L.capture $ L.dealDamage mid you $ toDamage 10 FromAbility) >>= resolve
      map ecCardId <$> boostsOf vid `shouldReturn` [CardId $ unMinionId mid]
      getsGame gameDefeatedMinionBoosts `shouldReturn` []
