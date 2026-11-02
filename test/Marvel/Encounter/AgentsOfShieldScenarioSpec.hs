{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Encounter.AgentsOfShieldScenarioSpec (spec) where

import Data.HashMap.Strict qualified as HM
import Data.Aeson.KeyMap qualified as KM
import Data.List (nub, sort)
import Marvel.AgentsOfShield.Board
import Marvel.AgentsOfShield.EncounterCards qualified as Cards
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Campaign (ShieldLog (..))
import Marvel.Card (toCard)
import Marvel.Environment.Types qualified as Environment
import Marvel.Keyword qualified as K
import Marvel.MainScheme.Types qualified as Main
import Marvel.Matchers qualified as Matchers
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Types qualified as Minion
import Marvel.Modifier
import Marvel.Query
import Marvel.Trait qualified as T
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import TestImport

lifted :: L.QueueT GameAppT () -> GameAppT ()
lifted body = resolve =<< L.runQueueT (L.capture body)

scenarioGame :: CardCode -> Difficulty -> [Text] -> IO TestGame
scenarioGame code mode heroes = do
  let setup = (fromMaybe (error "missing SHIELD setup") $ recommendedSetup code) {difficulty=mode}
  (app,ran,logs) <- dealtGameLogged setup (map starter heroes) 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logs seats
  inGame tg toFirstTurn
  pure tg

isFinished :: GameState -> Bool
isFinished = \case Finished{} -> True; _ -> False

spec :: Spec
spec = describe "Agents of S.H.I.E.L.D. scenarios" do
  for_ [("50064",Normal,"50064"),("50064",Expert,"50065"),("50086a",Normal,"50086a"),("50086a",Expert,"50086b"),("50103a",Normal,"50103a"),("50103a",Expert,"50103b"),("50129a",Normal,"50129a"),("50129a",Expert,"50129b"),("50165a",Normal,"50165a"),("50165a",Expert,"50166a")] $ \(code,mode,expected) ->
    it ("sets up the printed villain face for " <> show code <> " " <> show mode) do
      tg <- scenarioGame code mode ["spider-man"]
      inGame tg do
        villain <- getVillain
        toCardCode (getCardDef villain) `shouldBe` expected

  it "Black Widow setup gives each player a minion and printed starting threat" do
    tg <- scenarioGame "50064" Normal ["spider-man","captain-marvel"]
    inGame tg do
      selectListCount (MinionEngagedWith $ IdentityWithId $ player1 tg) `shouldReturn` 1
      selectListCount (MinionEngagedWith $ IdentityWithId $ player2 tg) `shouldReturn` 1
      mainSchemeThreatNow `shouldReturn` 4

  it "Black Widow's main scheme adds threat equal to her stage" do
    tg <- scenarioGame "50064" Normal ["spider-man"]
    inGame tg do
      before <- mainSchemeThreatNow
      resolve [AccelerateMainScheme]
      mainSchemeThreatNow `shouldReturn` (before+1)

  it "Batroc resets his fixed hit points and removes six threat without advancing the villain" do
    tg <- scenarioGame "50086a" Normal ["spider-man"]
    let p=player1 tg
    inGame tg do
      vid <- villainIdNow
      lifted $ L.dealDamage vid p (toDamage 8 FromAbility)
      getVillainHitPoints vid `shouldReturn` 8
      mainSchemeThreatNow `shouldReturn` 3
      villain <- getVillain
      toCardCode (getCardDef villain) `shouldBe` "50086a"

  it "Batroc releases a captive when the second scheme is cleared and can advance to extraction" do
    tg <- scenarioGame "50086a" Normal ["spider-man"]
    let p=player1 tg
    inGame tg do
      lifted $ L.removeThreat p p 99 MainScheme
      sid <- mainSchemeIdNow
      scheme <- getsGame (getCardDef . fromMaybe (error "missing second stage") . HM.lookup sid . gameMainSchemes)
      toCardCode scheme `shouldBe` "50088"
      lifted $ L.removeThreat p p 99 MainScheme
      selectListCount (AllyWithTitle "Rescued Captive") `shouldReturn` 1
      captive <- selectJust (AllyWithTitle "Rescued Captive")
      allyMatches Matchers.ExhaustedAlly captive `shouldReturn` True

  it "Alert Level flips at the player-scaled threshold, clears its threat and then loses on its High side" do
    tg <- scenarioGame "50086a" Normal ["spider-man"]
    inGame tg do
      env <- selectJust (EnvironmentIs Cards.aos50090a)
      lifted $ L.placeCounters env 4
      selectAny (EnvironmentIs Cards.aos50090b) `shouldReturn` True
      getCountersOn env `shouldReturn` 0
      lifted $ L.placeCounters env 4
      getsGame (isFinished . gameState) `shouldReturn` True

  it "M.O.D.O.K. exposes one Holding Cell and one upgrade in standard, two upgrades in expert" do
    for_ [(Normal,1),(Expert,2)] $ \(mode,n) -> do
      tg <- scenarioGame "50103a" mode ["spider-man"]
      inGame tg do
        selectListCount (EnvironmentWithTitle "Holding Cell") `shouldReturn` 1
        selectListCount (EnvironmentWithTrait T.Adaptoid) `shouldReturn` n
        selectListCount (MinionIs Cards.aos50113) `shouldReturn` 1
        cell <- selectJust (EnvironmentWithTitle "Holding Cell")
        getCountersOn cell `shouldReturn` 2

  it "Defeating M.O.D.O.K. removes two locks and resets his dial while a Holding Cell is in play" do
    tg <- scenarioGame "50103a" Normal ["spider-man"]
    let p=player1 tg
    inGame tg do
      vid <- villainIdNow
      lifted $ L.dealDamage vid p (toDamage 10 FromAbility)
      getVillainHitPoints vid `shouldReturn` 10
      selectListCount (AllyWithTrait T.Rescued) `shouldReturn` 1
      selectListCount (EnvironmentWithTitle "Holding Cell") `shouldReturn` 1
      getsGame (isFinished . gameState) `shouldReturn` False

  it "M.O.D.O.K.'s scheme completion recruits an upgrade and resets threat" do
    tg <- scenarioGame "50103a" Normal ["spider-man"]
    inGame tg do
      sid <- mainSchemeIdNow
      resolve [AdvanceMainScheme sid]
      selectListCount (EnvironmentWithTrait T.Adaptoid) `shouldReturn` 2
      mainSchemeThreatNow `shouldReturn` 0

  it "Thunderbolts uses one elite per player plus one backup, with guard supplied by its main scheme" do
    tg <- scenarioGame "50129a" Normal ["spider-man","captain-marvel"]
    inGame tg do
      selectListCount (MinionWithTrait T.Thunderbolt <> MinionWithTrait T.Elite) `shouldReturn` 3
      selectListCount (MinionWithTrait T.Thunderbolt <> MinionEngagedWith AnyIdentity) `shouldReturn` 2
      backups <- getsGame gameAosBackup
      length backups `shouldBe` 1
      mids <- selectList (MinionWithTrait T.Thunderbolt <> MinionWithTrait T.Elite)
      for_ mids $ \mid -> getEnemyKeywords (EnemyMinionId mid) >>= (`shouldSatisfy` elem K.Guard)

  it "Citizen V cannot be defeated until enough Thunderbolts are in the victory display" do
    tg <- scenarioGame "50129a" Normal ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      getModifiers (VillainRef vid) >>= (`shouldSatisfy` elem CannotBeDefeated)
      card <- encounterCard Cards.aos50139
      resolve [AddCardToVictoryDisplay card]
      getModifiers (VillainRef vid) >>= (`shouldSatisfy` notElem CannotBeDefeated)

  it "Baron Zemo starts with all three Board members on two secret counters and prepared evidence" do
    tg <- scenarioGame "50165a" Normal ["spider-man"]
    inGame tg do
      members <- boardMembers
      length members `shouldBe` 3
      traverse getCountersOn members `shouldReturn` [2,2,2]
      log <- getShieldLog
      length log.hiddenEvidence `shouldBe` 3

  it "a Board member flips at four secrets, keeps its counters, and three hostile members lose the game" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      for_ [Cards.aos50181a,Cards.aos50182a,Cards.aos50183a] $ \def -> do
        card <- encounterCard def
        resolve [PutEncounterCardIntoPlay p card]
        lifted $ L.placeCounters (EnvironmentId $ unCardId $ ecCardId card) 4
      selectListCount (EnvironmentWithTrait T.BoardMember) `shouldReturn` 0
      selectListCount (AttachmentWithTrait T.BoardMember) `shouldReturn` 3
      attachments <- selectList (AttachmentWithTrait T.BoardMember)
      traverse getCountersOn attachments `shouldReturn` [4,4,4]
      getsGame (isFinished . gameState) `shouldReturn` True

  it "freeing all four Inhumans empties the Holding Cell deck and permits the M.O.D.O.K. win" do
    tg <- scenarioGame "50103a" Normal ["spider-man"]
    let p=player1 tg
    inGame tg do
      for_ [1..4 :: Int] $ \_ -> do
        cell <- selectJust (EnvironmentWithTitle "Holding Cell")
        lifted $ L.removeCounters cell 2
      selectListCount (EnvironmentWithTitle "Holding Cell") `shouldReturn` 0
      selectListCount (AllyWithTrait T.Rescued) `shouldReturn` 4
      vid <- villainIdNow
      lifted $ L.dealDamage vid p (toDamage 10 FromAbility)
      getsGame gameState `shouldReturn` Finished Won

  it "a rescued Inhuman returned to hand becomes a cell, with no phantom card left in hand" do
    tg <- scenarioGame "50103a" Normal ["spider-man"]
    let p=player1 tg
    inGame tg do
      for_ [1..4 :: Int] $ \_ -> do
        cell <- selectJust (EnvironmentWithTitle "Holding Cell")
        lifted $ L.removeCounters cell 2
      rescued <- selectJust (AllyWithTrait T.Rescued)
      card <- getsGame (toCard . fromMaybe (error "rescued ally missing") . HM.lookup rescued . gameAllies)
      resolve [ReturnToHand $ AllyRef rescued]
      selectAny (AllyWithId rescued) `shouldReturn` False
      hand <- handOf p
      map pcCardId hand `shouldSatisfy` notElem (toCardId card)
      selectListCount (EnvironmentWithTitle "Holding Cell") `shouldReturn` 1
      cell <- selectJust (EnvironmentWithTitle "Holding Cell")
      getCountersOn cell `shouldReturn` 2

  it "recruiting the last Adaptoid upgrade loses the game" do
    tg <- scenarioGame "50103a" Normal ["spider-man"]
    inGame tg do
      sid <- mainSchemeIdNow
      resolve [AdvanceMainScheme sid]
      resolve [AdvanceMainScheme sid]
      getsGame (isFinished . gameState) `shouldReturn` False
      resolve [AdvanceMainScheme sid]
      selectListCount (EnvironmentWithTrait T.Adaptoid) `shouldReturn` 4
      getsGame gameState `shouldReturn` Finished Lost

  for_ [(Cards.aos50109,1,2,T.Aerial,[K.Incite 1]),(Cards.aos50110,1,1,T.Psionic,[K.Villainous]),(Cards.aos50111,2,1,T.Elite,[K.Overkill,K.Ranged]),(Cards.aos50112,2,1,T.Brute,[K.Toughness])] $ \(def,atk,sch,trait,keywords) ->
    it ("applies every printed modifier from " <> show (toCardCode def) <> " to Adaptoids") do
      tg <- testGame ["spider-man"]
      let p=player1 tg
      inGame tg do
        upgrade <- encounterCard def
        resolve [PutEncounterCardIntoPlay p upgrade]
        mid <- addMinion p Cards.aos50113
        attrs <- getsGame (toAttrs . fromMaybe (error "Adaptoid missing") . HM.lookup mid . gameMinions)
        Minion.getModifiedAttack attrs `shouldReturn` atk
        Minion.getModifiedScheme attrs `shouldReturn` sch
        traits <- getCharacterTraits (MinionCharacter mid)
        traits `shouldSatisfy` member trait
        actual <- getEnemyKeywords (EnemyMinionId mid)
        for_ keywords $ \keyword -> actual `shouldSatisfy` elem keyword

  it "defeating an Adaptoid removes a counter from an environment" do
    tg <- scenarioGame "50103a" Normal ["spider-man"]
    let p=player1 tg
    inGame tg do
      cell <- selectJust (EnvironmentWithTitle "Holding Cell")
      mid <- selectJust (MinionIs Cards.aos50113)
      lifted $ L.dealDamage mid p ((toDamage 5 FromAbility) {damagePiercing=True})
      getCountersOn cell `shouldReturn` 1

  for_ [Cards.aos50114,Cards.aos50115,Cards.aos50116,Cards.aos50118,Cards.aos50119] $ \def ->
    it ("discards " <> show (toCardCode def) <> " after M.O.D.O.K. resets his hit points") do
      tg <- scenarioGame "50103a" Normal ["spider-man"]
      let p=player1 tg
      inGame tg do
        card <- encounterCard def
        resolve [RevealEncounterCard p card]
        vid <- villainIdNow
        lifted $ L.dealDamage vid p (toDamage 100 FromAbility)
        getsGame (HM.member (AttachmentId $ unCardId $ ecCardId card) . gameAttachments) `shouldReturn` False

  it "Psionic Force Field takes damage in place of M.O.D.O.K. and discards at five" do
    tg <- scenarioGame "50103a" Normal ["spider-man"]
    let p=player1 tg
    inGame tg do
      card <- encounterCard Cards.aos50117
      resolve [RevealEncounterCard p card]
      vid <- villainIdNow
      lifted $ L.dealDamage vid p (toDamage 2 FromAbility)
      villainDamageNow `shouldReturn` 0
      getsGame (maybe 0 (Attachment.attachmentDamage . toAttrs) . HM.lookup (AttachmentId $ unCardId $ ecCardId card) . gameAttachments) `shouldReturn` 2
      lifted $ L.dealDamage vid p (toDamage 3 FromAbility)
      selectAny (AttachmentIs Cards.aos50117) `shouldReturn` False
      villainDamageNow `shouldReturn` 0

  it "Hostage Situation keeps the attached ally in play without a controller, then gives it to the defeating player" do
    tg <- scenarioGame "50103a" Normal ["spider-man"]
    let p=player1 tg
    inGame tg do
      cell <- selectJust (EnvironmentWithTitle "Holding Cell")
      lifted $ L.removeCounters cell 2
      ally <- selectJust (AllyWithTrait T.Rescued)
      hostage <- encounterCard Cards.aos50121
      resolve [RevealEncounterCard p hostage]
      selectAny (AllyWithId ally) `shouldReturn` True
      getSourceController (AllyRef ally) `shouldReturn` Nothing
      allyMatches (AllyControlledBy $ IdentityWithId p) ally `shouldReturn` False
      vid <- villainIdNow
      getModifiers (VillainRef vid) >>= (`shouldSatisfy` elem CannotTakeDamage)
      lifted $ L.removeThreat p p 99 (SchemeWithId $ SchemeSideSchemeId $ sideSchemeOf hostage)
      allyMatches (AllyControlledBy $ IdentityWithId p) ally `shouldReturn` True

  it "Thunderbolt Backup swaps the most damaged minion, preserving its state and healing one per player" do
    tg <- scenarioGame "50129a" Normal ["spider-man"]
    let p=player1 tg
    inGame tg do
      [(env,old)] <- getsGame gameAosBackup
      active <- selectJust (MinionWithTrait T.Thunderbolt <> MinionWithTrait T.Elite <> MinionEngagedWith (IdentityWithId p))
      lifted $ L.dealDamage active p ((toDamage 4 FromAbility) {damagePiercing=True})
      resolve [EndRound]
      getsGame gameAosBackup `shouldReturn` [(env,active)]
      minionMatches (MinionEngagedWith $ IdentityWithId p) old `shouldReturn` True
      minionMatches (MinionEngagedWith AnyIdentity) active `shouldReturn` False
      minionDamageOf active `shouldReturn` Just 3

  it "Psionic Blast confuses the alter ego and makes M.O.D.O.K. scheme" do
    tg <- scenarioGame "50103a" Normal ["spider-man"]
    let p=player1 tg
    inGame tg do
      lifted $ L.changeToAlterEgo p
      before <- mainSchemeThreatNow
      card <- encounterCard Cards.aos50124
      resolve [RevealEncounterCard p card]
      isConfused p `shouldReturn` True
      mainSchemeThreatNow >>= (`shouldSatisfy` (>before))

  it "It's Alive reveals an Adaptoid from the discard pile for each player" do
    tg <- scenarioGame "50103a" Normal ["spider-man","captain-marvel"]
    let p=player1 tg; other=player2 tg
    inGame tg do
      before <- selectListCount (MinionEngagedWith $ IdentityWithId p)
      beforeOther <- selectListCount (MinionEngagedWith $ IdentityWithId other)
      _ <- setEncounterDeck [Treacheries.advance,Treacheries.advance] [Cards.aos50113,Cards.aos50113]
      card <- encounterCard Cards.aos50123
      resolve [RevealEncounterCard p card]
      selectListCount (MinionEngagedWith $ IdentityWithId p) `shouldReturn` (before+1)
      selectListCount (MinionEngagedWith $ IdentityWithId other) `shouldReturn` (beforeOther+1)

  it "Chief Medical Officer spends two energy, removes one secret, and then heals a friendly character" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      lifted $ L.changeToHeroForm p >> L.damageIdentity p p (toDamage 2 FromAbility)
      card <- encounterCard Cards.aos50181a
      resolve [PutEncounterCardIntoPlay p card]
      let eid=EnvironmentId $ unCardId $ ecCardId card
      lifted $ L.placeCounters eid 2
      energy <- putInHand p "01088"
      choose "medical officer" (isAbilityOf $ EnvironmentRef eid)
      payWith [energy]
      chooseIf (isTargetChoice p)
      getCountersOn eid `shouldReturn` 1
      damageOf p `shouldReturn` 1

  it "Chief Surveillance Officer spends two mental, removes one secret, and then removes two scheme threat" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      lifted $ L.changeToHeroForm p
      sid <- mainSchemeIdNow
      lifted $ L.placeThreatOn p sid 3
      card <- encounterCard Cards.aos50182a
      resolve [PutEncounterCardIntoPlay p card]
      let eid=EnvironmentId $ unCardId $ ecCardId card
      lifted $ L.placeCounters eid 2
      genius <- putInHand p "01089"
      choose "surveillance officer" (isAbilityOf $ EnvironmentRef eid)
      payWith [genius]
      chooseIf (isTargetChoice sid)
      getCountersOn eid `shouldReturn` 1
      mainSchemeThreatNow `shouldReturn` 1

  it "Chief Tactical Officer spends two physical, removes one secret, and then deals two damage" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      lifted $ L.changeToHeroForm p
      vid <- villainIdNow
      card <- encounterCard Cards.aos50183a
      resolve [PutEncounterCardIntoPlay p card]
      let eid=EnvironmentId $ unCardId $ ecCardId card
      lifted $ L.placeCounters eid 2
      strength <- putInHand p "01090"
      choose "tactical officer" (isAbilityOf $ EnvironmentRef eid)
      payWith [strength]
      chooseIf (isTargetChoice vid)
      getCountersOn eid `shouldReturn` 1
      villainDamageNow `shouldReturn` 2

  for_ [(Cards.aos50182a,2,0),(Cards.aos50183a,0,2)] $ \(def,threat,damage) ->
    it ("resolves hostile Board member " <> show (toCardCode def) <> " only for newly placed secrets") do
      tg <- testGame ["spider-man"]
      let p=player1 tg
      inGame tg do
        card <- encounterCard def
        resolve [PutEncounterCardIntoPlay p card]
        let eid=EnvironmentId $ unCardId $ ecCardId card
        lifted $ L.placeCounters eid 4
        mainSchemeThreatNow `shouldReturn` 0
        damageOf p `shouldReturn` 0
        aid <- selectJust (AttachmentWithTrait T.BoardMember)
        lifted $ L.placeCounters aid 1
        mainSchemeThreatNow `shouldReturn` threat
        damageOf p `shouldReturn` damage
        getCountersOn aid `shouldReturn` 5

  it "Medical Officer's Aid can heal two villain damage after a newly placed secret" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      card <- encounterCard Cards.aos50181a
      resolve [PutEncounterCardIntoPlay p card]
      lifted $ L.placeCounters (EnvironmentId $ unCardId $ ecCardId card) 4
      vid <- villainIdNow
      lifted $ L.dealDamage vid p (toDamage 3 FromAbility)
      aid <- selectJust (AttachmentIs Cards.aos50181b)
      lifted $ L.placeCounters aid 1
      villainDamageNow `shouldReturn` 1
      damageOf p `shouldReturn` 0

  it "Executive Board members flip at three secrets in expert mode" do
    tg <- scenarioGame "50165a" Expert ["spider-man"]
    inGame tg do
      eid <- selectJust (EnvironmentIs Cards.aos50181a)
      lifted $ L.placeCounters eid 1
      selectAny (EnvironmentIs Cards.aos50181a) `shouldReturn` False
      aid <- selectJust (AttachmentIs Cards.aos50181b)
      getCountersOn aid `shouldReturn` 3

  for_ [Cards.aos50184a,Cards.aos50184b,Cards.aos50184c] $ \def ->
    for_ [False,True] $ \asBoost ->
      it ("A.I.M. Interference " <> show (toCardCode def) <> " adds secrets on boost and resolves incite only on reveal, boost=" <> show asBoost) do
        tg <- testGame ["spider-man"]
        let p=player1 tg
        inGame tg do
          for_ [Cards.aos50181a,Cards.aos50182a,Cards.aos50183a] $ \board -> do
            card <- encounterCard board
            resolve [PutEncounterCardIntoPlay p card]
          lifted $ L.changeToHeroForm p
          clearHand p
          card <- encounterCard def
          if asBoost then do
            vid <- villainIdNow
            resolve [RevealBoostCard card (EnemyVillainId vid)]
          else resolve [RevealEncounterCard p card]
          members <- boardMembers
          traverse getCountersOn members `shouldReturn` [1,1,1]
          mainSchemeThreatNow `shouldReturn` (if asBoost then 0 else 1)

  for_ [(Normal,"50065",2),(Expert,"50066",3)] $ \(mode,next,extra) ->
    it ("Black Widow advances to " <> show next <> " and resolves that stage's printed threat") do
      tg <- scenarioGame "50064" mode ["spider-man"]
      let p=player1 tg
      inGame tg do
        vid <- villainIdNow
        before <- mainSchemeThreatNow
        lifted $ L.dealDamage vid p (toDamage 99 FromAbility)
        nextVillain <- getVillain
        toCardCode (getCardDef nextVillain) `shouldBe` next
        mainSchemeThreatNow `shouldReturn` (before+extra)
        villainDamageNow `shouldReturn` 0

  it "Might Makes Right removes no secrets when stun cancels Zemo's attack" do
    tg <- scenarioGame "50165a" Normal ["spider-man"]
    let p=player1 tg
    inGame tg do
      lifted $ L.changeToHeroForm p
      vid <- villainIdNow
      lifted $ L.giveStatus vid StunnedStatus
      before <- boardMembers >>= traverse getCountersOn
      card <- encounterCard Cards.aos50176
      resolve [RevealEncounterCard p card]
      boardMembers >>= traverse getCountersOn >>= (`shouldBe` before)
      getCharacterStatusCount (VillainCharacter vid) StunnedStatus `shouldReturn` 0
      damageOf p `shouldReturn` 0

  it "S.H.I.E.L.D. Agent adds a secret before activating and before being defeated" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      card <- encounterCard Cards.aos50181a
      resolve [PutEncounterCardIntoPlay p card]
      let eid=EnvironmentId $ unCardId $ ecCardId card
      lifted $ L.placeCounters eid 2
      agent <- addMinion p Cards.aos50172
      getCountersOn eid `shouldReturn` 2
      resolve [MinionMessage agent MinionSchemes]
      getCountersOn eid `shouldReturn` 3
      lifted $ L.dealDamage agent p (toDamage 3 FromAbility)
      aid <- selectJust (AttachmentIs Cards.aos50181b)
      getCountersOn aid `shouldReturn` 4
      selectAny (MinionWithId agent) `shouldReturn` False

  it "discarding a vulnerable S.H.I.E.L.D. Agent does not trigger its defeat interrupt" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      card <- encounterCard Cards.aos50181a
      resolve [PutEncounterCardIntoPlay p card]
      let eid=EnvironmentId $ unCardId $ ecCardId card
      lifted $ L.placeCounters eid 2
      agent <- addMinion p Cards.aos50172
      lifted $ L.giveStatus agent ConfusedStatus
      getCountersOn eid `shouldReturn` 2
      selectAny (MinionWithId agent) `shouldReturn` False

  it "Leaping Kick attacks the highest-health ally across players and applies overkill to that ally's controller" do
    tg <- scenarioGame "50086a" Normal ["spider-man","captain-marvel"]
    let p=player1 tg; other=player2 tg
    inGame tg do
      lifted $ L.changeToHeroForm p
      small <- putIntoPlay p "01002"
      large <- putIntoPlay other "01084"
      lifted $ L.exhaust p >> L.exhaust (allyIdOf small) >> L.exhaust (allyIdOf large)
      _ <- setEncounterDeck [Cards.aos50124,Cards.aos50124] []
      card <- encounterCard Cards.aos50096
      resolve [RevealEncounterCard p card]
      selectAny (AllyWithId $ allyIdOf large) `shouldReturn` False
      selectAny (AllyWithId $ allyIdOf small) `shouldReturn` True
      damageOf p `shouldReturn` 0
      damageOf other `shouldReturn` 1

  it "Citizen V's Sword activates him when revealed and can be discarded by spending two physical after an attack damages him" do
    tg <- scenarioGame "50129a" Normal ["spider-man"]
    let p=player1 tg
    inGame tg do
      mids <- selectList AnyMinion
      for_ mids $ \mid -> lifted $ L.removeFromPlay mid
      _ <- setEncounterDeck [Cards.aos50124,Cards.aos50124,Cards.aos50124] []
      before <- mainSchemeThreatNow
      card <- encounterCard Cards.aos50132
      resolve [RevealEncounterCard p card]
      mainSchemeThreatNow >>= (`shouldSatisfy` (>before))
      lifted $ L.changeToHeroForm p
      strength <- putInHand p "01090"
      vid <- villainIdNow
      choose "attack" (isHeroAttack p)
      choose "attack Citizen V" (isTargetChoice vid)
      let attachment=AttachmentId $ unCardId $ ecCardId card
      offers (isAbilityOf $ AttachmentRef attachment) `shouldReturn` True
      choose "discard Citizen V's Sword" (isAbilityOf $ AttachmentRef attachment)
      payWith [strength]
      selectAny (AttachmentIs Cards.aos50132) `shouldReturn` False

  it "A.I.M. Interference spends a double energy resource once to prevent two secrets, without incite on boost" do
    tg <- testGame ["spider-man"]
    let p=player1 tg
    inGame tg do
      lifted $ L.changeToHeroForm p
      for_ [Cards.aos50181a,Cards.aos50182a,Cards.aos50183a] $ \def -> do
        board <- encounterCard def
        resolve [PutEncounterCardIntoPlay p board]
      clearHand p
      energy <- putInHand p "01088"
      vid <- villainIdNow
      card <- encounterCard Cards.aos50184a
      withGame_ $ questionL .~ mempty
      push $ RevealBoostCard card (EnemyVillainId vid)
      runGame
      choose "prevent two secrets" (isLabel $ L.countLabel "aosPreventSecrets" 2)
      payWith [energy]
      choose "prevent first secret" (isLabel "$label.aosPreventSecret")
      choose "prevent second secret" (isLabel "$label.aosPreventSecret")
      counters <- boardMembers >>= traverse getCountersOn
      sort counters `shouldBe` [0,0,1]
      mainSchemeThreatNow `shouldReturn` 0

  it "Extract Captives redirects Leaping Kick's direct attack on a higher-health noncaptive ally" do
    tg <- scenarioGame "50086a" Normal ["spider-man"]
    let p=player1 tg
    inGame tg do
      lifted $ L.removeThreat p p 99 MainScheme
      lifted $ L.removeThreat p p 99 MainScheme
      sid <- mainSchemeIdNow
      scheme <- getsGame (getCardDef . fromMaybe (error "missing extraction stage") . HM.lookup sid . gameMainSchemes)
      toCardCode scheme `shouldBe` "50089"
      captive <- selectJust (AllyWithTitle "Rescued Captive")
      large <- putIntoPlay p "16047"
      lifted $ L.changeToHeroForm p >> L.exhaust p >> L.exhaust (allyIdOf large)
      selectAny (EnvironmentIs Cards.aos50090b) `shouldReturn` True
      getCharacterRemainingHitPoints (AllyCharacter captive) `shouldReturn` 5
      _ <- setEncounterDeck [Treacheries.advance,Treacheries.advance,Treacheries.advance] []
      kick <- encounterCard Cards.aos50096
      resolve [RevealEncounterCard p kick]
      getsGame (maybe 0 allyDamage . fmap toAttrs . HM.lookup (allyIdOf large) . gameAllies) `shouldReturn` 0
      getsGame (maybe 0 allyDamage . fmap toAttrs . HM.lookup captive . gameAllies) `shouldReturn` 3
      selectAny (AllyWithId captive) `shouldReturn` True
      getsGame gameState `shouldReturn` InProgress
      damageOf p `shouldReturn` 0


  for_ [Normal,Expert] $ \mode ->
    it ("keeps exactly four physical Batroc captives outside the encounter piles in " <> show mode) do
      tg <- scenarioGame "50086a" mode ["spider-man"]
      inGame tg do
        piles <- (<>) <$> getEncounterDeck <*> getEncounterDiscard
        filter ((== "50091") . toCardCode) piles `shouldBe` []
        waiting <- filter ((== "50091") . toCardCode) <$> getSetAsidePlayerCards
        active <- getsGame (filter ((== "50091") . toCardCode . getCardDef) . HM.elems . gameAllies)
        victory <- filter ((== "50091") . toCardCode) <$> getPlayerVictoryDisplay
        let ids = map pcCardId waiting <> map (toCardId . toCard) active <> map pcCardId victory
        length ids `shouldBe` 4
        length (nub ids) `shouldBe` 4

  for_ [Normal,Expert] $ \mode ->
    it ("keeps exactly four physical Holding Cell fronts and no Inhuman backs in the encounter piles in " <> show mode) do
      tg <- scenarioGame "50103a" mode ["spider-man"]
      inGame tg do
        piles <- (<>) <$> getEncounterDeck <*> getEncounterDiscard
        let faces = ["50105a","50105b","50106a","50106b","50107a","50107b","50108a","50108b"]
        filter ((`elem` faces) . toCardCode) piles `shouldBe` []
        scenario <- getsGame (toJSON . gameScenario)
        remaining <- case scenario of
          Object o | Just metadata <- KM.lookup "_withMetadata" o, Success cards <- fromJSON @[EncounterCard] metadata -> pure cards
          _ -> error "Holding Cell deck metadata missing"
        active <- getsGame (filter (cardMatch (CardWithTitle "Holding Cell") . getCardDef) . HM.elems . gameEnvironments)
        sort (map toCardCode remaining <> map (toCardCode . getCardDef) active) `shouldBe` ["50105a","50106a","50107a","50108a"]
        let ids = map ecCardId remaining <> map (toCardId . toCard) active
        length ids `shouldBe` 4
        length (nub ids) `shouldBe` 4
