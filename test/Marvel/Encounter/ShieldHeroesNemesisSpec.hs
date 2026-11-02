{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Encounter.ShieldHeroesNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HM
import Marvel.AgentsOfShield.PlayerHelpers qualified as Fury
import Marvel.Environment.Cards qualified as Environments
import Marvel.Keyword qualified as K
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Phase
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Trait qualified as T
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Window qualified as W
import TestImport

lifted :: L.QueueT GameAppT () -> GameAppT ()
lifted body = resolve =<< L.runQueueT (L.capture body)

spec :: Spec
spec = describe "S.H.I.E.L.D. heroes nemeses" do
  it "Press Conference stays in play and removes counters only from its owner's supports at player-phase end" do
    tg <- testGame ["maria-hill","spider-man"]
    let p = player1 tg; other = player2 tg
    inGame tg do
      own <- putIntoPlay p "01092"
      theirs <- putIntoPlay other "01092"
      lifted $ L.placeCounters (supportIdOf own) 2 >> L.placeCounters (supportIdOf theirs) 2
      card <- encounterCard Obligations.aosP50029
      resolve [RevealEncounterCard p card]
      getsGame (HM.member (ObligationId $ unCardId $ ecCardId card) . gameObligations) `shouldReturn` True
      resolve [CheckWindows [W.Window W.After $ W.PhaseEnded PlayerPhase]]
      getCountersOn (SupportRef $ supportIdOf own) `shouldReturn` 1
      getCountersOn (SupportRef $ supportIdOf theirs) `shouldReturn` 2

  it "Controller removes a support counter after scheming and creates a Controlled minion only when its environment is present" do
    tg <- testGame ["maria-hill"]
    let p = player1 tg
    inGame tg do
      support <- putIntoPlay p "01092"
      lifted $ L.placeCounters (supportIdOf support) 2
      controller <- addMinion p Minions.aosP50030
      resolve [MinionMessage controller MinionSchemes]
      getCountersOn (SupportRef $ supportIdOf support) `shouldReturn` 1
      selectListCount (MinionWithTrait T.Controlled) `shouldReturn` 0
      env <- encounterCard Environments.aosP50032
      resolve [PutEncounterCardIntoPlay p env]
      resolve [MinionMessage controller MinionSchemes]
      selectListCount (MinionWithTrait T.Controlled) `shouldReturn` 1
      mids <- selectList (MinionWithTrait T.Controlled)
      attrs <- getsGame (toAttrs . fromMaybe (error "controlled minion missing") . HM.lookup (testHead mids) . gameMinions)
      minionFacedownCard attrs `shouldSatisfy` isJust

  it "Controlled Innocents adds threat after a Controlled minion is defeated, returning the facedown card to its owner" do
    tg <- testGame ["maria-hill"]
    let p = player1 tg
    inGame tg do
      env <- encounterCard Environments.aosP50032
      resolve [PutEncounterCardIntoPlay p env]
      [under] <- stackDeck p ["01090"]
      resolve [CardSpecific GameRef "aosControlledMinion" $ toJSON p]
      mid <- selectJust (MinionWithTrait T.Controlled)
      before <- mainSchemeThreatNow
      lifted $ L.dealDamage mid p (toDamage 1 FromAbility)
      mainSchemeThreatNow `shouldReturn` (before+1)
      discards <- discardOf p
      map pcCardId discards `shouldSatisfy` elem (pcCardId under)

  it "Army of the Controlled finds the environment and discards controlled minions without defeating them" do
    tg <- testGame ["maria-hill"]
    let p = player1 tg
    inGame tg do
      env <- encounterCard Environments.aosP50032
      resolve [SetAside [EncounterCard env]]
      card <- encounterCard Schemes.aosP50031
      resolve [RevealEncounterCard p card]
      selectAny (EnvironmentIs Environments.aosP50032) `shouldReturn` True
      support <- putIntoPlay p "01092"
      resolve [CardSpecific GameRef "aosControlledMinion" $ toJSON p]
      before <- mainSchemeThreatNow
      lifted $ L.removeThreat p p 99 (SchemeWithId $ SchemeSideSchemeId $ sideSchemeOf card)
      selectListCount (MinionWithTrait T.Controlled) `shouldReturn` 0
      mainSchemeThreatNow `shouldReturn` before
      getCountersOn (SupportRef $ supportIdOf support) `shouldReturn` 1

  it "Diabolical Discs removes a support counter and is harmless to minion count without Controlled Innocents" do
    tg <- testGame ["maria-hill"]
    let p = player1 tg
    inGame tg do
      support <- putIntoPlay p "01092"
      lifted $ L.placeCounters (supportIdOf support) 2
      card <- encounterCard Treacheries.aosP50033
      resolve [RevealEncounterCard p card]
      getCountersOn (SupportRef $ supportIdOf support) `shouldReturn` 1
      selectListCount (MinionWithTrait T.Controlled) `shouldReturn` 0

  it "Discovered changes to Assault and surges without suit threat" do
    tg <- testGame ["nick-fury"]
    let p = player1 tg
    inGame tg do
      lifted $ Fury.changeSuit p True
      card <- encounterCard Obligations.aosP50059
      resolve [RevealEncounterCard p card]
      Fury.isStealth p `shouldReturn` False
      Fury.suitThreat p `shouldReturn` 0
      getsGame (HM.member (ObligationId $ unCardId $ ecCardId card) . gameObligations) `shouldReturn` False

  it "Discovered offers both printed choices and can remove all suit threat" do
    tg <- testGame ["nick-fury"]
    let p = player1 tg
    inGame tg do
      lifted $ Fury.placeSuitThreat p 3
      _ <- revealNow p Obligations.aosP50059
      pressWhenRevealed
      offers (isLabel "$label.aosTakeSuitDamage") `shouldReturn` True
      offers (isLabel "$label.aosRemoveSuitThreat") `shouldReturn` True
      choose "remove threat" (isLabel "$label.aosRemoveSuitThreat")
      Fury.suitThreat p `shouldReturn` 0
      damageOf p `shouldReturn` 0

  it "Orion can keep multiple tough cards but gains a new one only after actually taking damage" do
    tg <- testGame ["nick-fury"]
    let p = player1 tg
    inGame tg do
      orion <- addMinion p Minions.aosP50060
      getCharacterStatusCount (MinionCharacter orion) ToughStatus `shouldReturn` 1
      lifted $ L.giveStatus orion ToughStatus
      getCharacterStatusCount (MinionCharacter orion) ToughStatus `shouldReturn` 2
      lifted $ L.dealDamage orion p (toDamage 1 FromAbility)
      getCharacterStatusCount (MinionCharacter orion) ToughStatus `shouldReturn` 1
      lifted $ L.dealDamage orion p ((toDamage 1 FromAbility) {damagePiercing=True})
      minionDamageOf orion `shouldReturn` Just 1
      getCharacterStatusCount (MinionCharacter orion) ToughStatus `shouldReturn` 1

  it "Acquire Infinity Formula finds Orion when Nick takes damage and adds tough when he is already in play" do
    tg <- testGame ["nick-fury"]
    let p = player1 tg
    inGame tg do
      orion <- encounterCard Minions.aosP50060
      resolve [SetAside [EncounterCard orion]]
      scheme <- encounterCard Schemes.aosP50061
      resolve [RevealEncounterCard p scheme]
      lifted $ L.damageIdentity p p (toDamage 1 FromAbility)
      mid <- selectJust (MinionIs Minions.aosP50060)
      getCharacterStatusCount (MinionCharacter mid) ToughStatus `shouldReturn` 1
      lifted $ L.damageIdentity p p (toDamage 1 FromAbility)
      getCharacterStatusCount (MinionCharacter mid) ToughStatus `shouldReturn` 2

  it "Leviathan Soldier damages the engaged identity after a scheme and starts tough" do
    tg <- testGame ["nick-fury"]
    let p = player1 tg
    inGame tg do
      soldier <- addMinion p Minions.aosP50062
      getCharacterStatusCount (MinionCharacter soldier) ToughStatus `shouldReturn` 1
      resolve [MinionMessage soldier MinionSchemes]
      damageOf p `shouldReturn` 1

  it "Cold Storage puts a Leviathan into play from the discard pile without revealing it" do
    tg <- testGame ["nick-fury"]
    let p = player1 tg
    inGame tg do
      _ <- setEncounterDeck [Treacheries.advance,Treacheries.advance] [Minions.aosP50062]
      card <- encounterCard Treacheries.aosP50063
      resolve [RevealEncounterCard p card]
      selectAny (MinionIs Minions.aosP50062) `shouldReturn` True
      damageOf p `shouldReturn` 0

  it "Cold Storage deals two damage if no Leviathan is found; its boost deals one" do
    tg <- testGame ["nick-fury"]
    let p = player1 tg
    inGame tg do
      _ <- setEncounterDeck [Treacheries.advance,Treacheries.advance] []
      card <- encounterCard Treacheries.aosP50063
      resolve [RevealEncounterCard p card]
      damageOf p `shouldReturn` 2
      boost <- encounterCard Treacheries.aosP50063
      vid <- villainIdNow
      resolve [RevealBoostCard boost (EnemyVillainId vid)]
      damageOf p `shouldReturn` 3

  it "Press Conference can be discarded by exhausting Maria Hill in alter-ego form" do
    tg <- testGame ["maria-hill"]
    let p=player1 tg
    inGame tg do
      card <- encounterCard Obligations.aosP50029
      resolve [RevealEncounterCard p card]
      let oid=ObligationId $ unCardId $ ecCardId card
      lifted $ L.changeToHeroForm p
      offers (isAbilityOf $ ObligationRef oid) `shouldReturn` False
      lifted $ L.changeToAlterEgo p
      offers (isAbilityOf $ ObligationRef oid) `shouldReturn` True
      choose "discard Press Conference" (isAbilityOf $ ObligationRef oid)
      isExhaustedNow p `shouldReturn` True
      getsGame (HM.member oid . gameObligations) `shouldReturn` False

  it "Diabolical Discs creates a facedown Controlled minion when its environment is present" do
    tg <- testGame ["maria-hill"]
    let p=player1 tg
    inGame tg do
      env <- encounterCard Environments.aosP50032
      resolve [PutEncounterCardIntoPlay p env]
      [under] <- stackDeck p ["01090"]
      card <- encounterCard Treacheries.aosP50033
      resolve [RevealEncounterCard p card]
      mid <- selectJust (MinionWithTrait T.Controlled)
      getEnemyPrintedScheme (EnemyMinionId mid) `shouldReturn` 1
      getEnemyPrintedAttack (EnemyMinionId mid) `shouldReturn` 1
      getEnemyPrintedHitPoints (EnemyMinionId mid) `shouldReturn` 1
      attrs <- getsGame (toAttrs . fromMaybe (error "controlled minion missing") . HM.lookup mid . gameMinions)
      fmap pcCardId (minionFacedownCard attrs) `shouldBe` Just (pcCardId under)

  it "Discovered's damage choice keeps suit threat and deals that much damage" do
    tg <- testGame ["nick-fury"]
    let p=player1 tg
    inGame tg do
      lifted $ Fury.placeSuitThreat p 3
      _ <- revealNow p Obligations.aosP50059
      pressWhenRevealed
      choose "take suit damage" (isLabel "$label.aosTakeSuitDamage")
      damageOf p `shouldReturn` 3
      Fury.suitThreat p `shouldReturn` 3
