module Marvel.Encounter.ShadowlandEncountersSpec (spec) where

import Marvel.FearNoEvil.Cards qualified as FNE
import Marvel.Matchers
import Marvel.Matchers qualified as Matcher
import Marvel.Keyword (Keyword(Guard,Patrol))
import Marvel.Shadowland.Position qualified as Position
import Marvel.Trait qualified as Trait
import Marvel.Message.Lifted qualified as L
import Marvel.Query
import Marvel.Shadowland.Cards qualified as Cards
import TestImport

positions :: IdentityId -> CardDef -> GameAppT ()
positions you villain = for_ [villain,Cards.street65004a] $ \def -> do
  card <- encounterCard def
  resolve [PutEncounterCardIntoPlay you card]

spec :: Spec
spec = describe "Shadowland general encounter behaviors" do
  for_ [(Cards.street65003a,True),(Cards.street65003b,False)] $ \(position,startsOnStreets) -> do
    it ("Black Tarantula resolves its destination effect from " <> show (toCardCode position)) do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        let you = player1 tg
        changeForm you
        clearHand you
        positions you position
        mid <- addMinion you Cards.street65005
        vid <- villainIdNow
        damageOf you `shouldReturn` (if startsOnStreets then 2 else 0)
        getCharacterStatusCount (MinionCharacter mid) ToughStatus `shouldReturn` (if startsOnStreets then 0 else 1)
        getCharacterStatusCount (VillainCharacter vid) ToughStatus `shouldReturn` (if startsOnStreets then 0 else 1)

    it ("Chaos in Hell's Kitchen resolves after moving from " <> show (toCardCode position)) do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        let you = player1 tg
        positions you position
        card <- encounterCard Cards.street65010
        resolve [RevealEncounterCard you card]
        mainSchemeThreatNow `shouldReturn` (if startsOnStreets then 2 else 0)
        damageOf you `shouldReturn` (if startsOnStreets then 0 else 2)

    it ("Surround the Fortress adds threat at its destination from " <> show (toCardCode position)) do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        let you = player1 tg
        positions you position
        card <- encounterCard Cards.street65009
        resolve [RevealEncounterCard you card]
        sid <- selectJust $ SideSchemeIs Cards.street65009
        getSchemeThreatAmount (SchemeSideSchemeId sid) `shouldReturn` (if startsOnStreets then 2 else 3)
        mainSchemeThreatNow `shouldReturn` (if startsOnStreets then 1 else 0)

  it "position changes happen at most once per phase for repeated encounter effects" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      let you = player1 tg
      positions you Cards.street65003a
      for_ [1 :: Int,2] $ \_ -> encounterCard Cards.street65010 >>= \card -> resolve [RevealEncounterCard you card]
      mainSchemeThreatNow `shouldReturn` 4
      damageOf you `shouldReturn` 0

  for_ [False,True] $ \hero ->
    it ("No Time to Think activates the villain against the player's current form, hero=" <> show hero) do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        let you = player1 tg
        when hero $ changeForm you
        clearHand you
        void $ setEncounterDeck (replicate 4 FNE.street60188) []
        card <- encounterCard Cards.street65044
        resolve [RevealEncounterCard you card]
        damageOf you `shouldReturn` (if hero then 2 else 0)
        mainSchemeThreatNow `shouldReturn` (if hero then 0 else 1)

  it "Dead End adds extra threat when Shifting Corridors is absent" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      card <- encounterCard Cards.street65042
      resolve [RevealEncounterCard (player1 tg) card]
      sid <- selectJust $ SideSchemeIs Cards.street65042
      getSchemeThreatAmount (SchemeSideSchemeId sid) `shouldReturn` 3

  it "Hall of Hidden Doors removes main-scheme threat before revealing the discarded minion" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      let you = player1 tg
      main <- selectJust EncounterMainScheme
      resolve =<< L.runQueueT (L.capture $ L.placeThreatOn GameRef main 6)
      card <- encounterCard Cards.street65043
      resolve [RevealEncounterCard you card]
      sid <- selectJust $ SideSchemeIs Cards.street65043
      ([_,minion,_,_],_) <- setEncounterDeck [FNE.street60085,FNE.street60092,FNE.street60188,FNE.street60188] []
      resolve =<< L.runQueueT (L.capture $ L.removeSchemeThreat you (SchemeSideSchemeId sid) 3)
      mainSchemeThreatNow `shouldReturn` 1
      selectAny (MinionWithId $ MinionId $ unCardId $ ecCardId minion) `shouldReturn` True

  it "Fist's action pays corruption twice to defeat him without attack retaliation" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      let you = player1 tg
      uid <- corruptionFor you
      changeForm you
      mid <- addMinion you Cards.street65066
      choose "hurt Fist" (isAbilityOf mid)
      choose "defeat Fist" (isAbilityOf mid)
      getCountersOn uid `shouldReturn` 2
      selectAny (MinionWithId mid) `shouldReturn` False
      damageOf you `shouldReturn` 0

  it "defeating Hellbride corrupts and heals the defeating player" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      let you = player1 tg
      uid <- corruptionFor you
      resolve =<< L.runQueueT (L.capture $ L.damageIdentity you GameRef $ toDamage 8 FromAbility)
      mid <- addMinion you Cards.street65067
      resolve [MinionMessage mid $ MinionDamaged (IdentityRef you) $ toDamage 7 FromAbility]
      getCountersOn uid `shouldReturn` 2
      damageOf you `shouldReturn` 2

  it "Hellbride's discard action purifies without triggering her defeat effect" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      let you = player1 tg
      uid <- corruptionFor you
      resolve [PlaceCounters (UpgradeRef uid) 2]
      changeForm you
      mid <- addMinion you Cards.street65067
      resolve [MinionMessage mid $ MinionDamaged (IdentityRef you) $ toDamage 5 FromAbility]
      choose "discard the weakened Hellbride" (isAbilityOf mid)
      getCountersOn uid `shouldReturn` 1
      selectAny (MinionWithId mid) `shouldReturn` False
      identityMatches Matcher.ExhaustedIdentity you `shouldReturn` True

  it "Retainer's optional corruption reduces this attack by three" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      let you = player1 tg
      uid <- corruptionFor you
      changeForm you
      clearHand you
      mid <- addMinion you Cards.street65068
      runNow [MinionMessage mid $ MinionAttacks you]
      choose "accept Retainer's corruption" (isAbilityOf mid)
      pass
      getCountersOn uid `shouldReturn` 1
      damageOf you `shouldReturn` 1

  for_ [False,True] $ \accept ->
    it ("Snakeroot Elders puts its corruption on the chosen recipient, accept=" <> show accept) do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        let you = player1 tg
        uid <- corruptionFor you
        card <- revealNow you Cards.street65069
        choose "choose the corruption recipient" (isLabel $ if accept then "$label.slCorrupt" else "$label.slResist")
        pass
        getCountersOn uid `shouldReturn` (if accept then 1 else 0)
        let mid = MinionId $ unCardId $ ecCardId card
        getCountersOn mid `shouldReturn` (if accept then 0 else 1)
        minionMatches (MinionWithKeyword Guard <> MinionWithKeyword Patrol) mid `shouldReturn` not accept

  it "Veil of Nightmares trades one corruption for five threat" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      let you = player1 tg
      uid <- corruptionFor you
      changeForm you
      card <- encounterCard Cards.street65070
      resolve [RevealEncounterCard you card]
      sid <- selectJust $ SideSchemeIs Cards.street65070
      choose "remove the veil" (isAbilityOf sid)
      choose "remove threat from the veil" (isTargetChoice sid)
      pass
      getCountersOn uid `shouldReturn` 1
      selectAny (SideSchemeIs Cards.street65070) `shouldReturn` False


  for_ [(Cards.street65003a,True),(Cards.street65003b,False)] $ \(position,startsOnStreets) ->
    it ("Ghost Maker resolves its defeat penalty after moving from " <> show (toCardCode position)) do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        let you = player1 tg
        positions you position
        _ <- putIntoPlay you "01091"
        mid <- addMinion you Cards.street65006
        resolve [MinionMessage mid $ MinionDamaged (IdentityRef you) $ toDamage 4 FromAbility]
        damageOf you `shouldReturn` (if startsOnStreets then 0 else 2)
        supportsWith "01091" >>= \remaining -> length remaining `shouldBe` (if startsOnStreets then 0 else 1)

  it "the street movement action places three threat and stuns before moving the heroes" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      let you = player1 tg
      positions you Cards.street65003a
      eid <- selectJust $ EnvironmentWithTrait Trait.HeroPosition
      choose "move to the roof" (isAbilityOf eid)
      mainSchemeThreatNow `shouldReturn` 3
      getCharacterStatusCount (IdentityCharacter you) StunnedStatus `shouldReturn` 1
      Position.positionCode Trait.HeroPosition `shouldReturn` Just "65004b"

  it "the rooftop movement action takes three damage before moving the heroes" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      let you = player1 tg
      positions you Cards.street65003a
      eid <- selectJust $ EnvironmentWithTrait Trait.HeroPosition
      resolve =<< L.runQueueT (L.capture $ L.flipEnvironment eid)
      choose "move to the streets" (isAbilityOf eid)
      damageOf you `shouldReturn` 3
      Position.positionCode Trait.HeroPosition `shouldReturn` Just "65004a"


  for_ [(Cards.street65003a,False),(Cards.street65003b,True)] $ \(position,atop) -> do
    it ("Strategic Positioning changes its icon with " <> show (toCardCode position)) do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        let you = player1 tg
        positions you position
        acceleration <- getAccelerationCount
        hazards <- getHazardCount
        card <- encounterCard Cards.street65008
        resolve [RevealEncounterCard you card]
        getAccelerationCount `shouldReturn` (acceleration + if atop then 0 else 1)
        getHazardCount `shouldReturn` (hazards + if atop then 1 else 0)

    it ("Moonlit Silhouette activates according to " <> show (toCardCode position)) do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        let you = player1 tg
        changeForm you
        clearHand you
        positions you position
        void $ setEncounterDeck (replicate 4 FNE.street60188) []
        card <- encounterCard Cards.street65011
        resolve [RevealEncounterCard you card]
        damageOf you `shouldReturn` (if atop then 0 else 3)
        mainSchemeThreatNow `shouldReturn` (if atop then 1 else 0)

  it "Moonlit Silhouette changes position and confuses an alter-ego without an activation" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      let you = player1 tg
      positions you Cards.street65003a
      card <- encounterCard Cards.street65011
      resolve [RevealEncounterCard you card]
      Position.villainAtop `shouldReturn` True
      getCharacterStatusCount (IdentityCharacter you) ConfusedStatus `shouldReturn` 1
      mainSchemeThreatNow `shouldReturn` 0
      damageOf you `shouldReturn` 0

  it "Sealed Entrances uses basic ATK for thwarting and moves the villain when defeated" do
    tg <- testGame ["captain-marvel"]
    inGame tg do
      let you = player1 tg
      changeForm you
      clearHand you
      positions you Cards.street65003a
      card <- encounterCard Cards.street65007
      resolve [RevealEncounterCard you card]
      sid <- selectJust $ SideSchemeIs Cards.street65007
      choose "thwart with ATK" (isHeroThwart you)
      choose "thwart Sealed Entrances" (isTargetChoice sid)
      pass
      getSchemeThreatAmount (SchemeSideSchemeId sid) `shouldReturn` 1
      resolve [IdentityMessage you ReadiedIdentity]
      choose "finish the assault thwart" (isHeroThwart you)
      choose "defeat Sealed Entrances" (isTargetChoice sid)
      choose "resolve the defeat effect" (isAbilityOf sid)
      pass
      selectAny (SideSchemeIs Cards.street65007) `shouldReturn` False
      Position.villainAtop `shouldReturn` True

  for_ [False,True] $ \accept ->
    it ("Submit or Suffer resolves the chosen corruption branch, accept=" <> show accept) do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        let you = player1 tg
        uid <- corruptionFor you
        resolve [PlaceCounters (UpgradeRef uid) 2]
        changeForm you
        clearHand you
        vid <- villainIdNow
        resolve [ReplaceVillain vid "65057" Advanced]
        void $ setEncounterDeck (replicate 4 FNE.street60188) []
        _ <- revealNow you Cards.street65071
        choose "choose the penalty" (isLabel $ if accept then "$label.slCorrupt" else "$label.slResist")
        pass
        getCountersOn uid `shouldReturn` (if accept then 3 else 1)
        damageOf you `shouldReturn` (if accept then 0 else 3)

corruptionFor :: IdentityId -> GameAppT UpgradeId
corruptionFor you = do
  card <- encounterCard Cards.street65072a
  resolve [PutEncounterCardIntoPlay you card]
  selectJust $ UpgradeControlledBy (IdentityWithId you) <> UpgradeNamed "Creeping Corruption"
