module Marvel.Encounter.SinisterGuerrillaSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.EncounterSet (EncounterSet (GuerrillaTactics))
import Marvel.Matchers hiding (ExhaustedIdentity, ExhaustedAlly)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Runner qualified as Minion
import Marvel.Modifier
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import TestImport

modularGame :: Difficulty -> [Text] -> IO TestGame
modularGame difficulty heroes = do
  (app, ran, logged) <- dealtGameLogged (ScenarioSetup "01094" [GuerrillaTactics] difficulty) (map starter heroes) 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

spec :: Spec
spec = describe "Guerrilla Tactics" do
  describe "Life-Size Decoy" do
    for_ [Normal, Expert] \difficulty ->
      it ("has Toughness only in Expert, including when its boost puts it into play: " <> show difficulty) do
        tg <- modularGame difficulty ["spider-man-miles-morales"]
        let p = player1 tg
        inGame tg do
          mid <- addMinion p Minions.smLifeSizeDecoy
          attrs <- getsGame (toAttrs . fromMaybe (error "missing decoy") . HashMap.lookup mid . gameMinions)
          minionTough attrs `shouldBe` (difficulty == Expert)
          resolve [RemoveFromGame $ toRef mid]
          _ <- setEncounterDeck [Minions.smLifeSizeDecoy, Minions.hydraSoldier] []
          vid <- villainIdNow
          resolve [VillainMessage vid (VillainAttacks p)]
          decoys <- getsGame (filter ((== "27142") . toCardCode . getCardDef) . HashMap.elems . gameMinions)
          length decoys `shouldBe` 1
          minionTough (toAttrs $ head decoys) `shouldBe` (difficulty == Expert)

    it "stops its engaged player's identity and allies thwarting side schemes, but permits the main scheme and another player" do
      tg <- modularGame Normal ["spider-man-miles-morales", "ghost-spider"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        mid <- addMinion p Minions.smLifeSizeDecoy
        side <- revealNow p Schemes.smCoordinatedEffort
        main <- mainSchemeIdNow
        resolve [MainSchemeMessage main (MainSchemePlaceThreat 1)]
        pass
        changeForm p
        pass
        ally <- putIntoPlay p "01068"
        otherAlly <- putIntoPlay other "01066"
        getModifiers p >>= (`shouldSatisfy` elem CannotThwartSideSchemes)
        getModifiers other >>= (`shouldSatisfy` notElem CannotThwartSideSchemes)
        sides <- selectList (SchemeAHeroCanBasicThwart p)
        sides `shouldSatisfy` notElem (SchemeSideSchemeId $ sideSchemeOf side)
        main <- mainSchemeIdNow
        sides `shouldSatisfy` elem (SchemeMainSchemeId main)
        allySchemes <- selectList (SchemeAnAllyCanThwart (allyIdOf ally) False)
        allySchemes `shouldSatisfy` notElem (SchemeSideSchemeId $ sideSchemeOf side)
        allySchemes `shouldSatisfy` elem (SchemeMainSchemeId main)
        otherAllySchemes <- selectList (SchemeAnAllyCanThwart (allyIdOf otherAlly) False)
        otherAllySchemes `shouldSatisfy` elem (SchemeSideSchemeId $ sideSchemeOf side)
        before <- sideSchemeThreat (sideSchemeOf side)
        resolve [SideSchemeMessage (sideSchemeOf side) (SideSchemeThwarted (AllyRef $ allyIdOf ally) 1)]
        sideSchemeThreat (sideSchemeOf side) `shouldReturn` before
        -- Removing threat without thwarting remains legal for the engaged player's cards.
        resolve [SideSchemeMessage (sideSchemeOf side) (SideSchemeThreatRemoved (AllyRef $ allyIdOf ally) 1)]
        sideSchemeThreat (sideSchemeOf side) `shouldReturn` (before - 1)
        resolve [MinionMessage mid (MinionDamaged GameRef $ toDamage 5 FromAbility)]
        getModifiers p >>= (`shouldSatisfy` notElem CannotThwartSideSchemes)

  describe "Coordinated Effort" do
    it "gives every enemy an acceleration icon, updates as enemies enter and leave, and removes the icons with the scheme" do
      tg <- modularGame Normal ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        before <- getAccelerationCount
        scheme <- revealNow p Schemes.smCoordinatedEffort
        pass
        getAccelerationCount `shouldReturn` before + 1
        mid <- addMinion p Minions.smLifeSizeDecoy
        getAccelerationCount `shouldReturn` before + 2
        getModifiers mid >>= (`shouldSatisfy` elem (AccelerationIconModifier 1))
        resolve [MinionMessage mid (MinionDamaged GameRef $ toDamage 5 FromAbility)]
        getAccelerationCount `shouldReturn` before + 1
        resolve [SideSchemeMessage (sideSchemeOf scheme) (SideSchemeThwarted GameRef 6)]
        getAccelerationCount `shouldReturn` before

    for_ [Normal, Expert] \difficulty ->
      it ("places a boost threat on every scheme, plus one on the main scheme in Expert: " <> show difficulty) do
        tg <- modularGame difficulty ["spider-man-miles-morales"]
        let p = player1 tg
        inGame tg do
          side <- revealNow p Schemes.smVolunteerWork
          pass
          before <- mainSchemeThreatNow
          _ <- setEncounterDeck [Schemes.smCoordinatedEffort, Minions.hydraSoldier] []
          vid <- villainIdNow
          resolve [VillainMessage vid (VillainAttacks p)]
          mainSchemeThreatNow `shouldReturn` before + (if difficulty == Expert then 2 else 1)
          sideSchemeThreat (sideSchemeOf side) `shouldReturn` 4

  describe "Hidden in Shadow" do
    it "gives every enemy a hazard icon and removes those icons as the enemies leave" do
      tg <- modularGame Normal ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        before <- getHazardCount
        scheme <- revealNow p Schemes.smHiddenInShadow
        pass
        getHazardCount `shouldReturn` before + 1
        mid <- addMinion p Minions.smLifeSizeDecoy
        getHazardCount `shouldReturn` before + 2
        resolve [SideSchemeMessage (sideSchemeOf scheme) (SideSchemeThwarted GameRef 4)]
        getHazardCount `shouldReturn` before
        getModifiers mid >>= (`shouldSatisfy` notElem (HazardIconModifier 1))

    for_ [Normal, Expert] \difficulty ->
      it ("deals boost indirect damage to each player and one extra to the first player in Expert: " <> show difficulty) do
        tg <- modularGame difficulty ["spider-man-miles-morales", "ghost-spider"]
        let first = player1 tg
            second = player2 tg
        inGame tg do
          base <- Villain.getModifiedAttack . toAttrs =<< getVillain
          _ <- setEncounterDeck [Schemes.smHiddenInShadow, Minions.hydraSoldier] []
          vid <- villainIdNow
          resolve [VillainMessage vid (VillainAttacks second)]
          damageOf first `shouldReturn` (if difficulty == Expert then 2 else 1)
          damageOf second `shouldReturn` base + 2

  describe "Teamwork Makes the Dream Work" do
    it "increases every enemy's ATK and SCH, and restores both when defeated" do
      tg <- modularGame Normal ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        mid <- addMinion p Minions.hydraSoldier
        base <- Villain.getModifiedAttack . toAttrs =<< getVillain
        scheme <- revealNow p Schemes.smTeamworkMakesTheDreamWork
        pass
        (Villain.getModifiedAttack . toAttrs =<< getVillain) `shouldReturn` base + 1
        attrs <- getsGame (toAttrs . fromMaybe (error "missing soldier") . HashMap.lookup mid . gameMinions)
        Minion.getModifiedAttack attrs `shouldReturn` 3
        Minion.getModifiedScheme attrs `shouldReturn` 2
        resolve [SideSchemeMessage (sideSchemeOf scheme) (SideSchemeThwarted GameRef 5)]
        Minion.getModifiedAttack attrs `shouldReturn` 2
        Minion.getModifiedScheme attrs `shouldReturn` 1

    for_ [Normal, Expert] \difficulty ->
      it ("adds two boost icons only in Expert: " <> show difficulty) do
        tg <- modularGame difficulty ["spider-man-miles-morales"]
        let p = player1 tg
        inGame tg do
          base <- Villain.getModifiedAttack . toAttrs =<< getVillain
          _ <- setEncounterDeck [Schemes.smTeamworkMakesTheDreamWork, Minions.hydraSoldier] []
          vid <- villainIdNow
          resolve [VillainMessage vid (VillainAttacks p)]
          damageOf p `shouldReturn` base + (if difficulty == Expert then 4 else 2)

    it "applies its boost bonus once when Chaos Control replaces the count and reopens the window" do
      tg <- modularGame Expert ["scarlet-witch"]
      let p = player1 tg
      inGame tg do
        changeForm p
        pass
        clearHand p
        base <- Villain.getModifiedAttack . toAttrs =<< getVillain
        _ <- setEncounterDeck [Schemes.smTeamworkMakesTheDreamWork, Treacheries.explosion, Minions.hydraSoldier] []
        vid <- villainIdNow
        start [VillainMessage vid (VillainAttacks p)]
        choose "do not defend" isNoDefender
        choose "replace the boost count" (isNamedAbility "Chaos Control")
        pass
        damageOf p `shouldReturn` base + 2

  describe "From Every Direction" do
    for_ [Normal, Expert] \difficulty ->
      it ("places threat for the villain and each minion, and surges only in Expert: " <> show difficulty) do
        tg <- modularGame difficulty ["spider-man-miles-morales"]
        let p = player1 tg
        inGame tg do
          _ <- addMinion p Minions.smLifeSizeDecoy
          _ <- setEncounterDeck [Schemes.bombScare, Minions.hydraSoldier] []
          before <- mainSchemeThreatNow
          _ <- revealNow p Treacheries.smFromEveryDirection
          pass
          mainSchemeThreatNow `shouldReturn` before + 2
          present <- any ((== "01109") . toCardCode) <$> dealtTo p
          present `shouldBe` (difficulty == Expert)
