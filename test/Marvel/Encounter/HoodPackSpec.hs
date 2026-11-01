module Marvel.Encounter.HoodPackSpec (spec) where

import Data.Text qualified as T
import Data.Maybe (fromJust)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Marvel.Ally.Types qualified as Ally
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Cost
import Marvel.Damage
import Marvel.EncounterSet qualified as Set
import Marvel.Environment.Cards qualified as Environments
import Marvel.Hood.Helpers
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Modifier
import Marvel.Query
import Marvel.Scenario.Types qualified as Scenario
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import TestImport

apply :: L.QueueT GameAppT () -> GameAppT ()
apply body = resolve =<< L.runQueueT (L.capture body)

reveal :: IdentityId -> CardDef -> GameAppT EncounterCard
reveal you def = do
  card <- encounterCard def
  resolve [RevealEncounterCard you card]
  pure card

resolveBoost :: CardDef -> GameAppT ()
resolveBoost def = do
  vid <- villainIdNow
  card <- encounterCard def
  resolve [RevealBoostCard card (EnemyVillainId vid)]

blankDeck :: GameAppT ()
blankDeck = void $ setEncounterDeck (replicate 30 Minions.hydraMercenary) []

asHero :: IdentityId -> GameAppT ()
asHero you = changeForm you >> pass >> clearHand you

settleEffects :: GameAppT ()
settleEffects = openQuestion >>= \case
  Just (_, QuestionLabel "$label.yourTurn" _ _) -> pure ()
  Just (you, _) -> answerQuestion you 0 >> settleEffects
  Nothing -> pure ()

minionAttrs :: MinionId -> GameAppT (Attrs Minion.Minion)
minionAttrs mid = getsGame (toAttrs . fromJust . lookup mid . gameMinions)

attachmentAttrs :: EncounterCard -> GameAppT (Attrs Attachment.Attachment)
attachmentAttrs c = getsGame (toAttrs . fromJust . lookup (AttachmentId $ unCardId $ ecCardId c) . gameAttachments)

spec :: Spec
spec = describe "The Hood pack" do
  describe "setup" do
    it "registers all nine complete modular sets" do
      let sets = [Set.StreetsOfMayhem,Set.BeastyBoys,Set.BrothersGrimm,Set.CrossfiresCrew,Set.MisterHydeSet,Set.RansackedArmory,Set.SinisterSyndicate,Set.StateOfEmergency,Set.WreckingCrewModular]
      for_ sets $ \set -> set `shouldSatisfy` (`elem` Set.modularSets)
      pure () :: IO ()
    for_ [(Normal,[Set.StandardII]),(Expert,[Set.StandardII,Set.ExpertII]),(Expert,[Set.ExpertII])] $ \(mode,sets) ->
      it ("replaces the selected standard/expert sets in " <> show mode <> show sets) do
        let setup = ScenarioSetup "01094" sets mode
            sc = fromJust $ lookupScenario setup
            chosen = Scenario.scenarioEncounterSets $ toAttrs sc
        for_ sets $ \set -> HashSet.member set chosen `shouldBe` True
        when (Set.StandardII `elem` sets) $ HashSet.member Set.Standard chosen `shouldBe` False
        when (Set.ExpertII `elem` sets) $ HashSet.member Set.ExpertSet chosen `shouldBe` False
        pure () :: IO ()
    for_ [Normal,Expert] $ \mode ->
      it ("puts Formidable Foe into play on its " <> show mode <> " side") do
        (app,ran,logged) <- dealtGameLogged (ScenarioSetup "01094" [Set.StandardII] mode) [starter "spider-man"] 1
        seats <- runGameApp app (getsGame gameSeats)
        inGame (TestGame app ran logged seats) do
          toFirstTurn
          envs <- getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameEnvironments)
          envs `shouldSatisfy` elem (if mode == Expert then "24049b" else "24049a")
          vid <- villainIdNow
          getModifiers vid >>= (`shouldSatisfy` elem (KeywordModifier Steady))

  describe "Streets of Mayhem" do
    for_ [(Environments.hood24060,AttackModifier 1),(Environments.hood24061,ThwartModifier 1),(Environments.hood24062,KeywordModifier $ Retaliate 1),(Environments.hood24063,KeywordModifier Steady)] $ \(def,modifier) ->
      it ("applies " <> show modifier <> " and replaces the previous Setting") do
        tg <- testGame ["spider-man"]
        inGame tg do
          blankDeck
          old <- reveal (player1 tg) Environments.mojo39041
          _ <- reveal (player1 tg) def
          getModifiers (player1 tg) >>= (`shouldSatisfy` elem modifier)
          encounterDiscard >>= (`shouldSatisfy` elem old)
    it "Secret Lair gives each enemy an acceleration icon" do
      tg <- testGame ["spider-man"]
      inGame tg do
        blankDeck
        mid <- addMinion (player1 tg) Minions.hydraSoldier
        _ <- reveal (player1 tg) Environments.hood24061
        getModifiers mid >>= (`shouldSatisfy` elem (AccelerationIconModifier 1))

    it "Warehouse District requires two stun cards to cancel an ally attack" do
      tg <- testGame ["spider-man"]
      inGame tg do
        ally <- putIntoPlay (player1 tg) "01067"
        let aid = allyIdOf ally
        _ <- reveal (player1 tg) Environments.hood24063
        apply $ L.giveStatus aid StunnedStatus
        resolve [AllyMessage aid AllyAttacked]
        villainDamageNow `shouldReturn` 1
        apply $ L.giveStatus aid StunnedStatus
        resolve [AllyMessage aid AllyAttacked]
        villainDamageNow `shouldReturn` 1
        getCharacterStatusCount (AllyCharacter aid) StunnedStatus `shouldReturn` 0

  describe "Beasty Boys" do
    it "Beast Mode increases damage to a character with a status by one" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- reveal (player1 tg) Schemes.hood24014
        apply $ L.giveStatus (player1 tg) StunnedStatus
        apply $ L.damageIdentity (player1 tg) GameRef (toDamage 2 FromAbility)
        damageOf (player1 tg) `shouldReturn` 3
    it "Griffin stuns the character he damages" do
      tg <- testGame ["spider-man"]
      inGame tg do
        blankDeck
        mid <- addMinion (player1 tg) Minions.hood24015
        asHero (player1 tg)
        resolve [MinionMessage mid (MinionAttacks $ player1 tg)]
        characterMatches (CharacterWithStatus StunnedStatus) (IdentityCharacter $ player1 tg) `shouldReturn` True
    it "Griffin returns to the encounter deck when a friendly character has a stun" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.hood24015
        apply $ L.giveStatus (player1 tg) StunnedStatus
        resolve [MinionMessage mid MinionDefeated]
        scenarioEncounterDeckNow >>= (`shouldSatisfy` any ((== "24015") . toCardCode))
    it "Mandrill confuses controlled characters and gains retaliate for each confused character" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- reveal (player1 tg) Minions.hood24016
        mid <- selectJust (MinionIs Minions.hood24016)
        characterMatches (CharacterWithStatus ConfusedStatus) (IdentityCharacter $ player1 tg) `shouldReturn` True
        getModifiers mid >>= (`shouldSatisfy` elem (KeywordModifier $ Retaliate 1))
    it "Double Trouble stuns and confuses controlled characters" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- reveal (player1 tg) Treacheries.hood24017
        for_ [StunnedStatus,ConfusedStatus] $ \status -> characterMatches (CharacterWithStatus status) (IdentityCharacter $ player1 tg) `shouldReturn` True

  describe "Brothers Grimm" do
    it "Brothers Grimm discards into and reveals an attachment before activating" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.hood24018
        void $ setEncounterDeck [Treacheries.advance,Attachments.hood24019,Minions.hydraSoldier] []
        resolve [MinionMessage mid (MinionSchemesAgainst $ player1 tg)]
        length <$> dealtTo (player1 tg) `shouldReturn` 1
        length <$> handOf (player1 tg) `shouldReturn` 5
    for_ [(Attachments.hood24019,0),(Attachments.hood24020,3),(Attachments.hood24021,0),(Attachments.hood24022,0)] $ \(def,damage) ->
      it (show (toCardCode def) <> " attaches to a Mystic and triggers after its activation") do
        tg <- testGame ["spider-man"]
        inGame tg do
          blankDeck
          mid <- addMinion (player1 tg) Minions.hood24018
          card <- reveal (player1 tg) def
          attachmentEnemy <- Attachment.attachmentEnemy <$> attachmentAttrs card
          attachmentEnemy `shouldBe` Just (EnemyMinionId mid)
          resolve [MinionMessage mid (MinionSchemesAgainst $ player1 tg)]
          length <$> dealtTo (player1 tg) `shouldReturn` 1
          damageOf (player1 tg) `shouldReturn` damage
          encounterDiscard >>= (`shouldSatisfy` elem card)
    it "a Grimm attachment falls back to the villain without a Mystic minion" do
      tg <- testGame ["spider-man"]
      inGame tg do
        card <- reveal (player1 tg) Attachments.hood24021
        vid <- villainIdNow
        Attachment.attachmentEnemy <$> attachmentAttrs card `shouldReturn` Just (EnemyVillainId vid)

    it "a Grimm attachment affects the player activated against, even during another player's turn" do
      tg <- testGame ["spider-man","captain-marvel"]
      inGame tg do
        _ <- reveal (player1 tg) Attachments.hood24019
        void $ setEncounterDeck [Attachments.theHoodsMantle,Minions.hydraSoldier] []
        first <- length <$> handOf (player1 tg)
        other <- length <$> handOf (player2 tg)
        vid <- villainIdNow
        apply $ L.villainSchemesAgainst vid (player2 tg)
        length <$> handOf (player1 tg) `shouldReturn` first
        length <$> handOf (player2 tg) `shouldReturn` other - 1
        length <$> dealtTo (player2 tg) `shouldReturn` 1

  describe "Crossfire's Crew" do
    it "Out for Blood repeats after defeating the weakest friendly character" do
      tg <- testGame ["spider-man"]
      inGame tg do
        ally <- putIntoPlay (player1 tg) "01067"
        let aid = allyIdOf ally
        resolve [AllyMessage aid (AllyDamaged GameRef $ toDamage 1 FromAbility)]
        _ <- reveal (player1 tg) Schemes.hood24023
        selectList (AllyWithId aid) `shouldReturn` []
        damageOf (player1 tg) `shouldReturn` 1
    it "Controller increases attack damage by the defending character's ATK" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.hood24024
        asHero (player1 tg)
        resolve [MinionMessage mid (MinionAttacks $ player1 tg)]
        damageOf (player1 tg) `shouldReturn` 3
    it "Corruptor exhausts allies and places threat for those exhausted" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- putIntoPlay (player1 tg) "01067"
        before <- mainSchemeThreatNow
        _ <- reveal (player1 tg) Minions.hood24025
        mainSchemeThreatNow `shouldReturn` before + 1
    it "Crossfire attacks the friendly character with the fewest hit points" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.hood24026
        ally <- putIntoPlay (player1 tg) "01067"
        let aid = allyIdOf ally
        asHero (player1 tg)
        resolve [MinionMessage mid (MinionAttacks $ player1 tg)]
        getsGame (fmap (Minion.minionAttacking . toAttrs) . lookup mid . gameMinions) `shouldReturn` Just Nothing
        selectList (AllyWithId aid) `shouldReturn` []
        damageOf (player1 tg) `shouldReturn` 0
    it "Mister Fear adds a mental resource cost to readying heroes and allies" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- addMinion (player1 tg) Minions.hood24027
        asHero (player1 tg)
        apply $ L.exhaust (player1 tg)
        apply $ L.ready (player1 tg)
        exhaustedNow (player1 tg) `shouldReturn` True
    it "Mister Fear allows a hero to pay the mental resource and ready" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- addMinion (player1 tg) Minions.hood24027
        asHero (player1 tg)
        resource <- putInHand (player1 tg) "01089"
        apply $ L.exhaust (player1 tg)
        runNow [IdentityMessage (player1 tg) ReadiedIdentityByGame]
        choose "pay to ready" (isLabel "$label.payToReady")
        payWith [resource]
        pass
        exhaustedNow (player1 tg) `shouldReturn` False
    it "Caught in the Crossfire reveals a Crew minion and then counts it for indirect damage" do
      tg <- testGame ["spider-man"]
      inGame tg do
        void $ setEncounterDeck [Treacheries.advance,Minions.hood24025,Minions.hydraSoldier] []
        _ <- reveal (player1 tg) Treacheries.hood24028
        selectList (MinionIs Minions.hood24025) >>= (`shouldSatisfy` not . null)
        damageOf (player1 tg) `shouldReturn` 1

  describe "Mister Hyde" do
    it "Self-Experimentation reveals Mister Hyde and redirects Brute damage into threat removal" do
      tg <- testGame ["spider-man"]
      inGame tg do
        void $ setEncounterDeck [Minions.hood24035,Minions.hydraSoldier] []
        card <- reveal (player1 tg) Schemes.hood24033
        mid <- selectJust (MinionIs Minions.hood24035)
        apply $ L.dealDamage mid GameRef (toDamage 3 FromAbility)
        Minion.minionDamage <$> minionAttrs mid `shouldReturn` 0
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 2
    it "Calvin Zabo's defeat puts Mister Hyde into play with the same player" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.hood24034
        void $ setEncounterDeck [Minions.hood24035,Minions.hydraSoldier] []
        resolve [MinionMessage mid MinionDefeated]
        selectList (MinionIs Minions.hood24035) >>= (`shouldSatisfy` not . null)
    it "revealed Mister Hyde replaces Calvin Zabo, gains tough, and damages heroes and allies" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.hood24034
        asHero (player1 tg)
        _ <- reveal (player1 tg) Minions.hood24035
        selectList (MinionWithId mid) `shouldReturn` []
        hyde <- selectJust (MinionIs Minions.hood24035)
        characterMatches (CharacterWithStatus ToughStatus) (MinionCharacter hyde) `shouldReturn` True
        damageOf (player1 tg) `shouldReturn` 1
    it "Hyde Formula makes Calvin Zabo scheme with +3 SCH before taking four damage" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.hood24034
        before <- mainSchemeThreatNow
        _ <- reveal (player1 tg) Treacheries.hood24036
        mainSchemeThreatNow `shouldReturn` before + 6
        selectList (MinionWithId mid) `shouldReturn` []

  describe "Ransacked Armory" do
    for_ [(Attachments.hood24037,AttackModifier 3),(Attachments.hood24038,HitPointModifier 4),(Attachments.hood24040,HitPointModifier 3)] $ \(def,modifier) ->
      it (show (toCardCode def) <> " attaches to the minion with most remaining hit points") do
        tg <- testGame ["spider-man"]
        inGame tg do
          mid <- addMinion (player1 tg) Minions.hood24035
          _ <- addMinion (player1 tg) Minions.hydraSoldier
          card <- reveal (player1 tg) def
          Attachment.attachmentEnemy <$> attachmentAttrs card `shouldReturn` Just (EnemyMinionId mid)
          getModifiers mid >>= (`shouldSatisfy` elem modifier)
    it "Jetpack reduces attack damage by the discarded card's boost icons" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.hood24035
        _ <- reveal (player1 tg) Attachments.hood24039
        void $ setEncounterDeck [Treacheries.shadowOfThePast,Minions.hydraSoldier] []
        apply $ L.dealDamage mid (player1 tg) (toDamage 4 $ FromPlayerAttack $ player1 tg)
        Minion.minionDamage <$> minionAttrs mid `shouldReturn` 2
    it "Armored Guard enters with tough and guard" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.hood24041
        a <- minionAttrs mid
        Minion.minionTough a `shouldBe` True
        Minion.getModifiedKeywords a >>= (`shouldSatisfy` elem Guard)

  describe "Sinister Syndicate" do
    it "Crime Pays finds a Criminal in the deck and puts it into play" do
      tg <- testGame ["spider-man"]
      inGame tg do
        void $ setEncounterDeck [Minions.hood24043,Minions.hydraSoldier] []
        _ <- reveal (player1 tg) Schemes.hood24042
        selectList (MinionIs Minions.hood24043) >>= (`shouldSatisfy` not . null)
    it "Beetle discards the lowest-cost upgrade after damaging the hero" do
      tg <- testGame ["spider-man"]
      inGame tg do
        cheap <- putIntoPlay (player1 tg) "01081"
        _ <- putIntoPlay (player1 tg) "01093"
        mid <- addMinion (player1 tg) Minions.hood24043
        asHero (player1 tg)
        resolve [MinionMessage mid (MinionAttacks $ player1 tg)]
        discardOf (player1 tg) >>= (`shouldSatisfy` elem cheap)
    it "Boomerang damages every controlled ally after attacking" do
      tg <- testGame ["spider-man"]
      inGame tg do
        ally <- putIntoPlay (player1 tg) "01067"
        let aid = allyIdOf ally
        mid <- addMinion (player1 tg) Minions.hood24044
        asHero (player1 tg)
        resolve [MinionMessage mid (MinionAttacks $ player1 tg)]
        getsGame (maybe 0 (Ally.allyDamage . toAttrs) . lookup aid . gameAllies) `shouldReturn` 1
    it "Shocker stuns a character after it attacks him" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.hood24045
        asHero (player1 tg)
        choose "basic attack" (isHeroAttack $ player1 tg)
        choose "attack minion" (\case TargetLabel target _ -> target == MinionRef mid; _ -> False)
        settleEffects
        characterMatches (CharacterWithStatus StunnedStatus) (IdentityCharacter $ player1 tg) `shouldReturn` True
    it "Speed Demon attacks the attacking character first" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.hood24046
        asHero (player1 tg)
        choose "basic attack" (isHeroAttack $ player1 tg)
        choose "attack minion" (\case TargetLabel target _ -> target == MinionRef mid; _ -> False)
        settleEffects
        damageOf (player1 tg) `shouldReturn` 2
    it "Speed Demon prevents an ally it defeats from completing its attack" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.hood24046
        ally <- putIntoPlay (player1 tg) "01067"
        let aid = allyIdOf ally
        runNow [AllyMessage aid AllyAttacked]
        choose "attack Speed Demon" (\case TargetLabel target _ -> target == MinionRef mid; _ -> False)
        settleEffects
        selectList (AllyWithId aid) `shouldReturn` []
        Minion.minionDamage <$> minionAttrs mid `shouldReturn` 0
    it "White Rabbit discards a random card before attacking" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.hood24047
        asHero (player1 tg)
        c <- putInHand (player1 tg) "01090"
        resolve [MinionMessage mid (MinionAttacks $ player1 tg)]
        discardOf (player1 tg) >>= (`shouldSatisfy` elem c)
    it "Sinister Onslaught activates Criminal enemies against the resolving player" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- addMinion (player1 tg) Minions.hood24047
        void $ setEncounterDeck [Attachments.theHoodsMantle,Minions.hydraSoldier] []
        before <- mainSchemeThreatNow
        _ <- reveal (player1 tg) Treacheries.hood24048
        mainSchemeThreatNow `shouldReturn` before + 3

    it "Sinister Onslaught gains surge when confusion replaces every scheme" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.hood24047
        vid <- villainIdNow
        apply $ L.giveStatus vid ConfusedStatus >> L.giveStatus mid ConfusedStatus
        blankDeck
        before <- mainSchemeThreatNow
        _ <- reveal (player1 tg) Treacheries.hood24048
        mainSchemeThreatNow `shouldReturn` before
        length <$> dealtTo (player1 tg) `shouldReturn` 1

  describe "State of Emergency" do
    it "Feisty Heist discards the highest-cost card from hand" do
      tg <- testGame ["spider-man"]
      inGame tg do
        clearHand (player1 tg)
        high <- putInHand (player1 tg) "01005"
        low <- putInHand (player1 tg) "01089"
        _ <- reveal (player1 tg) Schemes.hood24055
        handOf (player1 tg) `shouldReturn` [low]
        discardOf (player1 tg) >>= (`shouldSatisfy` elem high)
    it "Disaster at the Docks deals three indirect damage" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- reveal (player1 tg) Schemes.hood24056
        damageOf (player1 tg) `shouldReturn` 3
    it "Offshore Inferno discards the lowest-cost controlled card" do
      tg <- testGame ["spider-man"]
      inGame tg do
        cheap <- putIntoPlay (player1 tg) "01081"
        _ <- putIntoPlay (player1 tg) "01093"
        _ <- reveal (player1 tg) Schemes.hood24057
        discardOf (player1 tg) >>= (`shouldSatisfy` elem cheap)
    it "Hot Pursuit discards into a minion without revealing it" do
      tg <- testGame ["spider-man"]
      inGame tg do
        void $ setEncounterDeck [Treacheries.advance,Minions.hood24016,Minions.hydraSoldier] []
        _ <- reveal (player1 tg) Schemes.hood24058
        selectList (MinionIs Minions.hood24016) >>= (`shouldSatisfy` not . null)
        characterMatches (CharacterWithStatus ConfusedStatus) (IdentityCharacter $ player1 tg) `shouldReturn` False
    it "Citywide Crisis repeats side scheme When Revealed abilities without resetting threat" do
      tg <- testGame ["spider-man"]
      inGame tg do
        card <- reveal (player1 tg) Schemes.hood24056
        _ <- reveal (player1 tg) Treacheries.hood24059
        damageOf (player1 tg) `shouldReturn` 6
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 3
    it "Citywide Crisis places two threat on every scheme when none has a When Revealed" do
      tg <- testGame ["spider-man"]
      inGame tg do
        before <- mainSchemeThreatNow
        _ <- reveal (player1 tg) Treacheries.hood24059
        mainSchemeThreatNow `shouldReturn` before + 2

  describe "Wrecking Crew" do
    it "Top Talent grants retaliate to the villain and Elite minions and has hinder" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.hood24065
        card <- reveal (player1 tg) Schemes.hood24064
        vid <- villainIdNow
        for_ [MinionRef mid,VillainRef vid] $ \target -> getModifiers target >>= (`shouldSatisfy` elem (KeywordModifier $ Retaliate 1))
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 5
    it "Wrecker gains two ATK on an undefended attack" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.hood24065
        asHero (player1 tg)
        void $ setEncounterDeck [Attachments.theHoodsMantle,Minions.hydraSoldier] []
        resolve [MinionMessage mid (MinionAttacks $ player1 tg)]
        damageOf (player1 tg) `shouldReturn` 4
    it "Bulldozer's attacks have overkill" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.hood24066
        keys <- Minion.getModifiedKeywords =<< minionAttrs mid
        keys `shouldSatisfy` elem Overkill
    it "Piledriver has retaliate and villainous" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.hood24067
        keys <- Minion.getModifiedKeywords =<< minionAttrs mid
        keys `shouldSatisfy` elem (Retaliate 1)
        keys `shouldSatisfy` elem Villainous
    it "Thunderball damages each controlled character after attacking" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.hood24068
        asHero (player1 tg)
        void $ setEncounterDeck [Attachments.theHoodsMantle,Minions.hydraSoldier] []
        resolve [MinionMessage mid (MinionAttacks $ player1 tg)]
        damageOf (player1 tg) `shouldReturn` 2
    it "Combined Effort activates each Elite minion" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- addMinion (player1 tg) Minions.hood24065
        void $ setEncounterDeck [Attachments.theHoodsMantle,Minions.hydraSoldier] []
        before <- mainSchemeThreatNow
        _ <- reveal (player1 tg) Treacheries.hood24069
        mainSchemeThreatNow `shouldReturn` before + 2
    it "Combined Effort's boost adds one icon for each Elite minion" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- addMinion (player1 tg) Minions.hood24065
        asHero (player1 tg)
        void $ setEncounterDeck [Treacheries.hood24069,Minions.hydraSoldier] []
        apply $ L.currentVillainAttacks (player1 tg)
        damageOf (player1 tg) `shouldReturn` 4
    it "Magic Muscle gives Brutes tough" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.hood24065
        _ <- reveal (player1 tg) Treacheries.hood24070
        characterMatches (CharacterWithStatus ToughStatus) (MinionCharacter mid) `shouldReturn` True

  describe "boost abilities" do
    it "Double Trouble resolves both status choices as a boost" do
      tg <- testGame ["spider-man"]
      inGame tg do
        resolveBoost Treacheries.hood24017
        for_ [StunnedStatus,ConfusedStatus] $ \status -> characterMatches (CharacterWithStatus status) (IdentityCharacter $ player1 tg) `shouldReturn` True
    it "Brothers Grimm enters play with the first player after the activation" do
      tg <- testGame ["spider-man"]
      inGame tg do
        resolveBoost Minions.hood24018
        selectList (MinionIs Minions.hood24018) >>= (`shouldSatisfy` not . null)
    it "Brothers Grimm still enters play after the activation clears its boost cards" do
      tg <- testGameAgainst "40103" ["spider-man"]
      inGame tg do
        asHero (player1 tg)
        void $ setEncounterDeck [Minions.hood24018,Minions.hydraSoldier] []
        apply $ L.currentVillainAttacks (player1 tg)
        selectList (MinionIs Minions.hood24018) >>= (`shouldSatisfy` not . null)
    it "Out for Blood deals its damage without putting the scheme into play" do
      tg <- testGame ["spider-man"]
      inGame tg do
        resolveBoost Schemes.hood24023
        damageOf (player1 tg) `shouldReturn` 1
        selectList (SideSchemeIs Schemes.hood24023) `shouldReturn` []
    it "Corruptor exhausts a controlled character" do
      tg <- testGame ["spider-man"]
      inGame tg do
        resolveBoost Minions.hood24025
        exhaustedNow (player1 tg) `shouldReturn` True
    it "Mister Fear discards from the player's deck through the first ally" do
      tg <- testGame ["spider-man"]
      inGame tg do
        cards <- stackDeck (player1 tg) ["01090","01067","01093"]
        resolveBoost Minions.hood24027
        discarded <- discardOf (player1 tg)
        take 2 cards `shouldSatisfy` all (`elem` discarded)
    it "Beetle discards a controlled upgrade" do
      tg <- testGame ["spider-man"]
      inGame tg do
        card <- putIntoPlay (player1 tg) "01081"
        resolveBoost Minions.hood24043
        discardOf (player1 tg) >>= (`shouldSatisfy` elem card)
    it "Boomerang deals two damage to a chosen ally" do
      tg <- testGame ["spider-man"]
      inGame tg do
        ally <- putIntoPlay (player1 tg) "01067"
        resolveBoost Minions.hood24044
        selectList (AllyWithId $ allyIdOf ally) `shouldReturn` []
    it "Shocker stuns the controlled character with the highest ATK" do
      tg <- testGame ["spider-man"]
      inGame tg do
        resolveBoost Minions.hood24045
        characterMatches (CharacterWithStatus StunnedStatus) (IdentityCharacter $ player1 tg) `shouldReturn` True
    it "Speed Demon discards the lowest-cost support" do
      tg <- testGame ["spider-man"]
      inGame tg do
        low <- putIntoPlay (player1 tg) "01092"
        _ <- putIntoPlay (player1 tg) "01091"
        resolveBoost Minions.hood24046
        discardOf (player1 tg) >>= (`shouldSatisfy` elem low)
    it "White Rabbit discards an identity-specific card from hand" do
      tg <- testGame ["spider-man"]
      inGame tg do
        clearHand (player1 tg)
        card <- putInHand (player1 tg) "01005"
        _ <- putInHand (player1 tg) "01090"
        resolveBoost Minions.hood24047
        discardOf (player1 tg) >>= (`shouldSatisfy` elem card)
    it "Cruel Intentions deals the resolving player an encounter card" do
      tg <- testGame ["spider-man"]
      inGame tg do
        blankDeck
        resolveBoost Treacheries.hood24029
        length <$> dealtTo (player1 tg) `shouldReturn` 1
    for_ [Treacheries.hood24032,Treacheries.hood24050,Treacheries.hood24052] $ \def ->
      it (show (toCardCode def) <> " gives the villain an additional boost card") do
        tg <- testGame ["spider-man"]
        inGame tg do
          blankDeck
          resolveBoost def
          length . Villain.villainBoostCards . toAttrs <$> getVillain `shouldReturn` 1
    it "Citywide Crisis repeats a side scheme's When Revealed as a boost" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- reveal (player1 tg) Schemes.hood24056
        resolveBoost Treacheries.hood24059
        damageOf (player1 tg) `shouldReturn` 6

  describe "Standard II and Expert II" do
    it "logs an attack with one steady stun card and a canceled attack with two" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.hood24065
        asHero (player1 tg)
        _ <- reveal (player1 tg) Environments.hood24063
        void $ setEncounterDeck [Attachments.theHoodsMantle,Minions.hydraSoldier] []
        apply $ L.giveStatus mid StunnedStatus
        liftIO $ writeIORef (logged tg) []
        runNow [MinionMessage mid (MinionAttacksWithChange (player1 tg) 0 [])]
        settleEffects
        first <- liftIO $ T.concat . map entryText <$> readIORef (logged tg)
        first `shouldSatisfy` T.isInfixOf "$log.attacks"
        first `shouldSatisfy` not . T.isInfixOf "$log.stunnedInstead"
        apply $ L.giveStatus mid StunnedStatus
        liftIO $ writeIORef (logged tg) []
        runNow [MinionMessage mid (MinionAttacks $ player1 tg)]
        settleEffects
        second <- liftIO $ T.concat . map entryText <$> readIORef (logged tg)
        second `shouldSatisfy` T.isInfixOf "$log.stunnedInstead"
    it "Cruel Intentions deals each player an encounter card and gives the villain a boost" do
      tg <- testGame ["spider-man","captain-marvel"]
      inGame tg do
        blankDeck
        _ <- reveal (player1 tg) Treacheries.hood24029
        length <$> dealtTo (player1 tg) `shouldReturn` 2
        length <$> dealtTo (player2 tg) `shouldReturn` 1
        length . Villain.villainBoostCards . toAttrs <$> getVillain `shouldReturn` 1
    it "Ruination reveals a side scheme and adds threat to each scheme" do
      tg <- testGame ["spider-man"]
      inGame tg do
        void $ setEncounterDeck [Schemes.bombScare,Minions.hydraSoldier] []
        before <- mainSchemeThreatNow
        _ <- reveal (player1 tg) Treacheries.hood24030
        mainSchemeThreatNow `shouldReturn` before + 3
        sid <- selectJust (SideSchemeIs Schemes.bombScare)
        sideSchemeThreat sid `shouldReturn` 5
    it "Seek and Destroy puts the nemesis minion into play without revealing the rest" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- reveal (player1 tg) Treacheries.hood24031
        selectList (MinionIs Minions.vulture) >>= (`shouldSatisfy` not . null)
    it "Slug It Out exhausts the identity and deals two damage" do
      tg <- testGame ["spider-man"]
      inGame tg do
        blankDeck
        _ <- reveal (player1 tg) Treacheries.hood24032
        exhaustedNow (player1 tg) `shouldReturn` True
        damageOf (player1 tg) `shouldReturn` 2
    it "Dark Dealings schemes with one additional SCH" do
      tg <- testGame ["spider-man"]
      inGame tg do
        void $ setEncounterDeck [Attachments.theHoodsMantle,Minions.hydraSoldier] []
        before <- mainSchemeThreatNow
        _ <- reveal (player1 tg) Treacheries.hood24050
        mainSchemeThreatNow `shouldReturn` before + 2
    it "Mob Mentality discards seven cards and puts the first discarded minion into play" do
      tg <- testGame ["spider-man"]
      inGame tg do
        void $ setEncounterDeck (Treacheries.advance : Minions.hood24016 : replicate 10 Treacheries.advance) []
        _ <- reveal (player1 tg) Treacheries.hood24051
        selectList (MinionIs Minions.hood24016) >>= (`shouldSatisfy` not . null)
        characterMatches (CharacterWithStatus ConfusedStatus) (IdentityCharacter $ player1 tg) `shouldReturn` False
    it "Overwhelming Force discards the highest-cost upgrade or support" do
      tg <- testGame ["spider-man"]
      inGame tg do
        low <- putIntoPlay (player1 tg) "01081"
        high <- putIntoPlay (player1 tg) "01093"
        _ <- reveal (player1 tg) Treacheries.hood24052
        discardOf (player1 tg) >>= (`shouldSatisfy` elem high)
        discardOf (player1 tg) >>= (`shouldSatisfy` not . elem low)
    it "Shadow of the Past reveals the nemesis minion and scheme" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- reveal (player1 tg) Treacheries.hood24053
        selectList (MinionIs Minions.vulture) >>= (`shouldSatisfy` not . null)
        selectList (SideSchemeIs Schemes.highwayRobbery) >>= (`shouldSatisfy` not . null)
    it "Total Annihilation attacks a hero and gains surge" do
      tg <- testGame ["spider-man"]
      inGame tg do
        asHero (player1 tg)
        void $ setEncounterDeck [Attachments.theHoodsMantle,Minions.hydraMercenary,Minions.hydraSoldier] []
        _ <- reveal (player1 tg) Treacheries.hood24054
        damageOf (player1 tg) `shouldReturn` 2
        length <$> dealtTo (player1 tg) `shouldReturn` 1
