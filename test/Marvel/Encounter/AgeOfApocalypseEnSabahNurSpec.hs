module Marvel.Encounter.AgeOfApocalypseEnSabahNurSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types ()
import Marvel.Hp (unHp)
import Marvel.Keyword
import Marvel.MainScheme.Types qualified as Main
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types ()
import Marvel.Modifier
import Marvel.Query
import Marvel.Scenario.AgeOfApocalypseEnSabahNur
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Stats ()
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import Marvel.Ability qualified as Ability
import TestImport

ensGame :: Difficulty -> [Text] -> IO TestGame
ensGame mode heroes = do
  (application, trace, logs) <- dealtGameLogged (ScenarioSetup "45184a" [] mode) (map starter heroes) 1
  seating <- runGameApp application (getsGame gameSeats)
  let tg = TestGame application trace logs seating
  inGame tg toFirstTurn
  pure tg

lifted :: L.QueueT GameAppT () -> GameAppT ()
lifted action = resolve =<< L.runQueueT (L.capture action)

current :: GameAppT VillainId
current = maybe (error "Apocalypse missing") pure =<< enSabahNur

hp :: GameAppT Int
hp = unHp . Villain.villainHp . toAttrs <$> getVillain

villainCodeNow :: GameAppT CardCode
villainCodeNow = toCardCode . getCardDef <$> getVillain

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> settleForced >> clearHand p

attachmentIdOf :: EncounterCard -> AttachmentId
attachmentIdOf = AttachmentId . unCardId . ecCardId

attachmentExists :: EncounterCard -> GameAppT Bool
attachmentExists card = getsGame $ HashMap.member (attachmentIdOf card) . gameAttachments

-- Resolve mandatory ability windows while leaving real target/resource choices
-- for the test to answer explicitly. TestImport.pass only declines optional ones.
settleForced :: GameAppT ()
settleForced = do
  pass
  question <- asked
  case question >>= (headMay . filter forced . snd) of
    Just choice -> choose "resolve mandatory ability" (== choice) >> settleForced
    Nothing -> pure ()
 where
  forced = \case
    UseAbility ab -> abilityType ab `elem` [Ability.ForcedInterrupt, Ability.ForcedResponse, Ability.WhenRevealed]
    _ -> False


spec :: Spec
spec = describe "Age of Apocalypse En Sabah Nur" do
  for_ [(Normal, "45184a", 16), (Expert, "45185a", 20)] $ \(mode, expected, health) ->
    it ("starts in Biomorph with the appropriate stage and one encounter card per player: " <> show mode) do
      tg <- ensGame mode ["spider-man", "she-hulk"]
      inGame tg do
        villainCodeNow `shouldReturn` expected
        hp `shouldReturn` 2 * health
        length <$> dealtTo (player1 tg) `shouldReturn` 1
        length <$> dealtTo (player2 tg) `shouldReturn` 1
        apocalypseInForm "Biomorph" `shouldReturn` True
        vid <- current
        getModifiers (VillainRef vid) >>= (`shouldSatisfy` elem (KeywordModifier Overkill))

  it "changes forms without replacing the entity, resetting HP or losing attachments and tough" do
    tg <- ensGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      vid <- current
      resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 3 FromAbility, GiveStatus (VillainRef vid) ToughStatus]
      card <- revealNow p Attachments.celestialWeapon
      settleForced
      before <- mainSchemeThreatNow
      lifted $ changeApocalypseForm "Cyberpath"
      current `shouldReturn` vid
      villainCodeNow `shouldReturn` "45184b"
      hp `shouldReturn` 13
      attachmentExists card `shouldReturn` True
      Villain.villainTough . toAttrs <$> getVillain `shouldReturn` True
      mainSchemeThreatNow `shouldReturn` before + 1

  it "changing to Giant heals its stage number, gains stalwart, and removes status cards" do
    tg <- ensGame Expert ["spider-man"]
    inGame tg do
      vid <- current
      resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 5 FromAbility,
        GiveStatus (VillainRef vid) StunnedStatus, GiveStatus (VillainRef vid) ConfusedStatus]
      lifted $ changeApocalypseForm "Giant"
      villainCodeNow `shouldReturn` "45185c"
      hp `shouldReturn` 17
      attrs <- toAttrs <$> getVillain
      Villain.villainStunned attrs `shouldBe` False
      Villain.villainConfused attrs `shouldBe` False

  it "changing to Biomorph deals indirect damage to each player and repeating the same form does nothing" do
    tg <- ensGame Normal ["spider-man", "she-hulk"]
    inGame tg do
      lifted $ changeApocalypseForm "Cyberpath"
      lifted $ changeApocalypseForm "Biomorph"
      damageOf (player1 tg) `shouldReturn` 1
      damageOf (player2 tg) `shouldReturn` 1
      lifted $ changeApocalypseForm "Biomorph"
      damageOf (player1 tg) `shouldReturn` 1

  it "defeating a nonfinal stage reveals the next Biomorph at full HP and keeps attachments, statuses and counters" do
    tg <- ensGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      vid <- current
      card <- revealNow p Attachments.celestialWeapon
      settleForced
      lifted $ changeApocalypseForm "Cyberpath"
      resolve [PlaceCounters (VillainRef vid) 3, GiveStatus (VillainRef vid) ConfusedStatus]
      resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 99 FromAbility]
      villainCodeNow `shouldReturn` "45185a"
      hp `shouldReturn` 20
      attachmentExists card `shouldReturn` True
      attrs <- toAttrs <$> getVillain
      Villain.villainConfused attrs `shouldBe` True
      Villain.villainCounters attrs `shouldBe` 3

  it "defeating the final stage wins" do
    tg <- ensGame Expert ["spider-man"]
    inGame tg do
      vid <- current
      resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 99 FromAbility]
      villainCodeNow `shouldReturn` "45186a"
      resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 99 FromAbility]
      getsGame gameState `shouldReturn` Finished Won

  for_ [(Treacheries.biomorphicBlast, "Biomorph"), (Treacheries.technologicalInterface, "Cyberpath"), (Treacheries.giantSizedDespot, "Giant")] $ \(def, form) ->
    it ("the form treachery changes to " <> show form <> " and places a power counter, or activates when already there") do
      tg <- ensGame Normal ["spider-man"]
      inGame tg do
        let p = player1 tg
        when (form == "Biomorph") $ lifted $ changeApocalypseForm "Cyberpath"
        _ <- revealNow p def
        settleForced
        apocalypseInForm form `shouldReturn` True
        counters <- Main.mainSchemeCounters . toAttrs . testHead . HashMap.elems <$> getsGame gameMainSchemes
        counters `shouldBe` 1
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance] []
        before <- mainSchemeThreatNow
        _ <- revealNow p def
        settleForced
        after <- mainSchemeThreatNow
        (after > before) `shouldBe` True

  for_ [(Schemes.sourceOfPower, "Biomorph"), (Schemes.pluggedIn, "Cyberpath"), (Schemes.giantGrowth, "Giant")] $ \(def, form) ->
    it ("the defeated Superpower scheme changes to " <> show form <> " and gives tough") do
      tg <- ensGame Normal ["spider-man"]
      inGame tg do
        let p = player1 tg
        when (form == "Biomorph") $ lifted $ changeApocalypseForm "Cyberpath"
        side <- sideSchemeOf <$> revealNow p def
        resolve [SideSchemeMessage side $ SideSchemeThwarted (IdentityRef p) 5]
        settleForced
        apocalypseInForm form `shouldReturn` True
        Villain.villainTough . toAttrs <$> getVillain `shouldReturn` True

  it "Staggering Strength changes to Giant and is discarded after its stunning attack activation" do
    tg <- ensGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      asHero p
      card <- revealNow p Attachments.staggeringStrength
      settleForced
      apocalypseInForm "Giant" `shouldReturn` True
      vid <- current
      _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance] []
      runNow [VillainMessage vid $ VillainAttacks p]
      settleForced
      playerIdentityStunned <$> playerAttrs p `shouldReturn` True
      attachmentExists card `shouldReturn` False

  it "the Pyramid resolves its four-power threshold by discarding until a Superpower and revealing it" do
    tg <- ensGame Normal ["spider-man"]
    inGame tg do
      sid <- mainSchemeIdNow
      resolve [PlaceCounters (MainSchemeRef sid) 4]
      _ <- setEncounterDeck [Treacheries.advance, Treacheries.technologicalInterface, Treacheries.advance] []
      lifted $ L.cardSpecific sid "checkPowerCounters" ()
      apocalypseInForm "Cyberpath" `shouldReturn` True
      -- Technological Interface places one new power counter after the four were spent.
      counters <- Main.mainSchemeCounters . toAttrs . testHead . HashMap.elems <$> getsGame gameMainSchemes
      counters `shouldBe` 1

  describe "Celestial Tech" do
    for_ [("01088", True, False, False), ("01089", False, True, False), ("01090", False, False, True), ("01072", True, True, True)] $ \(top, healing, confused, tough) ->
      it ("Celestial Armor applies each printed resource result: " <> show top) do
        tg <- testGame ["spider-man"]
        inGame tg do
          let p = player1 tg
          vid <- villainIdNow
          resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 4 FromAbility]
          card <- revealNow p Attachments.celestialArmor
          settleForced
          _ <- stackDeck p [top]
          _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance] []
          lifted $ L.villainSchemesAgainst vid p
          villainDamageNow `shouldReturn` if healing then 2 else 4
          playerIdentityConfused <$> playerAttrs p `shouldReturn` confused
          Villain.villainTough . toAttrs <$> getVillain `shouldReturn` tough
          attachmentExists card `shouldReturn` not confused

    for_ [("01088", 4, False, True), ("01089", 2, False, False), ("01090", 2, True, True), ("01072", 4, True, False)] $ \(top, damage, stunned, keptHand) ->
      it ("Celestial Weapon applies each printed resource result: " <> show top) do
        tg <- testGame ["spider-man"]
        inGame tg do
          let p = player1 tg
          asHero p
          handCard <- putInHand p "01083"
          card <- revealNow p Attachments.celestialWeapon
          settleForced
          _ <- stackDeck p [top]
          _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance] []
          vid <- villainIdNow
          runNow [VillainMessage vid $ VillainAttacks p]
          settleForced
          when (top `elem` ["01089", "01072"]) $ chooseIf (isTargetChoice handCard)
          settleForced
          damageOf p `shouldReturn` damage
          playerIdentityStunned <$> playerAttrs p `shouldReturn` stunned
          length <$> handOf p `shouldReturn` if keptHand then 1 else 0
          attachmentExists card `shouldReturn` not stunned

    it "Celestial Tech searches and reveals an attachment if none is attached" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- setEncounterDeck [Attachments.celestialArmor, Treacheries.advance] []
        _ <- revealNow p Treacheries.celestialTech
        chooseIf (isLabel "$label.done")
        settleForced
        selectAny (AttachmentIs Attachments.celestialArmor) `shouldReturn` True

  describe "Clan Akkaba" do
    it "Ancient Ritual removes five threat and deals every player an encounter card at ten threat" do
      tg <- testGame ["spider-man", "she-hulk"]
      inGame tg do
        side <- sideSchemeOf <$> revealNow (player1 tg) Schemes.ancientRitual
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance, Treacheries.advance] []
        lifted $ placeAncientRitualThreat 5
        sideSchemeThreat side `shouldReturn` 5
        length <$> dealtTo (player1 tg) `shouldReturn` 1
        length <$> dealtTo (player2 tg) `shouldReturn` 1

    it "Ozymandias schemes onto Ancient Ritual, including his villainous boost" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        side <- sideSchemeOf <$> revealNow p Schemes.ancientRitual
        minion <- addMinion p Minions.ozymandias
        before <- mainSchemeThreatNow
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance] []
        runNow [MinionMessage minion MinionSchemes]
        settleForced
        sideSchemeThreat side `shouldReturn` 6
        mainSchemeThreatNow `shouldReturn` before

    it "Scarab adds one threat after an attack that did not defeat an ally" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        side <- sideSchemeOf <$> revealNow p Schemes.ancientRitual
        minion <- addMinion p Minions.scarab
        -- addMinion resolves its quickstrike before returning.
        before <- sideSchemeThreat side
        runNow [MinionMessage minion $ MinionAttacks p]
        settleForced
        sideSchemeThreat side `shouldReturn` before + 1

    it "Scarab adds three threat instead when his attack defeats an ally" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        side <- sideSchemeOf <$> revealNow p Schemes.ancientRitual
        minion <- addMinion p Minions.scarab
        before <- sideSchemeThreat side
        ally <- putIntoPlay p "01002"
        runNow [MinionMessage minion $ MinionAttacks p]
        choose "Black Cat defends" \case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False
        settleForced
        sideSchemeThreat side `shouldReturn` before + 3

    it "defeating a Clan Akkaba Zealot places two threat on Ancient Ritual" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        side <- sideSchemeOf <$> revealNow p Schemes.ancientRitual
        minion <- addMinion p Minions.clanAkkabaZealot
        resolve [MinionMessage minion $ MinionDamaged GameRef $ toDamage 3 FromAbility]
        sideSchemeThreat side `shouldReturn` 7

    it "Zealot's boost places one threat on Ancient Ritual" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        side <- sideSchemeOf <$> revealNow p Schemes.ancientRitual
        card <- encounterCard Minions.clanAkkabaZealot
        vid <- villainIdNow
        resolve [RevealBoostCard card $ EnemyVillainId vid]
        sideSchemeThreat side `shouldReturn` 6

    it "Tyrant Worship places five threat and triggers the ritual encounter cards" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        side <- sideSchemeOf <$> revealNow p Schemes.ancientRitual
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance] []
        _ <- revealNow p Treacheries.tyrantWorship
        settleForced
        sideSchemeThreat side `shouldReturn` 5
        length <$> dealtTo p `shouldReturn` 1

    for_ [Minions.ozymandias, Treacheries.tyrantWorship] $ \def ->
      it ("the choice boost can put three threat on Ancient Ritual: " <> show (toCardCode def)) do
        tg <- testGame ["spider-man"]
        inGame tg do
          let p = player1 tg
          side <- sideSchemeOf <$> revealNow p Schemes.ancientRitual
          card <- encounterCard def
          vid <- villainIdNow
          runNow [RevealBoostCard card $ EnemyVillainId vid]
          choose "three threat" (isLabelStarting "$label.aoaPlaceAncientRitualThreat")
          settleForced
          sideSchemeThreat side `shouldReturn` 8

    for_ [Minions.ozymandias, Treacheries.tyrantWorship] $ \def ->
      it ("the choice boost can gain three actual boost icons instead: " <> show (toCardCode def)) do
        tg <- testGame ["spider-man"]
        inGame tg do
          let p = player1 tg
          asHero p
          _ <- setEncounterDeck [def] [Treacheries.advance]
          vid <- villainIdNow
          runNow [VillainMessage vid $ VillainAttacks p]
          settleForced
          choose "boost icons" (isLabel "$label.aoaThreeBoostIcons")
          settleForced
          damageOf p `shouldReturn` 5

  it "Staggering Strength stuns the controller when Apocalypse directly attacks their ally" do
    tg <- ensGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      asHero p
      ally <- putIntoPlay p "01002"
      card <- revealNow p Attachments.staggeringStrength
      settleForced
      vid <- current
      _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance] []
      runNow [VillainMessage vid $ VillainAttacksAlly p $ allyIdOf ally]
      settleForced
      playerIdentityStunned <$> playerAttrs p `shouldReturn` True
      attachmentExists card `shouldReturn` False

  it "a non-thwart scheme defeat activates against its actual source controller while another player is active" do
    tg <- ensGame Normal ["spider-man", "she-hulk"]
    inGame tg do
      let p = player1 tg
          defeatingPlayer = player2 tg
      asHero p
      scheme <- sideSchemeOf <$> revealNow p Schemes.sourceOfPower
      before <- mainSchemeThreatNow
      _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance] []
      runNow [SideSchemeMessage scheme $ SideSchemeThreatRemoved (IdentityRef defeatingPlayer) 5]
      settleForced
      -- The defeating She-Hulk player is in alter ego, so Apocalypse schemes;
      -- the active Spider-Man hero is not attacked instead.
      mainSchemeThreatNow `shouldReturn` before + 2
      damageOf p `shouldReturn` 0
