module Marvel.Encounter.AgeOfApocalypseApocalypseSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Hp (unHp)
import Marvel.Keyword
import Marvel.MainScheme.Types qualified as Main
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Modifier
import Marvel.Query
import Marvel.Scenario.AgeOfApocalypseApocalypse
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Trait qualified as Trait
import Marvel.Stats (Sch (..))
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import Marvel.Ability qualified as Ability
import TestImport

apocalypseGame :: Difficulty -> [Text] -> IO TestGame
apocalypseGame mode heroes = do
  (application, trace, logs) <- dealtGameLogged (ScenarioSetup "45101b" [] mode) (map starter heroes) 1
  seating <- runGameApp application (getsGame gameSeats)
  let tg = TestGame application trace logs seating
  inGame tg toFirstTurn
  pure tg

lifted :: L.QueueT GameAppT () -> GameAppT ()
lifted action = resolve =<< L.runQueueT (L.capture action)

current :: GameAppT VillainId
current = maybe (error "Apocalypse missing") pure =<< apocalypse

hp :: GameAppT Int
hp = unHp . Villain.villainHp . toAttrs <$> getVillain

villainCodeNow :: GameAppT CardCode
villainCodeNow = toCardCode . getCardDef <$> getVillain

clearPrelates :: GameAppT ()
clearPrelates = selectList (MinionWithTrait Trait.Prelate) >>= traverse_ (\mid -> resolve [RemoveFromPlay $ MinionRef mid])

clearTough :: GameAppT ()
clearTough = do
  vid <- current
  resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 1 FromAbility]

attachmentIdOf :: EncounterCard -> AttachmentId
attachmentIdOf = AttachmentId . unCardId . ecCardId

minionAttrs :: MinionId -> GameAppT (Attrs Minion)
minionAttrs mid = getsGame $ toAttrs . fromMaybe (error "minion missing") . HashMap.lookup mid . gameMinions

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> settleForced >> clearHand p

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
spec = describe "Age of Apocalypse Apocalypse" do
  for_ [(Normal, "45101b", 9), (Expert, "45102a", 10)] $ \(mode, expected, scalar) ->
    it ("sets up correct stage, random Prelate and Heart of the Empire: " <> show mode) do
      tg <- apocalypseGame mode ["spider-man", "she-hulk"]
      inGame tg do
        villainCodeNow `shouldReturn` expected
        apocalypseScalarHp `shouldReturn` scalar
        hp `shouldReturn` fromIntegral (2 * scalar)
        prelates <- selectList $ MinionWithTrait Trait.Prelate
        length prelates `shouldBe` 1
        attrs <- minionAttrs $ testHead prelates
        minionEngagedIdentity attrs `shouldBe` player1 tg
        Minion.getModifiedHitPoints attrs `shouldReturn` 10
        sid <- selectJust $ SideSchemeIs Schemes.heartOfTheEmpire
        sideSchemeThreat sid `shouldReturn` 2
        scheme <- getsGame $ toAttrs . testHead . HashMap.elems . gameMainSchemes
        Main.getThreshold scheme `shouldReturn` scalar
        cards <- scenarioEncounterDeckNow
        any (cardMatch $ CardWithTrait Trait.Prelate) cards `shouldBe` False

  it "advances on main-scheme completion, clearing all threat and revealing the next stage" do
    tg <- apocalypseGame Normal ["spider-man"]
    inGame tg do
      placeMainThreat 8
      settleForced
      villainCodeNow `shouldReturn` "45102a"
      mainSchemeThreatNow `shouldReturn` 0
      hp `shouldReturn` 10
      Villain.villainTough . toAttrs <$> getVillain `shouldReturn` True

  it "continues through stage IV and loses when that stage's scheme completes" do
    tg <- apocalypseGame Expert ["spider-man"]
    inGame tg do
      placeMainThreat 9
      settleForced
      villainCodeNow `shouldReturn` "45102b"
      placeMainThreat 11
      settleForced
      getsGame gameState `shouldReturn` Finished Lost

  it "would-defeat without No Longer Worthy heals all damage, discards attachments, and removes scalar threat" do
    tg <- apocalypseGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      vid <- current
      clearTough
      _ <- revealNow p Attachments.cyberpathy
      settleForced
      placeMainThreat 5
      before <- mainSchemeThreatNow
      resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 99 FromAbility]
      hp `shouldReturn` 9
      mainSchemeThreatNow `shouldReturn` 0
      (before > 0) `shouldBe` True
      villainCodeNow `shouldReturn` "45101b"
      HashMap.size <$> getsGame gameAttachments `shouldReturn` 0
      getsGame gameState `shouldReturn` InProgress

  it "No Longer Worthy heals five per player and prevents damage while a Prelate remains" do
    tg <- apocalypseGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      vid <- current
      clearTough
      resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 7 FromAbility]
      _ <- revealNow p Attachments.noLongerWorthy
      settleForced
      hp `shouldReturn` 7
      resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 4 FromAbility]
      hp `shouldReturn` 7
      clearPrelates
      resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 99 FromAbility]
      getsGame gameState `shouldReturn` Finished Won

  it "Heart of the Empire is locked by a Prelate and flips into the same physical Citadel when defeated" do
    tg <- apocalypseGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      asHero p
      sid <- selectJust $ SideSchemeIs Schemes.heartOfTheEmpire
      resolve [SideSchemeMessage sid $ SideSchemeThwarted (IdentityRef p) 2]
      sideSchemeThreat sid `shouldReturn` 2
      clearPrelates
      resolve [SideSchemeMessage sid $ SideSchemeThreatRemoved GameRef 1]
      choose "thwart Heart" (isHeroThwart p)
      choose "Heart" (isTargetChoice sid)
      settleForced
      selectOne (SideSchemeIs Schemes.theToweringCitadel) `shouldReturn` Just sid
      sideSchemeThreat sid `shouldReturn` 3
      length <$> selectList (MinionWithTrait Trait.Prelate) `shouldReturn` 1

  it "The Towering Citadel reveals The Tyrant's Throne and removes itself from the game" do
    tg <- apocalypseGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      clearPrelates
      sid <- sideSchemeOf <$> revealNow p Schemes.theToweringCitadel
      resolve [SideSchemeMessage sid $ SideSchemeThwarted (IdentityRef p) 3]
      settleForced
      selectOne (SideSchemeIs Schemes.theToweringCitadel) `shouldReturn` Nothing
      selectAny (SideSchemeIs Schemes.theTyrantsThrone) `shouldReturn` True

  it "The Tyrant's Throne transforms into No Longer Worthy with the same card UUID" do
    tg <- apocalypseGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      clearPrelates
      throne <- revealNow p Schemes.theTyrantsThrone
      let sid = sideSchemeOf throne
      resolve [SideSchemeMessage sid $ SideSchemeThwarted (IdentityRef p) 4]
      settleForced
      selectAny (AttachmentIs Attachments.noLongerWorthy) `shouldReturn` True
      attrs <- getsGame $ toAttrs . fromMaybe (error "No Longer Worthy missing") . HashMap.lookup (attachmentIdOf throne) . gameAttachments
      Attachment.attachmentId attrs `shouldBe` attachmentIdOf throne
      selectOne (SideSchemeIs Schemes.theTyrantsThrone) `shouldReturn` Nothing

  it "Cyberpathy places one threat on each side scheme after Apocalypse schemes" do
    tg <- apocalypseGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      attached <- revealNow p Attachments.cyberpathy
      settleForced
      sid <- selectJust $ SideSchemeIs Schemes.heartOfTheEmpire
      before <- sideSchemeThreat sid
      vid <- current
      host <- getsGame $ join . fmap (Attachment.attachmentEnemy . toAttrs) . HashMap.lookup (attachmentIdOf attached) . gameAttachments
      host `shouldBe` Just (EnemyVillainId vid)
      schemeBefore <- mainSchemeThreatNow
      _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance] []
      lifted $ L.villainSchemesAgainst vid p
      mainSchemeThreatNow `shouldReturn` schemeBefore + 3
      sideSchemeThreat sid `shouldReturn` before + 1

  for_ [(Attachments.biomorphing, KeywordModifier Overkill), (Attachments.molecularControl, KeywordModifier Stalwart)] $ \(def, modifier) ->
    it ("grants its printed combat keywords: " <> show (toCardCode def)) do
      tg <- apocalypseGame Normal ["spider-man"]
      inGame tg do
        _ <- revealNow (player1 tg) def
        settleForced
        vid <- current
        getModifiers (VillainRef vid) >>= (`shouldSatisfy` elem modifier)

  for_ [Attachments.cyberpathy, Attachments.biomorphing, Attachments.molecularControl] $ \def ->
    it ("attaches its boost card to Apocalypse rather than discarding it: " <> show (toCardCode def)) do
      tg <- apocalypseGame Normal ["spider-man"]
      inGame tg do
        card <- encounterCard def
        vid <- current
        resolve [RevealBoostCard card (EnemyVillainId vid)]
        exists <- getsGame $ HashMap.member (attachmentIdOf card) . gameAttachments
        exists `shouldBe` True

  it "The Apocalypse Solution discards the stage's scalar numeral of encounter cards" do
    tg <- apocalypseGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      sid <- sideSchemeOf <$> revealNow p Schemes.theApocalypseSolution
      _ <- setEncounterDeck (replicate 12 Treacheries.advance) []
      resolve [SideSchemeMessage sid $ SideSchemeThwarted (IdentityRef p) 3]
      length <$> scenarioEncounterDeckNow `shouldReturn` 3

  it "The Fittest selects the highest printed hit points, gives tough, and adds five HP" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      small <- addMinion p Minions.hydraMercenary
      large <- addMinion p Minions.barrage
      card <- revealNow p Attachments.theFittest
      offers (isTargetChoice small) `shouldReturn` False
      choose "highest HP minion" (isTargetChoice large)
      settleForced
      attrs <- minionAttrs large
      Minion.getModifiedHitPoints attrs `shouldReturn` 9
      minionTough attrs `shouldBe` True
      host <- getsGame $ join . fmap (Attachment.attachmentEnemy . toAttrs) . HashMap.lookup (attachmentIdOf card) . gameAttachments
      host `shouldBe` Just (EnemyMinionId large)

  it "Wolf Among Sheep activates a Prelate instead of Apocalypse" do
    tg <- apocalypseGame Normal ["spider-man"]
    inGame tg do
      let p = player1 tg
      prelate <- testHead <$> selectList (MinionWithTrait Trait.Prelate)
      attrs <- minionAttrs prelate
      before <- mainSchemeThreatNow
      _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance] []
      _ <- revealNow p Treacheries.wolfAmongSheep
      settleForced
      after <- mainSchemeThreatNow
      -- Mister Sinister is villainous and receives the zero-icon boost fixture;
      -- every other Prelate uses only its printed scheme power.
      after `shouldBe` before + unSch (minionScheme attrs)

  describe "Prelates" do
    it "all five have five HP per player and engage the first player regardless of who reveals them" do
      for_ [Minions.misterSinisterOverseer, Minions.theShadowKingOverseer, Minions.abyssOverseer, Minions.sugarManOverseer, Minions.mikhailRasputinOverseer] $ \def -> do
        tg <- testGame ["spider-man", "she-hulk"]
        inGame tg do
          card <- revealNow (player2 tg) def
          settleForced
          attrs <- minionAttrs $ minionOf card
          minionEngagedIdentity attrs `shouldBe` player1 tg
          Minion.getModifiedHitPoints attrs `shouldReturn` 10
          resolve [PassFirstPlayer]
          minionEngagedIdentity <$> minionAttrs (minionOf card) `shouldReturn` player2 tg

    it "Abyss attaches the top deck card facedown after activating, gaining two HP" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        abyss <- addMinion p Minions.abyssOverseer
        top <- testHead <$> stackDeck p ["01088"]
        runNow [MinionMessage abyss MinionSchemes]
        settleForced
        attrs <- minionAttrs abyss
        minionHeldCards attrs `shouldBe` [top]
        Minion.getModifiedHitPoints attrs `shouldReturn` 7

    it "Mikhail Rasputin deals one nonattack damage when initiating an attack" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        minion <- addMinion p Minions.mikhailRasputinOverseer
        runNow [MinionMessage minion $ MinionAttacks p]
        settleForced
        damageOf p `shouldReturn` 3

    it "Sugar Man's attack gains piercing and heals five after defeating a character" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        sugar <- addMinion p Minions.sugarManOverseer
        resolve [MinionMessage sugar $ MinionDamaged GameRef $ toDamage 1 FromAbility,
          MinionMessage sugar $ MinionDamaged GameRef $ toDamage 4 FromAbility]
        ally <- putIntoPlay p "01002"
        runNow [MinionMessage sugar $ MinionAttacks p]
        choose "Black Cat defends" \case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False
        settleForced
        minionDamage <$> minionAttrs sugar `shouldReturn` 0
        getModifiers (MinionRef sugar) >>= (`shouldSatisfy` elem (KeywordModifier Piercing))

    it "Shadow King selects the ally with highest THW and offers discarding it or placing its THW as threat" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        _ <- putIntoPlay p "01002"
        cable <- putIntoPlay p "45011"
        king <- addMinion p Minions.theShadowKingOverseer
        before <- mainSchemeThreatNow
        runNow [MinionMessage king $ MinionAttacks p]
        settleForced
        choose "Cable has highest THW" (isTargetChoice $ allyIdOf cable)
        choose "place two threat" (isLabelStarting "$label.aoaPlaceMainThreat")
        settleForced
        mainSchemeThreatNow `shouldReturn` before + 2

  describe "Dark Riders" do
    for_ [(Minions.gauntlet, "45112" :: CardCode), (Minions.barrage, "45113"), (Minions.hardDrive, "45114"), (Minions.tusk, "45115"), (Minions.psynapse, "45116")] $ \(def, riderCode) ->
      it ("resolves its printed response after attacking: " <> show riderCode) do
        tg <- testGame ["spider-man"]
        inGame tg do
          let p = player1 tg
          asHero p
          upgrade <- putIntoPlay p "01008"
          _ <- putIntoPlay p "01002"
          rider <- addMinion p def
          before <- mainSchemeThreatNow
          runNow [MinionMessage rider $ MinionAttacks p]
          settleForced
          when (riderCode == "45112") $ choose "discard upgrade" (isTargetChoice $ upgradeIdOf upgrade)
          settleForced
          case riderCode of
            "45112" -> length <$> upgradesWith "01008" `shouldReturn` 0
            "45113" -> allyDamage <$> allyIn "01002" `shouldReturn` 1
            "45114" -> mainSchemeThreatNow `shouldReturn` before + 1
            "45115" -> playerIdentityStunned <$> playerAttrs p `shouldReturn` True
            "45116" -> playerIdentityConfused <$> playerAttrs p `shouldReturn` True
            _ -> pure ()

    for_ [(Minions.tusk, True), (Minions.psynapse, False)] $ \(def, stunned) ->
      it ("resolves the rider boost's status: " <> show (toCardCode def)) do
        tg <- testGame ["spider-man"]
        inGame tg do
          card <- encounterCard def
          vid <- villainIdNow
          resolve [RevealBoostCard card $ EnemyVillainId vid]
          if stunned then playerIdentityStunned <$> playerAttrs (player1 tg) `shouldReturn` True
            else playerIdentityConfused <$> playerAttrs (player1 tg) `shouldReturn` True

    it "The Dark Riders side scheme adds hinder, finds a rider, and gives entering riders toughness" do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- setEncounterDeck [Treacheries.advance, Minions.gauntlet] []
        side <- sideSchemeOf <$> revealNow p Schemes.theDarkRiders
        settleForced
        sideSchemeThreat side `shouldReturn` 3
        rider <- selectJust $ MinionIs Minions.gauntlet
        minionTough <$> minionAttrs rider `shouldReturn` True

  it "The Fittest has only its two boost icons and never attaches as a boost" do
    tg <- apocalypseGame Normal ["spider-man"]
    inGame tg do
      card <- encounterCard Attachments.theFittest
      vid <- current
      resolve [RevealBoostCard card $ EnemyVillainId vid]
      selectAny (AttachmentIs Attachments.theFittest) `shouldReturn` False
