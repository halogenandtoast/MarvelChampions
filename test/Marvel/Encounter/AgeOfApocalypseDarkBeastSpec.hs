module Marvel.Encounter.AgeOfApocalypseDarkBeastSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.Maybe (maybeToList)
import Data.Text qualified as Text
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.AgeOfApocalypse.Settings
import Marvel.Ability qualified as Ability
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.EncounterSet qualified as Set
import Marvel.Environment.Cards qualified as Environments
import Marvel.Keyword hiding (Surge)
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Query
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Status
import Marvel.Trait (Trait (Setting))
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import TestImport

darkBeast :: Difficulty -> IO TestGame
darkBeast mode = do
  (application, trace, logs) <- dealtGameLogged (ScenarioSetup "45118" [] mode) [starter "iron-man"] 1
  seating <- runGameApp application (getsGame gameSeats)
  let tg = TestGame application trace logs seating
  inGame tg toFirstTurn
  pure tg

perform :: L.QueueT GameAppT () -> GameAppT ()
perform body = resolve =<< L.runQueueT (L.capture body)

setting :: IdentityId -> CardDef -> GameAppT ()
setting p def = void (revealNow p def) >> pass

settingCode :: GameAppT (Maybe CardCode)
settingCode = fmap (toCardCode . snd) <$> currentSetting

attachmentIdOf :: EncounterCard -> AttachmentId
attachmentIdOf = AttachmentId . unCardId . ecCardId

attachmentHost :: EncounterCard -> GameAppT (Maybe EnemyId)
attachmentHost card = getsGame (join . fmap (Attachment.attachmentEnemy . toAttrs) . HashMap.lookup (attachmentIdOf card) . gameAttachments)

boostNow :: CardDef -> GameAppT EncounterCard
boostNow def = do
  card <- encounterCard def
  villain <- villainIdNow
  resolve [RevealBoostCard card (EnemyVillainId villain)]
  pure card

newtype SettingTestEffect = SettingTestEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage SettingTestEffect where
  runMessage msg (SettingTestEffect attrs) = SettingTestEffect <$> runMessage msg attrs

blankSetting :: GameAppT ()
blankSetting = do
  found <- currentSetting
  for_ found \(environment, _) -> do
    eid <- getRandom
    withGame_ $ entitiesL . effectsL . at eid ?~ Effect (SettingTestEffect EffectAttrs
      {effectId = eid, effectCardCode = "45127", effectSource = GameRef
      ,effectMatcher = SourceEntity (EnvironmentRef environment), effectModifiers = [Blank], effectEnds = Nothing})

spec :: Spec
spec = describe "Age of Apocalypse Dark Beast" do
  for_ [Environments.theSavageLand, Environments.genosha, Environments.blueAreaOfTheMoon] \def ->
    it ("waits for a click before resolving " <> show (toCardCode def) <> "'s Special") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        setting p def
        Just (eid, _) <- currentSetting
        beforeThreat <- mainSchemeThreatNow
        beforeDamage <- damageOf p
        beforeDiscard <- discardOf p
        waiting <- openQuestion
        withGame_ $ questionL .~ mempty
        msgs <- L.runQueueT $ L.capture $ resolveSettingSpecial p
        pushAll $ msgs <> [Ask ident q | Just (ident, q) <- [waiting]]
        runGameMessages
        mainSchemeThreatNow `shouldReturn` beforeThreat
        damageOf p `shouldReturn` beforeDamage
        discardOf p `shouldReturn` beforeDiscard
        openQuestion >>= \case
          Just (ident, QuestionLabel "$label.resolveSpecial" (Just target) q) -> do
            liftIO $ target `shouldBe` EnvironmentRef eid
            liftIO $ choicesOf q `shouldSatisfy` any (isTargetChoice $ EnvironmentRef eid)
            answerQuestion ident 0
          _ -> error "expected the Setting's Special prompt"
        pass
        case toCardCode def of
          "45127" -> length <$> discardOf p `shouldReturn` length beforeDiscard + 3
          "45133" -> mainSchemeThreatNow `shouldReturn` beforeThreat + 1
          "45139" -> damageOf p `shouldReturn` beforeDamage + 1
          _ -> error "unexpected Setting"

  for_ [(Normal, "45118"), (Expert, "45119")] \(mode, code) ->
    it ("sets up the correct villain, one random Setting and two unused settings in " <> show mode) do
      tg <- darkBeast mode
      inGame tg do
        toCardCode . getCardDef <$> getVillain `shouldReturn` code
        environments <- selectList (EnvironmentWithTrait Setting)
        length environments `shouldBe` 1
        aside <- getSetAsideEncounterCards
        length (filter ((== EnvironmentType) . cdCardType . getCardDef) aside) `shouldBe` 2
        present <- selectList (AttachmentIs Attachments.highTechGoggles)
        notNull present `shouldBe` (mode == Expert)
        let p = player1 tg
        dealt <- length . playerIdentityEncounterCards <$> playerAttrs p
        dealt `shouldBe` (if mode == Expert then 1 else 0)

  it "recruits an unused setting set when the villain advances and keeps the earlier cards" do
    tg <- darkBeast Normal
    inGame tg do
      beforeSetting <- settingCode
      deck <- getEncounterDeck
      discard <- getEncounterDiscard
      v <- villainIdNow
      perform $ L.dealDamage v GameRef (toDamage 15 FromAbility)
      pass
      toCardCode . getCardDef <$> getVillain `shouldReturn` "45119"
      afterSetting <- settingCode
      afterSetting `shouldSatisfy` (/= beforeSetting)
      aside <- getSetAsideEncounterCards
      length (filter ((== EnvironmentType) . cdCardType . getCardDef) aside) `shouldBe` 1
      later <- (<>) <$> getEncounterDeck <*> getEncounterDiscard
      let firstSet = [set | card <- deck <> discard, set <- maybeToList (cdEncounterSet $ getCardDef card), set `elem` [Set.AoaBlueMoon, Set.AoaGenosha, Set.AoaSavageLand]]
      later `shouldSatisfy` (\cards -> any (\card -> cdEncounterSet (getCardDef card) `elem` map Just firstSet) cards)

  it "Dark Beast resolves the attacked player's Setting Special before attacking" do
    tg <- darkBeast Normal
    inGame tg do
      let p = player1 tg
      changeForm p
      setting p Environments.genosha
      before <- mainSchemeThreatNow
      v <- villainIdNow
      resolve [VillainMessage v (VillainAttacks p)]
      mainSchemeThreatNow `shouldReturn` before + 1

  it "a stunned Dark Beast does not resolve his attack interrupt" do
    tg <- darkBeast Normal
    inGame tg do
      let p = player1 tg
      changeForm p
      setting p Environments.genosha
      v <- villainIdNow
      resolve [GiveStatus (VillainRef v) StunnedStatus, GiveStatus (VillainRef v) StunnedStatus]
      before <- mainSchemeThreatNow
      resolve [VillainMessage v (VillainAttacks p)]
      mainSchemeThreatNow `shouldReturn` before

  it "The Savage Land discards three actual cards from the acting player's deck" do
    tg <- testGame ["spider-man", "iron-man"]
    inGame tg do
      let p = player1 tg
          other = player2 tg
      setting p Environments.theSavageLand
      cards <- stackDeck other ["01086", "01087", "01093"]
      hand <- handOf p
      perform $ resolveSettingSpecial other
      discardOf other >>= (`shouldSatisfy` (\xs -> all (`elem` xs) cards))
      handOf p `shouldReturn` hand

  it "each newly revealed Setting replaces the previous one and changes its constant" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      v <- villainIdNow
      setting p Environments.theSavageLand
      getModifiers (VillainRef v) >>= (`shouldSatisfy` elem (KeywordModifier $ Retaliate 1))
      setting p Environments.genosha
      settingCode `shouldReturn` Just "45133"
      getModifiers (VillainRef v) >>= (`shouldSatisfy` (\ms -> KeywordModifier Steady `elem` ms && KeywordModifier (Retaliate 1) `notElem` ms))
      setting p Environments.blueAreaOfTheMoon
      mid <- addMinion p Minions.hydraRegular
      getEnemyKeywords (EnemyMinionId mid) >>= (`shouldSatisfy` elem Guard)
      length <$> selectList (EnvironmentWithTrait Setting) `shouldReturn` 1

  for_ [("45122" :: Text, Attachments.highTechGoggles), ("45123", Attachments.geneticEnhancement)] \(code, def) ->
    it ("an attachment " <> show code <> " exhausts the hero and resolves the current Special before discarding") do
      tg <- darkBeast Normal
      inGame tg do
        let p = player1 tg
        changeForm p
        setting p Environments.genosha
        card <- revealNow p def
        pass
        before <- mainSchemeThreatNow
        choose "discard through its hero action" (isAbilityOf $ attachmentIdOf card)
        pass
        isExhaustedPlayer p `shouldReturn` True
        mainSchemeThreatNow `shouldReturn` before + 1
        getsGame (HashMap.member (attachmentIdOf card) . gameAttachments) `shouldReturn` False

  it "High-Tech Goggles and Genetic Enhancement boosts attach their physical cards to Dark Beast" do
    tg <- darkBeast Normal
    inGame tg do
      v <- villainIdNow
      for_ [Attachments.highTechGoggles, Attachments.geneticEnhancement] \def -> do
        card <- boostNow def
        attachmentHost card `shouldReturn` Just (EnemyVillainId v)

  it "Cruel Experiment reveals the found minion and gives it its printed enhancements" do
    tg <- darkBeast Normal
    inGame tg do
      let p = player1 tg
      _ <- stackEncounterDeck [Treacheries.advance, Minions.hydraRegular]
      card <- revealNow p Attachments.cruelExperiment
      pass
      minions <- selectList (MinionIs Minions.hydraRegular)
      [mid] <- pure minions
      attachmentHost card `shouldReturn` Just (EnemyMinionId mid)
      getModifiers (MinionRef mid) >>= (`shouldSatisfy` (\ms -> all (`elem` ms) [AttackModifier 1, SchemeModifier 1, HitPointModifier 2, KeywordModifier Guard]))

  it "Evil Genius schemes in alter-ego form and grants tough" do
    tg <- darkBeast Normal
    inGame tg do
      let p = player1 tg
      before <- mainSchemeThreatNow
      card <- encounterCard Treacheries.evilGenius
      resolve [RevealEncounterCard p card]
      after <- mainSchemeThreatNow
      after `shouldSatisfy` (> before)
      Villain.villainTough . toAttrs <$> getVillain `shouldReturn` True

  it "Evil Genius attacks a hero with two boost cards" do
    tg <- darkBeast Normal
    inGame tg do
      let p = player1 tg
      changeForm p
      setting p Environments.blueAreaOfTheMoon
      _ <- stackEncounterDeck [Attachments.charge, Attachments.charge]
      card <- encounterCard Treacheries.evilGenius
      runNow [RevealEncounterCard p card]
      settleForced
      damageOf p `shouldReturn` 7

  it "Time-Travel Shenanigans reveals the next card belonging to the current Setting" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      setting p Environments.genosha
      _ <- stackEncounterDeck [Treacheries.advance, Minions.magistrate]
      card <- revealNow p Schemes.timeTravelShenanigans
      pass
      resolve [SideSchemeMessage (sideSchemeOf card) (SideSchemeThreatRemoved (IdentityRef p) 20)]
      selectAny (MinionIs Minions.magistrate) `shouldReturn` True

  it "Pterosaur resolves whichever Setting is current, and its Savage Land boost is dealt facedown" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      setting p Environments.genosha
      before <- mainSchemeThreatNow
      _ <- addMinion p Minions.pterosaur
      mainSchemeThreatNow `shouldReturn` before + 1
      setting p Environments.theSavageLand
      dealt <- length . playerIdentityEncounterCards <$> playerAttrs p
      _ <- boostNow Minions.pterosaur
      length . playerIdentityEncounterCards <$> playerAttrs p `shouldReturn` dealt + 1

  it "Velociraptor's attack bonus counts resource icons, including double-resource cards" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      raptor <- addMinion p Minions.velociraptor
      changeForm p
      _ <- stackDeck p ["01088"]
      resolve [MinionMessage raptor (MinionAttacks p)]
      damageOf p `shouldReturn` 3

  it "Giant Ape resolves its Special for the player who defeats it" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      setting p Environments.blueAreaOfTheMoon
      ape <- addMinion p Minions.giantApe
      perform $ L.dealDamage ape p (toDamage 5 FromAbility)
      damageOf p `shouldReturn` 1

  it "Land Out of Time counts the icons actually discarded for indirect damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      setting p Environments.theSavageLand
      cards <- stackDeck p ["01088", "01086", "01093"]
      treachery <- encounterCard Treacheries.landOutOfTime
      resolve [RevealEncounterCard p treachery]
      damageOf p `shouldReturn` 4
      discardOf p >>= (`shouldSatisfy` (\xs -> all (`elem` xs) cards))

  it "Land Out of Time finds Savage Land without dealing damage when it was absent" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      _ <- stackEncounterDeck [Environments.theSavageLand]
      setting p Environments.genosha
      treachery <- encounterCard Treacheries.landOutOfTime
      resolve [RevealEncounterCard p treachery]
      settingCode `shouldReturn` Just "45127"
      damageOf p `shouldReturn` 0

  it "Village Under Attack resolves the Special for each player" do
    tg <- testGame ["spider-man", "iron-man"]
    inGame tg do
      let p = player1 tg
      setting p Environments.blueAreaOfTheMoon
      card <- revealNow p Schemes.villageUnderAttack
      pass
      resolve [SideSchemeMessage (sideSchemeOf card) (SideSchemeThreatRemoved (IdentityRef p) 20)]
      damageOf p `shouldReturn` 1
      damageOf (player2 tg) `shouldReturn` 1

  it "Magistrate transfers Escaped Mutant to the player who defeats it" do
    tg <- testGame ["spider-man", "iron-man"]
    inGame tg do
      let p = player1 tg
          other = player2 tg
      escaped <- revealNow other Attachments.escapedMutant
      pass
      magistrate <- addMinion other Minions.magistrate
      perform $ L.dealDamage magistrate p (toDamage 3 FromAbility)
      getAttachmentIdentity (attachmentIdOf escaped) `shouldReturn` Just p

  it "Magistrate's boost gains three icons only for the Escaped Mutant's player" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      _ <- revealNow p Attachments.escapedMutant
      pass
      v <- villainIdNow
      resolve [VillainMessage v (VillainSchemesAgainst p)]
      _ <- boostNow Minions.magistrate
      Villain.villainBoost . toAttrs <$> getVillain `shouldReturn` 3

  it "Armored Unibike engages Escaped Mutant's attached player and resolves its Special after attacking" do
    tg <- testGame ["spider-man", "iron-man"]
    inGame tg do
      let p = player1 tg
          other = player2 tg
      setting p Environments.genosha
      _ <- revealNow other Attachments.escapedMutant
      pass
      bike <- addMinion p Minions.armoredUnibike
      selectAny (MinionWithId bike <> MinionEngagedWith (IdentityWithId other)) `shouldReturn` True
      before <- mainSchemeThreatNow
      resolve [MinionMessage bike (MinionAttacks other)]
      mainSchemeThreatNow `shouldReturn` before + 1

  it "Genoshan Mech resolves its Special twice after defeating an ally" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      setting p Environments.genosha
      mech <- addMinion p Minions.genoshanMech
      ally <- putIntoPlay p "45048"
      before <- mainSchemeThreatNow
      resolve [MinionMessage mech (MinionAttacksAlly p $ allyIdOf ally)]
      mainSchemeThreatNow `shouldReturn` before + 2

  it "Escaped Mutant grants quickstrike only when a Genosha minion engages its attached player" do
    tg <- testGame ["spider-man", "iron-man"]
    inGame tg do
      let p = player1 tg
          other = player2 tg
      changeForm p
      _ <- revealNow p Attachments.escapedMutant
      pass
      _ <- addMinion p Minions.magistrate
      damageOf p `shouldReturn` 2
      _ <- addMinion other Minions.magistrate
      damageOf other `shouldReturn` 0

  it "Escaped Mutant's alter-ego action resolves the Setting before discarding" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      setting p Environments.blueAreaOfTheMoon
      card <- revealNow p Attachments.escapedMutant
      pass
      choose "escape through the setting" (isAbilityOf $ attachmentIdOf card)
      pass
      damageOf p `shouldReturn` 1
      getAttachmentIdentity (attachmentIdOf card) `shouldReturn` Nothing

  it "Police State reveals Escaped Mutant for the player defeating it" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      [escaped] <- stackEncounterDeck [Attachments.escapedMutant]
      scheme <- revealNow p Schemes.policeState
      pass
      resolve [SideSchemeMessage (sideSchemeOf scheme) (SideSchemeThreatRemoved (IdentityRef p) 20)]
      getAttachmentIdentity (attachmentIdOf escaped) `shouldReturn` Just p

  it "Gladiator is immune while Trial by Combat is in play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      trial <- revealNow p Schemes.trialByCombat
      pass
      gladiator <- addMinion p Minions.gladiator
      perform $ L.dealDamage gladiator p (toDamage 4 FromAbility)
      minionDamageOf gladiator `shouldReturn` Just 0
      resolve [RemoveFromPlay $ SideSchemeRef $ sideSchemeOf trial]
      perform $ L.dealDamage gladiator p (toDamage 4 FromAbility)
      minionDamageOf gladiator `shouldReturn` Just 4

  for_ [(Minions.oracle, ConfusedStatus), (Minions.manta, StunnedStatus)] \(def, status) ->
    it ("Imperial Guard " <> show (toCardCode def) <> " applies its status or resolves the Special if already affected") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        setting p Environments.blueAreaOfTheMoon
        resolve [GiveStatus (IdentityRef p) status]
        _ <- addMinion p def
        damageOf p `shouldReturn` 1

  it "Earthquake exhausts the identity or resolves the current Special if already exhausted" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      setting p Environments.blueAreaOfTheMoon
      resolve [IdentityMessage p ExhaustedIdentity]
      _ <- addMinion p Minions.earthquake
      damageOf p `shouldReturn` 1

  it "Warstar reveals the discarded Imperial Guard minion" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      _ <- stackEncounterDeck [Minions.oracle]
      _ <- revealNow p Minions.warstar
      pass
      selectAny (MinionIs Minions.oracle) `shouldReturn` True

  it "Warstar resolves the Special if its discarded card was not an Imperial Guard minion" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      setting p Environments.blueAreaOfTheMoon
      _ <- stackEncounterDeck [Treacheries.advance]
      _ <- addMinion p Minions.warstar
      damageOf p `shouldReturn` 1

  it "Imperial Guardsman gives its minion four HP and the trait, then resolves the Special on defeat" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      setting p Environments.blueAreaOfTheMoon
      mid <- addMinion p Minions.hydraRegular
      attachment <- revealNow p Attachments.imperialGuardsman
      choose "attach the title" (isTargetChoice mid)
      pass
      attachmentHost attachment `shouldReturn` Just (EnemyMinionId mid)
      getModifiers (MinionRef mid) >>= (`shouldSatisfy` elem (HitPointModifier 4))
      perform $ L.dealDamage mid p (toDamage 6 FromAbility)
      damageOf p `shouldReturn` 1

  it "Trial by Combat shuffles only discarded Imperial Guard minions" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      oracle <- encounterCard Minions.oracle
      other <- encounterCard Minions.hydraRegular
      resolve [DiscardedCard $ EncounterCard oracle, DiscardedCard $ EncounterCard other]
      scheme <- revealNow p Schemes.trialByCombat
      pass
      resolve [SideSchemeMessage (sideSchemeOf scheme) (SideSchemeThreatRemoved (IdentityRef p) 20)]
      getEncounterDeck >>= (`shouldSatisfy` elem oracle)
      getEncounterDiscard >>= (`shouldSatisfy` elem other)

  it "Savage Land resource-counting effects respect Domino's doubled wild icons" do
    tg <- testGame ["domino"]
    inGame tg do
      let p = player1 tg
      raptor <- addMinion p Minions.velociraptor
      changeForm p
      _ <- stackDeck p ["40040"]
      resolve [MinionMessage raptor (MinionAttacks p)]
      damageOf p `shouldReturn` 3
      setting p Environments.theSavageLand
      _ <- stackDeck p ["40040", "01086", "01093"]
      card <- encounterCard Treacheries.landOutOfTime
      resolve [RevealEncounterCard p card]
      damageOf p `shouldReturn` 7

  it "Dark Beast also resolves the controller's Special when directly attacking an ally" do
    tg <- darkBeast Normal
    inGame tg do
      let p = player1 tg
      setting p Environments.genosha
      ally <- putIntoPlay p "45031"
      before <- mainSchemeThreatNow
      v <- villainIdNow
      resolve [VillainMessage v (VillainAttacksAlly p $ allyIdOf ally)]
      mainSchemeThreatNow `shouldReturn` before + 1

  it "Velociraptor discards and enhances its direct attack against an ally" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      raptor <- addMinion p Minions.velociraptor
      ally <- putIntoPlay p "45048"
      [discarded] <- stackDeck p ["01088"]
      resolve [MinionMessage raptor (MinionAttacksAlly p $ allyIdOf ally)]
      discardOf p >>= (`shouldSatisfy` elem discarded)
      alliesWith "45048" `shouldReturn` []

  it "Armored Unibike resolves the Special after directly attacking an ally" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      setting p Environments.genosha
      bike <- addMinion p Minions.armoredUnibike
      ally <- putIntoPlay p "45031"
      before <- mainSchemeThreatNow
      resolve [MinionMessage bike (MinionAttacksAlly p $ allyIdOf ally)]
      mainSchemeThreatNow `shouldReturn` before + 1

  for_ [(Minions.oracle, ConfusedIdentity), (Minions.manta, StunnedIdentity)] \(def, condition) ->
    it ("Imperial Guard " <> show (toCardCode def) <> " initially applies its status") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let p = player1 tg
        setting p Environments.blueAreaOfTheMoon
        _ <- addMinion p def
        identityMatches condition p `shouldReturn` True
        damageOf p `shouldReturn` 0

  it "Earthquake initially exhausts an otherwise ready identity" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      setting p Environments.blueAreaOfTheMoon
      _ <- addMinion p Minions.earthquake
      isExhaustedPlayer p `shouldReturn` True
      damageOf p `shouldReturn` 0

  it "does not offer attachment removal when Savage Land's Special has no cards to discard" do
    tg <- darkBeast Normal
    inGame tg do
      let p = player1 tg
      changeForm p
      setting p Environments.theSavageLand
      card <- revealNow p Attachments.highTechGoggles
      pass
      withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity attrs) -> PlayerIdentity attrs
        {playerIdentityDeck = Deck [], playerIdentityDiscard = Discard []}
      offers (isAbilityOf $ attachmentIdOf card) `shouldReturn` False
      isExhaustedPlayer p `shouldReturn` False
      getsGame (HashMap.member (attachmentIdOf card) . gameAttachments) `shouldReturn` True

  it "does not offer Escaped Mutant's removal when the Setting is blank" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let p = player1 tg
      setting p Environments.blueAreaOfTheMoon
      card <- revealNow p Attachments.escapedMutant
      pass
      blankSetting
      offers (isAbilityOf $ attachmentIdOf card) `shouldReturn` False
      getAttachmentIdentity (attachmentIdOf card) `shouldReturn` Just p

  it "pays the Setting Special before the attachment ability is used" do
    tg <- darkBeast Normal
    inGame tg do
      let p = player1 tg
      changeForm p
      setting p Environments.blueAreaOfTheMoon
      card <- revealNow p Attachments.highTechGoggles
      pass
      liftIO $ writeIORef (ran tg) []
      choose "pay both attachment removal costs" (isAbilityOf $ attachmentIdOf card)
      pass
      damageOf p `shouldReturn` 1
      messages <- reverse <$> liftIO (readIORef $ ran tg)
      let position needle = headMay [index | (index, message) <- zip [0 :: Int ..] messages, needle `Text.isInfixOf` message]
      case (position "IdentityDamaged", position "UsedAbility") of
        (Just damaged, Just used) -> damaged `shouldSatisfy` (< used)
        found -> liftIO $ expectationFailure $ "missing cost/ability trace: " <> show found

-- Attack interrupts must resolve before checking the attack's actual damage.
settleForced :: GameAppT ()
settleForced = do
  pass
  question <- asked
  case question >>= (headMay . filter forced . snd) of
    Just choice -> choose "resolve mandatory ability" (== choice) >> settleForced
    Nothing -> pure ()
 where
  forced (UseAbility ab) = Ability.abilityType ab `elem` [Ability.ForcedInterrupt, Ability.ForcedResponse, Ability.WhenRevealed]
  forced _ = False
