module Marvel.Encounter.SinisterModularsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Environment.Cards qualified as Environments
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Hero.GhostSpiderSpec (ghostSpider, asGhost)
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Keyword qualified as Keyword
import Marvel.Keyword (Keyword (Overkill, Piercing, Retaliate))
import Marvel.Villain.Villains.SmSinisterSix qualified as Six
import Marvel.Villain.Types (villainTough, villainBoostCards)
import Marvel.Modifier
import Marvel.Window qualified as W
import TestImport

attachmentOf :: EncounterCard -> AttachmentId
attachmentOf = AttachmentId . unCardId . ecCardId

attachmentPresent :: EncounterCard -> GameAppT Bool
attachmentPresent card = getsGame (HashMap.member (attachmentOf card) . gameAttachments)

revealAttachment :: IdentityId -> CardDef -> GameAppT EncounterCard
revealAttachment p def = do
  _ <- stackEncounterDeck [Environments.smPublicOutcryA]
  revealNow p def

sixGame :: IO TestGame
sixGame = do
  let setup = fromMaybe (error "Six setup missing") $ recommendedSetup "27100"
  (app, ran, logged) <- dealtGameLogged setup [starter "ghost-spider"] 1
  seats <- runGameApp app $ getsGame gameSeats
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

spec :: Spec
spec = describe "Sinister Motives modular sets" do
  describe "Goblin Gear" do
    for_ [Attachments.smConcussiveBombs, Attachments.smIncendiaryBombs, Attachments.smSmokeBombs] $ \def ->
      it ("attaches with two bomb counters and increases the villain attack: " <> show (toCardCode def)) do
        tg <- ghostSpider
        inGame tg do
          card <- revealAttachment (player1 tg) def
          getCountersOn (AttachmentRef $ attachmentOf card) `shouldReturn` 2
          vid <- villainIdNow
          mods <- getModifiers (VillainRef vid)
          mods `shouldSatisfy` elem (AttackModifier 1)

    it "does not offer Advanced Glider's removal without enough printed attack cost" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        card <- revealAttachment p Attachments.smAdvancedGlider
        _ <- putInHand p "01088"
        offers (isAbilityOf $ AttachmentRef $ attachmentOf card) `shouldReturn` False

    it "pays discarded attack cards' combined printed cost to remove Advanced Glider" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        card <- revealAttachment p Attachments.smAdvancedGlider
        attack <- putInHand p "01087"
        second <- putInHand p "01087"
        choose "remove Advanced Glider" (isAbilityOf $ AttachmentRef $ attachmentOf card)
        choose "discard Haymaker" (isTargetChoice attack)
        chooseIf (isTargetChoice second)
        attachmentPresent card `shouldReturn` False
        discardOf p >>= (`shouldSatisfy` \cards -> attack `elem` cards && second `elem` cards)

    it "Incendiary Bombs spends exactly one counter for two indirect damage after an attack" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        card <- revealAttachment p Attachments.smIncendiaryBombs
        vid <- villainIdNow
        runNow [CheckWindows [W.Window W.After (W.EnemyAttack (EnemyVillainId vid) p)]]
        choose "Incendiary Bombs" (isAbilityOf $ AttachmentRef $ attachmentOf card)
        pass
        damageOf p `shouldReturn` 2
        getCountersOn (AttachmentRef $ attachmentOf card) `shouldReturn` 1

    it "Smoke Bombs discards the lowest-cost event and leaves non-events alone" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        card <- revealAttachment p Attachments.smSmokeBombs
        cheap <- putInHand p "01085"
        expensive <- putInHand p "01087"
        resource <- putInHand p "01088"
        vid <- villainIdNow
        runNow [CheckWindows [W.Window W.After (W.EnemyAttack (EnemyVillainId vid) p)]]
        choose "Smoke Bombs" (isAbilityOf $ AttachmentRef $ attachmentOf card)
        pass
        handOf p >>= (`shouldSatisfy` all (`elem` [expensive, resource]))
        discardOf p >>= (`shouldSatisfy` elem cheap)

    it "Remote Navigation searches and reveals Advanced Glider from the discard pile" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        (_, discard) <- setEncounterDeck [Minions.hydraMercenary] [Attachments.smAdvancedGlider]
        _ <- revealNow p Treacheries.smRemoteNavigation
        pass
        attachmentPresent (testHead discard) `shouldReturn` True

    it "Concussive Bombs exhausts one upgrade and one support, spending its counter" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        upgrade <- putIntoPlay p "01093"
        support <- putIntoPlay p "01091"
        card <- revealAttachment p Attachments.smConcussiveBombs
        vid <- villainIdNow
        runNow [CheckWindows [W.Window W.After (W.EnemyAttack (EnemyVillainId vid) p)]]
        choose "Concussive Bombs" (isAbilityOf $ AttachmentRef $ attachmentOf card)
        pass
        getsGame (fmap (upgradeExhausted . toAttrs) . HashMap.lookup (upgradeOf upgrade) . gameUpgrades) `shouldReturn` Just True
        getsGame (fmap (supportExhausted . toAttrs) . HashMap.lookup (SupportId $ unCardId $ pcCardId support) . gameSupports) `shouldReturn` Just True
        getCountersOn (AttachmentRef $ attachmentOf card) `shouldReturn` 1

    it "Concussive Bombs still spends its counter when its player has no upgrades or supports" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        card <- revealAttachment p Attachments.smConcussiveBombs
        vid <- villainIdNow
        runNow [CheckWindows [W.Window W.After (W.EnemyAttack (EnemyVillainId vid) p)]]
        choose "Concussive Bombs" (isAbilityOf $ AttachmentRef $ attachmentOf card)
        pass
        getCountersOn (AttachmentRef $ attachmentOf card) `shouldReturn` 1

    it "Advanced Glider repeats an activation only once per round for that player" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        card <- revealAttachment p Attachments.smAdvancedGlider
        vid <- villainIdNow
        _ <- setEncounterDeck [Environments.smPublicOutcryA, Environments.smPublicOutcryB] []
        runNow [CheckWindows [W.Window W.After (W.EnemyActivation (EnemyVillainId vid) p)]]
        choose "Advanced Glider" (isAbilityOf $ AttachmentRef $ attachmentOf card)
        pass
        mainSchemeThreatNow `shouldReturn` 1
        runNow [CheckWindows [W.Window W.After (W.EnemyActivation (EnemyVillainId vid) p)]]
        offers (isAbilityOf $ AttachmentRef $ attachmentOf card) `shouldReturn` False
        pass
        mainSchemeThreatNow `shouldReturn` 1

    it "Limitless Supply grants surge to a Tech attachment without a printed surge keyword" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        supply <- revealNow p Schemes.smLimitlessSupply
        getsGame (HashMap.member (sideSchemeOf supply) . gameSideSchemes) `shouldReturn` True
        _ <- setEncounterDeck [Environments.smPublicOutcryA, Minions.hydraMercenary] []
        card <- revealNow p Attachments.smConcussiveBombs
        before <- map toCardCode . playerIdentityEncounterCards <$> playerAttrs p
        pass
        getModifiers (AttachmentRef $ attachmentOf card) >>= (`shouldSatisfy` elem (KeywordModifier Keyword.Surge))
        after <- map toCardCode . playerIdentityEncounterCards <$> playerAttrs p
        (before, after) `shouldSatisfy` (elem "27174a" . snd)

  describe "Osborn Tech" do
    it "Arm Cannon grants overkill and piercing only to its attached villain" do
      tg <- ghostSpider
      inGame tg do
        _ <- revealAttachment (player1 tg) Attachments.smArmCannon
        vid <- villainIdNow
        mods <- getModifiers (VillainRef vid)
        mods `shouldSatisfy` elem (KeywordModifier Overkill)
        mods `shouldSatisfy` elem (KeywordModifier Piercing)

    it "Kinetic Armor grants retaliate and charges three actual indirect damage for removal" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        card <- revealAttachment p Attachments.smKineticArmor
        vid <- villainIdNow
        getModifiers (VillainRef vid) >>= (`shouldSatisfy` elem (KeywordModifier $ Retaliate 1))
        choose "remove Kinetic Armor" (isAbilityOf $ AttachmentRef $ attachmentOf card)
        pass
        damageOf p `shouldReturn` 3
        attachmentPresent card `shouldReturn` False

    it "Kinetic Armor cannot be removed using damage that tough would prevent" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        card <- revealAttachment p Attachments.smKineticArmor
        resolve [GiveStatus (IdentityRef p) ToughStatus]
        offers (isAbilityOf $ AttachmentRef $ attachmentOf card) `shouldReturn` False

    it "Neocarbon Scales cannot pay its removal cost when the villain is already tough" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        card <- revealAttachment p Attachments.smNeocarbonScales
        vid <- villainIdNow
        resolve [GiveStatus (VillainRef vid) ToughStatus]
        offers (isAbilityOf $ AttachmentRef $ attachmentOf card) `shouldReturn` False

    it "Tracking Display grants the attached villain undefendable attacks" do
      tg <- ghostSpider
      inGame tg do
        _ <- revealAttachment (player1 tg) Attachments.smTrackingDisplay
        vid <- villainIdNow
        getModifiers (VillainRef vid) >>= (`shouldSatisfy` elem AttacksCannotBeDefended)

    it "Arm Cannon requires discarding an upgrade with the highest printed cost" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        cheaper <- putIntoPlay p "27024"
        expensive <- putIntoPlay p "01093"
        _ <- putIntoPlay p "01093"
        card <- revealAttachment p Attachments.smArmCannon
        choose "remove Arm Cannon" (isAbilityOf $ AttachmentRef $ attachmentOf card)
        offers (isTargetChoice $ upgradeOf cheaper) `shouldReturn` False
        chooseIf (isTargetChoice $ upgradeOf expensive)
        pass
        attachmentPresent card `shouldReturn` False
        discardOf p >>= (`shouldSatisfy` elem expensive)

    it "Ionic Boots places threat after damaging an identity but not after damaging an ally" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        card <- revealAttachment p Attachments.smIonicBoots
        ally <- putIntoPlay p "01002"
        vid <- villainIdNow
        runNow [CheckWindows [W.Window W.After (W.EnemyAttacksAndDamages (EnemyVillainId vid) (AllyCharacter $ allyIdOf ally) 1)]]
        offers (isAbilityOf $ AttachmentRef $ attachmentOf card) `shouldReturn` False
        pass
        runNow [CheckWindows [W.Window W.After (W.EnemyAttacksAndDamages (EnemyVillainId vid) (IdentityCharacter p) 1)]]
        choose "Ionic Boots" (isAbilityOf $ AttachmentRef $ attachmentOf card)
        pass
        mainSchemeThreatNow `shouldReturn` 2

    it "Neocarbon Scales pays a tough status and a facedown boost before leaving play" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        card <- revealAttachment p Attachments.smNeocarbonScales
        _vid <- villainIdNow
        choose "remove Neocarbon Scales" (isAbilityOf $ AttachmentRef $ attachmentOf card)
        pass
        attachmentPresent card `shouldReturn` False
        villain <- getVillain
        villainTough (toAttrs villain) `shouldBe` True
        length (villainBoostCards $ toAttrs villain) `shouldBe` 1

    it "Spiked Gauntlet attacks its payer and stays if that identity is damaged" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        card <- revealAttachment p Attachments.smSpikedGauntlet
        _ <- stackEncounterDeck [Treacheries.sonicBoom]
        choose "remove Spiked Gauntlet" (isAbilityOf $ AttachmentRef $ attachmentOf card)
        choose "do not defend" isNoDefender
        pass
        damageOf p >>= (`shouldSatisfy` (> 0))
        attachmentPresent card `shouldReturn` True

    it "Tracking Display pays exhaustion and a random hand discard to leave play" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        card <- revealAttachment p Attachments.smTrackingDisplay
        resource <- putInHand p "01088"
        choose "remove Tracking Display" (isAbilityOf $ AttachmentRef $ attachmentOf card)
        pass
        isExhaustedNow p `shouldReturn` True
        discardOf p >>= (`shouldSatisfy` elem resource)
        attachmentPresent card `shouldReturn` False

    it "Spiked Gauntlet leaves when tough prevents all damage from its attack" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        card <- revealAttachment p Attachments.smSpikedGauntlet
        resolve [GiveStatus (IdentityRef p) ToughStatus]
        _ <- stackEncounterDeck [Treacheries.sonicBoom]
        choose "remove Spiked Gauntlet" (isAbilityOf $ AttachmentRef $ attachmentOf card)
        choose "do not defend" isNoDefender
        pass
        damageOf p `shouldReturn` 0
        attachmentPresent card `shouldReturn` False

    it "Ionic Boots pays one energy, mental, and physical resource to leave play" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        card <- revealAttachment p Attachments.smIonicBoots
        resources <- traverse (putInHand p) ["01088", "01089", "01090"]
        choose "remove Ionic Boots" (isAbilityOf $ AttachmentRef $ attachmentOf card)
        payWith resources
        attachmentPresent card `shouldReturn` False

    it "Neocarbon Scales reduces each attack's damage by one" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        _ <- revealAttachment p Attachments.smNeocarbonScales
        choose "basic attack" (isHeroAttack p)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 1

  describe "Sinister Assault" do
    it "Doctor Octopus places threat on every scheme after activating" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        card <- revealNow p Minions.smDoctorOctopus
        pass
        before <- mainSchemeThreatNow
        sid <- sideSchemeOf <$> revealNow p Schemes.smLimitlessSupply
        pass
        runNow [CheckWindows [W.Window W.After (W.EnemyActivation (EnemyMinionId $ minionOf card) p)]]
        choose "Doctor Octopus" (isAbilityOf $ MinionRef $ minionOf card)
        pass
        mainSchemeThreatNow `shouldReturn` (before + 1)
        sideSchemeThreat sid `shouldReturn` 6

    it "Electro discards through the first printed energy or wild card when engaging" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity attrs) -> PlayerIdentity attrs {playerIdentityDeck = Deck []}
        cards <- stackDeck p ["01086", "01088", "01089"]
        let first = cards !! 0
            stop = cards !! 1
        card <- revealNow p Minions.smElectro
        chooseIf (isAbilityOf $ MinionRef $ minionOf card)
        pass
        discardOf p >>= (`shouldSatisfy` all (`elem` [first, stop]))

    it "Hobgoblin deals two indirect damage after attacking" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        card <- revealNow p Minions.smHobgoblin
        pass
        runNow [CheckWindows [W.Window W.After (W.EnemyAttack (EnemyMinionId $ minionOf card) p)]]
        choose "Hobgoblin" (isAbilityOf $ MinionRef $ minionOf card)
        pass
        damageOf p `shouldReturn` 2

    it "Scorpion stuns a damaged character, and deals two damage if already stunned" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        card <- revealNow p Minions.smScorpion
        pass
        for_ [1 :: Int, 2] $ \_ -> do
          runNow [CheckWindows [W.Window W.After (W.EnemyAttacksAndDamages (EnemyMinionId $ minionOf card) (IdentityCharacter p) 1)]]
          choose "Scorpion" (isAbilityOf $ MinionRef $ minionOf card)
          pass
        isStunned p `shouldReturn` True
        damageOf p `shouldReturn` 2

    it "Vulture discards one random card after activating" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        card <- revealNow p Minions.smVulture
        pass
        clearHand p
        resource <- putInHand p "01088"
        runNow [CheckWindows [W.Window W.After (W.EnemyActivation (EnemyMinionId $ minionOf card) p)]]
        choose "Vulture" (isAbilityOf $ MinionRef $ minionOf card)
        pass
        discardOf p >>= (`shouldSatisfy` elem resource)

    for_ [False, True] $ \permanent ->
      it ("Kraven resolves without a discard question when no discardable card exists: " <> show permanent) do
        tg <- ghostSpider
        let p = player1 tg
        inGame tg do
          when permanent $ void $ putIntoPlay p "27182a"
          card <- revealNow p Minions.smKravenTheHunter
          pass
          runNow [CheckWindows [W.Window W.After (W.EnemyAttacksAndDamages (EnemyMinionId $ minionOf card) (IdentityCharacter p) 1)]]
          choose "Kraven" (isAbilityOf $ MinionRef $ minionOf card)
          pass
          if permanent then upgradesWith "27182a" >>= (\cards -> length cards `shouldBe` 1) else pure ()

    it "Kraven discards an upgrade or support controlled by the damaged character's player" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        upgrade <- putIntoPlay p "01093"
        card <- revealNow p Minions.smKravenTheHunter
        pass
        runNow [CheckWindows [W.Window W.After (W.EnemyAttacksAndDamages (EnemyMinionId $ minionOf card) (IdentityCharacter p) 1)]]
        choose "Kraven" (isAbilityOf $ MinionRef $ minionOf card)
        chooseIf (isTargetChoice $ upgradeOf upgrade)
        pass
        discardOf p >>= (`shouldSatisfy` elem upgrade)

  describe "Symbiotic Strength" do
    it "Webbed Up attaches to the revealing identity and replaces that hero's next attack" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        card <- revealAttachment p Attachments.smWebbedUp
        choose "attack" (isHeroAttack p)
        pass
        attachmentPresent card `shouldReturn` False
        isStunned p `shouldReturn` True
        villainDamageNow `shouldReturn` 0

    it "Violent Tendencies adds a facedown boost after taking attack damage and discards at three" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        card <- revealAttachment p Attachments.smViolentTendencies
        haymaker <- putInHand p "01087"
        energy <- putInHand p "01088"
        playWith haymaker [energy]
        chooseVillain
        choose "Violent Tendencies" (isAbilityOf $ AttachmentRef $ attachmentOf card)
        pass
        attachmentPresent card `shouldReturn` False

    it "Enraged Symbiote enters play engaged with the player when turned up as a boost" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        _ <- stackEncounterDeck [Minions.smEnragedSymbiote]
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        pass
        getsGame (map (toCardCode . minionCardDef . toAttrs) . HashMap.elems . gameMinions) >>= (`shouldSatisfy` elem "27167")

    it "Webbed Up's boost stuns, then deals two damage if already stunned" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        card <- revealAttachment p Attachments.smWebbedUp
        vid <- villainIdNow
        for_ [1 :: Int, 2] $ \_ -> resolve [Boost $ RevealedAsBoost (AttachmentRef $ attachmentOf card) (EnemyVillainId vid)]
        isStunned p `shouldReturn` True
        damageOf p `shouldReturn` 2

    it "Improvised Weapons has a printed amplify icon and the three-type removal cost" do
      cdAmplify Attachments.smImprovisedWeapons `shouldBe` 1
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        card <- revealAttachment p Attachments.smImprovisedWeapons
        resources <- traverse (putInHand p) ["01088", "01089", "01090"]
        choose "remove Improvised Weapons" (isAbilityOf $ AttachmentRef $ attachmentOf card)
        payWith resources
        attachmentPresent card `shouldReturn` False

    it "Swinging Assault changes an alter-ego to hero before attacking" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        _ <- stackEncounterDeck [Environments.smPublicOutcryA]
        _ <- revealNow p Treacheries.smSwingingAssault
        choose "no defender" isNoDefender
        pass
        playerIdentitySide <$> playerAttrs p `shouldReturn` A
        damageOf p `shouldReturn` 2

    it "Swinging Assault gives an existing hero an extra boost card" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        _ <- setEncounterDeck [Minions.hydraMercenary, Minions.hydraMercenary, Environments.smPublicOutcryA] []
        _ <- revealNow p Treacheries.smSwingingAssault
        choose "no defender" isNoDefender
        pass
        damageOf p `shouldReturn` 4

    it "Unstable Sentience gives the villain a facedown boost when revealed" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        _ <- setEncounterDeck [Minions.hydraMercenary, Environments.smPublicOutcryA] []
        _ <- revealNow p Treacheries.smUnstableSentience
        pass
        villain <- getVillain
        length (villainBoostCards $ toAttrs villain) `shouldBe` 1

    it "Swinging Assault does not deal any additional boost when stunned replaces the attack" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        vid <- villainIdNow
        resolve [GiveStatus (VillainRef vid) StunnedStatus]
        _ <- setEncounterDeck [Minions.hydraMercenary, Minions.hydraMercenary] []
        _ <- revealNow p Treacheries.smSwingingAssault
        pass
        damageOf p `shouldReturn` 0
        encounterDeckSize `shouldReturn` 2
        villain <- getVillain
        villainBoostCards (toAttrs villain) `shouldBe` []

    it "Swinging Assault ambushes with a Sinister Six villain before its extra-boost attack" do
      tg <- sixGame
      let p = player1 tg
      inGame tg do
        asGhost p
        villains <- Six.sixVillains
        for_ villains $ \(vid, _) -> resolve [ResolveVillainDefeat vid]
        Six.sixVillains `shouldReturn` []
        _ <- setEncounterDeck [Environments.smPublicOutcryA, Environments.smPublicOutcryB, Environments.smPublicOutcryA] []
        _ <- revealNow p Treacheries.smSwingingAssault
        chooseIf isNoDefender
        pass
        Six.sixVillains >>= (\present -> length present `shouldBe` 1)
        damageOf p >>= (`shouldSatisfy` (> 0))
        encounterDeckSize `shouldReturn` 1

    it "Remote Navigation's boost deals two additional boost cards for that activation" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        _ <- setEncounterDeck [Treacheries.smRemoteNavigation, Minions.hydraMercenary, Minions.hydraMercenary, Environments.smPublicOutcryA] []
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        choose "no defender" isNoDefender
        pass
        damageOf p `shouldReturn` 4
        encounterDeckSize `shouldReturn` 1

    it "Unstable Sentience's boost adds two icons during an attack" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        _ <- setEncounterDeck [Treacheries.smUnstableSentience, Environments.smPublicOutcryA] []
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainAttacks p)]
        choose "no defender" isNoDefender
        pass
        damageOf p `shouldReturn` 4
