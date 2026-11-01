module Marvel.Encounter.NextEvolutionModularsSpec (spec) where
import Control.Monad (replicateM, replicateM_)
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Types qualified as Ally
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types hiding (AttachmentId)
import Marvel.EncounterSet qualified as Set
import Marvel.Keyword
import Marvel.Matchers hiding (ExhaustedIdentity, ExhaustedAlly)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import TestImport

modularGame :: Set.EncounterSet -> [Text] -> IO TestGame
modularGame set heroes = do
  (app,ran,logged) <- dealtGameLogged (ScenarioSetup "01094" [set] Normal) (map starter heroes) 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

ackForced :: GameAppT ()
ackForced = openQuestion >>= \case
  Just (you, QuestionLabel "$label.resolveForced" _ _) -> answerQuestion you 0 >> ackForced
  Just (you,q) | Just n <- headMay [i | (i,Label label _) <- zip [0..] (choicesOf q), label `elem` ["$label.noResponse", "$label.noDefender", "$label.declineToDefend"]] -> answerQuestion you n >> ackForced
  _ -> pure ()

settle :: GameAppT ()
settle = go (150 :: Int)
 where
  go 0 = error "NeXt Evolution modular test did not settle"
  go n = openQuestion >>= \case
    Just (_,QuestionLabel "$label.yourTurn" _ _) -> pure ()
    Just (you,_) -> answerQuestion you 0 >> go (n-1)
    Nothing -> pure ()

attachmentWith :: CardCode -> GameAppT (Attrs Attachment)
attachmentWith code = getsGame (filter ((==code) . toCardCode . getCardDef) . map toAttrs . HashMap.elems . gameAttachments) >>= \case
  [a] -> pure a
  _ -> error "expected one attachment"

isAttachmentAbility :: AttachmentId -> Natural -> Choice -> Bool
isAttachmentAbility aid n = \case
  UseAbility ab -> abilitySource ab == toSource aid && abilityIndex ab == n
  _ -> False

_cardAbility :: PlayerCard -> Natural -> Choice -> Bool
_cardAbility card n = \case
  UseAbility ab -> isAbilityOfCard card (UseAbility ab) && abilityIndex ab == n
  _ -> False

spec :: Spec
spec = describe "NeXt Evolution modulars" do
  describe "Military Grade" do
    it "Heavy Armament attaches to the highest ATK enemy and grants +2 ATK and retaliate 2" do
      tg <- modularGame Set.MilitaryGrade ["spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- revealNow p Attachments.ne40090
        pass
        a <- attachmentWith "40090"
        vid <- villainIdNow
        attachmentEnemy a `shouldBe` Just (EnemyVillainId vid)
        getVillainAttackValue vid `shouldReturn` 4
        keywords <- Villain.getModifiedKeywords . toAttrs =<< getVillain
        keywords `shouldSatisfy` elem (Retaliate 2)
    it "Heavy Armament can be discarded with two same-type resources after a hero attacks" do
      tg <- modularGame Set.MilitaryGrade ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        _ <- revealNow p Attachments.ne40090
        pass
        a <- attachmentWith "40090"
        resources <- replicateM 2 (putInHand p "01003")
        choose "basic attack" (\case UseAbility ab -> abilitySource ab == toSource p && abilityIndex ab == 301; _ -> False)
        chooseVillain
        choose "discard armament" (isAttachmentAbility (attachmentId a) 1)
        payWith resources
        pass
        getsGame (HashMap.member (attachmentId a) . gameAttachments) `shouldReturn` False
    it "Titanium Exoskeleton attaches to the enemy with fewest remaining hit points" do
      tg <- modularGame Set.MilitaryGrade ["spider-man"]
      inGame tg do
        let p = player1 tg
        mid <- addMinion p Minions.hydraMercenary
        _ <- revealNow p Attachments.ne40091
        pass
        attachmentEnemy <$> attachmentWith "40091" `shouldReturn` Just (EnemyMinionId mid)
    it "Titanium Exoskeleton limits a large attack to two damage" do
      tg <- modularGame Set.MilitaryGrade ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        _ <- revealNow p Attachments.ne40091
        pass
        kick <- putInHand p "01005"
        two <- putInHand p "01090"
        one <- putInHand p "01003"
        playWith kick [two,one]
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 2
    it "Titanium Exoskeleton caps the complete resource-enhanced attack" do
      tg <- modularGame Set.MilitaryGrade ["domino"]
      inGame tg do
        let p = player1 tg
        asHero p
        _ <- revealNow p Attachments.ne40091
        pass
        _ <- stackDeck p ["40040"]
        workout <- putInHand p "40040"
        pay <- putInHand p "01088"
        playWith workout [pay]
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 2
    for_ [(11,StunnedStatus),(12,ConfusedStatus)] $ \(index,status) ->
      it ("Titanium Exoskeleton can be discarded by removing " <> show status <> " from its host") do
        tg <- modularGame Set.MilitaryGrade ["spider-man"]
        inGame tg do
          let p = player1 tg
          asHero p
          _ <- revealNow p Attachments.ne40091
          pass
          a <- attachmentWith "40091"
          vid <- villainIdNow
          resolve [GiveStatus (VillainRef vid) status]
          choose "pay status removal" (isAttachmentAbility (attachmentId a) index)
          pass
          getsGame (HashMap.member (attachmentId a) . gameAttachments) `shouldReturn` False
          characterMatches (CharacterWithStatus status) (VillainCharacter vid) `shouldReturn` False
    it "Titanium Exoskeleton offers no status-removal payment when the host has no such status" do
      tg <- modularGame Set.MilitaryGrade ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        _ <- revealNow p Attachments.ne40091
        pass
        a <- attachmentWith "40091"
        offers (isAttachmentAbility (attachmentId a) 11) `shouldReturn` False
        offers (isAttachmentAbility (attachmentId a) 12) `shouldReturn` False
    it "Inhibitor Collar blanks only its attached identity and applies its printed ATK penalty" do
      tg <- modularGame Set.MilitaryGrade ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        _ <- revealNow p Attachments.ne40092
        pass
        isBlanked p `shouldReturn` True
        mods <- getModifiers p
        mods `shouldSatisfy` elem (AttackPenalty 1)
    it "any player can remove Inhibitor Collar by exhausting a character they control" do
      tg <- modularGame Set.MilitaryGrade ["spider-man", "captain-marvel"]
      inGame tg do
        let p = player1 tg; other = player2 tg
        card <- revealNow p Attachments.ne40092
        pass
        endTurn
        pass
        let aid = AttachmentId $ unCardId $ ecCardId card
        choose "other player removes collar" (isAttachmentAbility aid 20)
        choose "exhaust their character" (isTargetChoice other)
        pass
        exhaustedNow other `shouldReturn` True
        isBlanked p `shouldReturn` False
    it "Inhibitor Collar may instead be removed by taking three damage" do
      tg <- modularGame Set.MilitaryGrade ["spider-man"]
      inGame tg do
        let p = player1 tg
        card <- revealNow p Attachments.ne40092
        pass
        choose "take damage to remove" (isAttachmentAbility (AttachmentId $ unCardId $ ecCardId card) 21)
        pass
        damageOf p `shouldReturn` 3
        isBlanked p `shouldReturn` False
    it "the damage-payment option on Inhibitor Collar is unavailable through tough" do
      tg <- modularGame Set.MilitaryGrade ["spider-man"]
      inGame tg do
        let p = player1 tg
        card <- revealNow p Attachments.ne40092
        pass
        resolve [IdentityMessage p IdentityBecomeTough]
        offers (isAttachmentAbility (AttachmentId $ unCardId $ ecCardId card) 21) `shouldReturn` False
    it "The Senator's Support adds Hinder and reveals the attachment found when defeated" do
      tg <- modularGame Set.MilitaryGrade ["spider-man"]
      inGame tg do
        let p = player1 tg
        scheme <- sideSchemeOf <$> revealNow p Schemes.ne40093
        pass
        sideSchemeThreat scheme `shouldReturn` 4
        _ <- stackEncounterDeck [Treacheries.imTough, Attachments.ne40092]
        resolve [SideSchemeMessage scheme (SideSchemeThwarted (toSource p) 4)]
        isBlanked p `shouldReturn` True
  describe "Nasty Boys" do
    it "Gorgeous George exhausts a character before attacking, preventing that identity from defending" do
      tg <- modularGame Set.NastyBoys ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        george <- addMinion p Minions.ne40112
        runNow [MinionMessage george (MinionAttacks p)]
        ackForced
        choose "exhaust hero" (isTargetChoice p)
        pass
        exhaustedNow p `shouldReturn` True
        damageOf p `shouldReturn` 2
    it "Gorgeous George's boost exhausts a controlled character" do
      tg <- modularGame Set.NastyBoys ["spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- stackEncounterDeck [Minions.ne40112]
        vid <- villainIdNow
        runNow [VillainMessage vid VillainSchemes]
        choose "exhaust identity" (isTargetChoice p)
        pass
        exhaustedNow p `shouldReturn` True
    it "Hairbag surges when revealed" do
      tg <- modularGame Set.NastyBoys ["spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Minions.ne40113
        ackForced
        pass
        dealtTo p >>= (`shouldSatisfy` any ((== "01101") . toCardCode . getCardDef))
    it "Hairbag's boost is shuffled back after the activation instead of remaining discarded" do
      tg <- modularGame Set.NastyBoys ["spider-man"]
      inGame tg do
        _ <- stackEncounterDeck [Minions.ne40113]
        vid <- villainIdNow
        resolve [VillainMessage vid VillainSchemes]
        encounterDiscard >>= (`shouldSatisfy` all ((/= "40113") . toCardCode))
        scenarioEncounterDeckNow >>= (`shouldSatisfy` any ((== "40113") . toCardCode))
    it "Ramrod's attacks pierce tough and his retaliate remains active" do
      tg <- modularGame Set.NastyBoys ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        ramrod <- addMinion p Minions.ne40114
        resolve [IdentityMessage p IdentityBecomeTough]
        runNow [MinionMessage ramrod (MinionAttacks p)]
        pass
        damageOf p `shouldReturn` 2
        host <- getsGame $ toAttrs . (HashMap.! ramrod) . gameMinions
        keywords <- Minion.getModifiedKeywords host
        keywords `shouldSatisfy` elem (Retaliate 1)
    it "Ruckus stuns every controlled character when revealed" do
      tg <- modularGame Set.NastyBoys ["spider-man"]
      inGame tg do
        let p = player1 tg
        _ally <- putIntoPlay p "01083"
        _ <- revealNow p Minions.ne40115
        ackForced
        pass
        isStunned p `shouldReturn` True
        Ally.allyStunned <$> allyIn "01083" `shouldReturn` True
    it "Ruckus's boost stuns the boosted player" do
      tg <- modularGame Set.NastyBoys ["spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- stackEncounterDeck [Minions.ne40115]
        vid <- villainIdNow
        resolve [VillainMessage vid VillainSchemes]
        isStunned p `shouldReturn` True
    it "Slab enters tough and accumulates growth, adding it only to each current attack" do
      tg <- modularGame Set.NastyBoys ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        slab <- addMinion p Minions.ne40116
        host <- getsGame $ toAttrs . (HashMap.! slab) . gameMinions
        Minion.minionTough host `shouldBe` True
        resolve [MinionMessage slab (MinionAttacks p)]
        getCountersOn slab `shouldReturn` 1
        damageOf p `shouldReturn` 2
        resolve [MinionMessage slab (MinionAttacks p)]
        getCountersOn slab `shouldReturn` 2
        damageOf p `shouldReturn` 5
        hostAfter <- getsGame $ toAttrs . (HashMap.! slab) . gameMinions
        Minion.getModifiedAttack hostAfter `shouldReturn` 1
    it "Get Nasty boosts all minions and adds extra threat before finding a Nasty Boy" do
      tg <- modularGame Set.NastyBoys ["spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- addMinion p Minions.ne40114
        _ <- addMinion p Minions.hydraMercenary
        scheme <- sideSchemeOf <$> revealNow p Schemes.ne40117
        settle
        sideSchemeThreat scheme `shouldReturn` 6
        minions <- getsGame (map toAttrs . HashMap.elems . gameMinions)
        for_ minions $ \m -> do
          attack <- Minion.getModifiedAttack m
          attack `shouldSatisfy` (>= 1)

  describe "Extreme Measures" do
    it "Strobe lets the player stun all controlled characters" do
      tg <- modularGame Set.ExtremeMeasures ["spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- putIntoPlay p "01083"
        _ <- revealNow p Minions.ne40180
        choose "stun everyone" (isLabelStarting "$label.neStunEachCharacter")
        pass
        isStunned p `shouldReturn` True
        Ally.allyStunned <$> allyIn "01083" `shouldReturn` True
    it "Strobe's alternative damages each controlled character" do
      tg <- modularGame Set.ExtremeMeasures ["spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- putIntoPlay p "01083"
        _ <- revealNow p Minions.ne40180
        choose "damage everyone" (isLabelStarting "$label.neDamageEachCharacter")
        pass
        damageOf p `shouldReturn` 1
        Ally.allyDamage <$> allyIn "01083" `shouldReturn` 1
    it "Tempo increases only her engaged player's hand size" do
      tg <- modularGame Set.ExtremeMeasures ["spider-man", "captain-marvel"]
      inGame tg do
        before <- handSizeOf (player1 tg)
        other <- handSizeOf (player2 tg)
        _ <- addMinion (player1 tg) Minions.ne40181
        handSizeOf (player1 tg) `shouldReturn` (before + 1)
        handSizeOf (player2 tg) `shouldReturn` other
    it "Tempo discards twice the current hand size after activation" do
      tg <- modularGame Set.ExtremeMeasures ["spider-man"]
      inGame tg do
        let p = player1 tg
        clearHand p
        _ <- putInHand p "01003"
        _ <- putInHand p "01005"
        cards <- stackDeck p ["01003", "01003", "01003", "01003", "01005"]
        tempo <- addMinion p Minions.ne40181
        resolve [MinionMessage tempo MinionSchemes]
        discarded <- discardOf p
        filter (`elem` discarded) cards `shouldSatisfy` ((== 4) . length)
    it "Thumbelina returns a highest-cost upgrade" do
      tg <- modularGame Set.ExtremeMeasures ["spider-man"]
      inGame tg do
        let p = player1 tg
        upgrade <- putIntoPlay p "01057"
        _ <- revealNow p Minions.ne40182
        pass
        hand <- handOf p
        hand `shouldSatisfy` elem upgrade
    it "Wildside can return the highest-cost support instead of attacking" do
      tg <- modularGame Set.ExtremeMeasures ["spider-man"]
      inGame tg do
        let p = player1 tg
        support <- putIntoPlay p "01091"
        _ <- revealNow p Minions.ne40183
        choose "return support" (isLabelStarting "$label.neReturnHighestSupport")
        pass
        hand <- handOf p
        hand `shouldSatisfy` elem support
        damageOf p `shouldReturn` 0
    it "Thumbelina reduces a non-tiny hero attack by one" do
      tg <- modularGame Set.ExtremeMeasures ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        thumbelina <- addMinion p Minions.ne40182
        choose "basic attack" (\case UseAbility ab -> abilitySource ab == toSource p && abilityIndex ab == 301; _ -> False)
        choose "attack Thumbelina" (isTargetChoice thumbelina)
        pass
        minionDamageOf thumbelina `shouldReturn` Just 1
    it "Wildside attacks when that branch is chosen" do
      tg <- modularGame Set.ExtremeMeasures ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        _ <- revealNow p Minions.ne40183
        choose "Wildside attacks" (isLabelStarting "$label.neWildsideAttacks")
        pass
        damageOf p `shouldReturn` 3
    it "Extreme Measures enters with its base threat plus hinder" do
      tg <- modularGame Set.ExtremeMeasures ["spider-man", "captain-marvel"]
      inGame tg do
        scheme <- sideSchemeOf <$> revealNow (player1 tg) Schemes.ne40184
        pass
        sideSchemeThreat scheme `shouldReturn` 7
    it "Extreme Measures damages the controller when a card is put into play" do
      tg <- modularGame Set.ExtremeMeasures ["spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- revealNow p Schemes.ne40184
        pass
        _ <- putIntoPlay p "01091"
        settle
        damageOf p `shouldReturn` 4
    it "Extreme Measures charges an attached upgrade's controller rather than its payer" do
      tg <- modularGame Set.ExtremeMeasures ["spider-man", "captain-marvel"]
      inGame tg do
        let p = player1 tg; other = player2 tg
        asHero p
        ally <- putIntoPlay other "01083"
        _ <- revealNow p Schemes.ne40184
        pass
        sidearm <- putInHand p "40030"
        payment <- putInHand p "01003"
        playOn sidearm (allyIdOf ally) [payment]
        settle
        damageOf p `shouldReturn` 0
        otherDamage <- damageOf other
        allyDamage <- Ally.allyDamage <$> allyIn "01083"
        otherDamage + allyDamage `shouldBe` 1
  describe "Mutant Insurrection" do
    it "Dragoness counts printed energy resources for this activation only" do
      tg <- modularGame Set.MutantInsurrection ["spider-man"]
      inGame tg do
        let p = player1 tg
        clearHand p
        _ <- putInHand p "01002"
        _ <- putInHand p "01002"
        dragoness <- addMinion p Minions.ne40185
        resolve [MinionMessage dragoness MinionSchemes]
        mainSchemeThreatNow `shouldReturn` 3
        attrs <- getsGame $ toAttrs . (HashMap.! dragoness) . gameMinions
        Minion.getModifiedScheme attrs `shouldReturn` 1
    it "Forearm discards cards equal to printed physical icons after attacking" do
      tg <- modularGame Set.MutantInsurrection ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        _ <- putInHand p "01003"
        _ <- putInHand p "01003"
        cards <- stackDeck p ["01005", "01005", "01005"]
        forearm <- addMinion p Minions.ne40186
        resolve [MinionMessage forearm (MinionAttacks p)]
        discarded <- discardOf p
        filter (`elem` discarded) cards `shouldSatisfy` ((== 2) . length)
    it "Reaper's thresholds stun at two mental icons without exhausting" do
      tg <- modularGame Set.MutantInsurrection ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        _ <- putInHand p "01005"
        _ <- putInHand p "01005"
        reaper <- addMinion p Minions.ne40187
        resolve [MinionMessage reaper (MinionAttacks p)]
        isStunned p `shouldReturn` True
        isExhaustedPlayer p `shouldReturn` False
    it "Reaper exhausts as well as stunning at four mental icons" do
      tg <- modularGame Set.MutantInsurrection ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        replicateM_ 4 (putInHand p "01005")
        reaper <- addMinion p Minions.ne40187
        resolve [MinionMessage reaper (MinionAttacks p)]
        isStunned p `shouldReturn` True
        isExhaustedPlayer p `shouldReturn` True
    it "Samurai accumulates charges and offers their damage after each attack" do
      tg <- modularGame Set.MutantInsurrection ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        samurai <- addMinion p Minions.ne40188
        runNow [MinionMessage samurai (MinionAttacks p)]
        ackForced
        choose "take charge damage" (isLabelStarting "$label.neSamuraiDamage")
        pass
        getCountersOn samurai `shouldReturn` 1
        damageOf p `shouldReturn` 3
    it "Mutant Insurrection counts MLF characters and grants toughness to newly entering minions" do
      tg <- modularGame Set.MutantInsurrection ["spider-man"]
      inGame tg do
        let p = player1 tg
        _ <- addMinion p Minions.ne40185
        scheme <- sideSchemeOf <$> revealNow p Schemes.ne40189
        pass
        sideSchemeThreat scheme `shouldReturn` 5
        minion <- addMinion p Minions.hydraMercenary
        attrs <- getsGame $ toAttrs . (HashMap.! minion) . gameMinions
        Minion.minionTough attrs `shouldBe` True

    it "Reaper does not stun or exhaust below two mental icons" do
      tg <- modularGame Set.MutantInsurrection ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        _ <- putInHand p "01005"
        reaper <- addMinion p Minions.ne40187
        resolve [MinionMessage reaper (MinionAttacks p)]
        isStunned p `shouldReturn` False
        isExhaustedPlayer p `shouldReturn` False
    it "Forearm does not discard when the hand has no physical resources" do
      tg <- modularGame Set.MutantInsurrection ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        _ <- putInHand p "01005"
        cards <- stackDeck p ["01005", "01005"]
        forearm <- addMinion p Minions.ne40186
        resolve [MinionMessage forearm (MinionAttacks p)]
        discarded <- discardOf p
        filter (`elem` discarded) cards `shouldBe` []
    it "Samurai can discard a qualifying card instead of taking charge damage" do
      tg <- modularGame Set.MutantInsurrection ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        support <- putIntoPlay p "01091"
        samurai <- addMinion p Minions.ne40188
        runNow [MinionMessage samurai (MinionAttacks p)]
        ackForced
        choose "discard qualifying card" (isLabelStarting "$label.neSamuraiDiscard")
        pass
        discarded <- discardOf p
        discarded `shouldSatisfy` elem support
        damageOf p `shouldReturn` 2
    it "Samurai omits the discard option when no controlled card qualifies" do
      tg <- modularGame Set.MutantInsurrection ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        samurai <- addMinion p Minions.ne40188
        runNow [MinionMessage samurai (MinionAttacks p)]
        ackForced
        offers (isLabelStarting "$label.neSamuraiDiscard") `shouldReturn` False
        choose "take damage" (isLabelStarting "$label.neSamuraiDamage")
        pass

    it "Samurai can discard a cost-one controlled player side scheme without defeating it" do
      tg <- modularGame Set.MutantInsurrection ["spider-man"]
      inGame tg do
        let p = player1 tg
        asHero p
        scheme <- putIntoPlay p "40059"
        samurai <- addMinion p Minions.ne40188
        runNow [MinionMessage samurai (MinionAttacks p)]
        ackForced
        choose "discard player side scheme" (isLabelStarting "$label.neSamuraiDiscard")
        pass
        discarded <- discardOf p
        discarded `shouldSatisfy` elem scheme
        let sid = SideSchemeId $ unCardId $ pcCardId scheme
        getsGame (HashMap.member sid . gameSideSchemes) `shouldReturn` False
        damageOf p `shouldReturn` 2
