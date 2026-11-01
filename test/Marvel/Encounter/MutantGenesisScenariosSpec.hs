module Marvel.Encounter.MutantGenesisScenariosSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Phase (VillainPhaseStep (StepEnemiesActivate))
import Marvel.Ally.Types qualified as Ally
import Marvel.Ally.Cards qualified as Allies
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Keyword qualified as Keyword
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Minion.Cards qualified as Minions
import Marvel.Matchers
import Marvel.Query
import Marvel.Status
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Modifier
import Marvel.SideScheme.Types (sideSchemeHeldCards)
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import TestImport

spec :: Spec
spec = describe "Mutant Genesis scenarios" do
  describe "scenario setup" do
    it "starts Sabretooth with Find the Senator and a living Robert Kelly" do
      tg <- testGameAgainst "32060" ["spider-man"]
      inGame tg do
        getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameSideSchemes) >>= (`shouldSatisfy` elem "32065a")
        getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameAllies) >>= (`shouldSatisfy` elem "32066")
        toCardCode . getCardDef <$> getVillain `shouldReturn` "32060"

    it "starts Wideawake with both objectives, and keeps Captives out of the encounter deck" do
      tg <- testGameAgainst "32084" ["spider-man"]
      inGame tg do
        getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameSideSchemes) >>= (`shouldSatisfy` (\xs -> all (`elem` xs) ["32104", "32088a"]))
        aside <- getSetAsidePlayerCards
        map toCardCode aside `shouldSatisfy` (\xs -> all (`elem` xs) ["32089", "32090", "32091", "32092"])

    it "gives every player a Sentinel minion at Master Mold setup" do
      tg <- testGameAgainst "32109" ["spider-man", "captain-marvel"]
      inGame tg do
        for_ tg.seats \p -> selectAny (MinionEngagedWith (IdentityWithId p) <> MinionWithTrait Trait.Sentinel) `shouldReturn` True
        selectAny (AllyIs Allies.mgCampaignMagneto) `shouldReturn` True

    it "begins Mansion Attack at one random location and one Brotherhood villain" do
      tg <- testGameAgainst "32121a" ["spider-man"]
      inGame tg do
        schemes <- getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameMainSchemes)
        length schemes `shouldBe` 1
        schemes `shouldSatisfy` all (`elem` ["32126", "32127", "32128", "32129"])
        getsGame (length . gameVillains) `shouldReturn` 1

    it "reveals Boarding Party at Magneto setup and caps sustained damage" do
      tg <- testGameAgainst "32138" ["spider-man"]
      inGame tg do
        selectAny (SideSchemeIs Schemes.mgBoardingParty) `shouldReturn` True
        vid <- villainIdNow
        getModifiers (VillainRef vid) >>= (`shouldSatisfy` elem (MaximumSustainedDamage 6))

  describe "modular minions" do
    it "Mystique prevents attacks against the villain" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- addMinion (player1 tg) Minions.mgMystique
        vid <- villainIdNow
        getModifiers (VillainRef vid) >>= (`shouldSatisfy` elem CannotBeAttacked)

    it "Nimrod takes only three total damage in one phase" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.mgNimrod
        resolve [MinionMessage mid (MinionDamaged (toSource $ player1 tg) $ toDamage 2 FromAbility)]
        resolve [MinionMessage mid (MinionDamaged (toSource $ player1 tg) $ toDamage 2 FromAbility)]
        minionDamageOf mid `shouldReturn` Just 3

    it "Nimrod's allowance is not restored by healing during the same phase" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.mgNimrod
        resolve [MinionMessage mid (MinionDamaged (toSource $ player1 tg) $ toDamage 3 FromAbility)]
        resolve [MinionMessage mid (MinionHealed 2)]
        resolve [MinionMessage mid (MinionDamaged (toSource $ player1 tg) $ toDamage 3 FromAbility)]
        minionDamageOf mid `shouldReturn` Just 1

    it "Senyaka's attacks have piercing" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.mgSenyaka
        getModifiers (MinionRef mid) >>= (`shouldSatisfy` elem (KeywordModifier Keyword.Piercing))

  describe "attachments" do
    it "Adaptive Armor adds eight hit points to the villain" do
      tg <- testGame ["spider-man"]
      inGame tg do
        vid <- villainIdNow
        _ <- revealNow (player1 tg) Attachments.mgAdaptiveArmor
        getModifiers (VillainRef vid) >>= (`shouldSatisfy` elem (HitPointModifier 8))

    it "Magneto's Helmet prevents confusion without preventing stun" do
      tg <- testGame ["spider-man"]
      inGame tg do
        vid <- villainIdNow
        _ <- revealNow (player1 tg) Attachments.mgMagnetosHelmet
        resolve [VillainMessage vid (VillainConfused $ toSource $ player1 tg)]
        Villain.villainConfused . toAttrs <$> getVillain `shouldReturn` False
        resolve [VillainMessage vid (VillainStunned $ toSource $ player1 tg)]
        Villain.villainStunned . toAttrs <$> getVillain `shouldReturn` True

    it "Wrapped in Metal blocks all four basic powers" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- revealNow (player1 tg) Attachments.mgWrappedInMetal
        mods <- getModifiers (IdentityRef $ player1 tg)
        mods `shouldSatisfy` (\xs -> all (`elem` xs) [CannotAttack, CannotThwart, CannotDefend, CannotRecover])

  describe "Magneto counter errata" do
    it "spends three magnet counters before revealing a Magnetic card" do
      tg <- testGameAgainst "32138" ["spider-man"]
      inGame tg do
        _ <- setEncounterDeck [Treacheries.mgMetalShards, Treacheries.advance] []
        mid <- mainSchemeIdNow
        runNow [PlaceCounters (MainSchemeRef mid) 3]
        pass
        getsGame gameState `shouldReturn` InProgress
        getCountersOn (MainSchemeRef mid) `shouldReturn` 1

  describe "encounter effects" do
    it "Self-Repair removes statuses, gives tough, and heals five" do
      tg <- testGame ["spider-man"]
      inGame tg do
        vid <- villainIdNow
        resolve [VillainMessage vid (VillainDamaged (toSource $ player1 tg) $ toDamage 7 FromAbility), VillainMessage vid (VillainStunned $ toSource $ player1 tg), VillainMessage vid (VillainConfused $ toSource $ player1 tg)]
        _ <- revealNow (player1 tg) Treacheries.mgSelfRepair
        v <- getVillain
        Villain.villainDamage v `shouldBe` 2
        Villain.villainTough (toAttrs v) `shouldBe` True
        Villain.villainStunned (toAttrs v) `shouldBe` False
        Villain.villainConfused (toAttrs v) `shouldBe` False

  describe "Teamwork current ruling" do
    it "does not activate the first Acolyte when no other Acolyte is in play" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _mid <- mainSchemeIdNow
        before <- mainSchemeThreatNow
        _ <- addMinion (player1 tg) Minions.mgSenyaka
        mainSchemeThreatNow `shouldReturn` before

    it "activates only the entering Acolyte, leaving the existing Acolyte idle" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- addMinion (player1 tg) Minions.mgSenyaka
        before <- mainSchemeThreatNow
        _ <- addMinion (player1 tg) Minions.mgUnuscione
        mainSchemeThreatNow `shouldReturn` before + 2

  describe "Acolyte defeat abilities" do
    it "Zeal for the Cause resolves defeat abilities without defeating its Acolytes" do
      tg <- testGame ["spider-man"]
      inGame tg do
        amelia <- addMinion (player1 tg) Minions.mgAmeliaVoght
        unuscione <- addMinion (player1 tg) Minions.mgUnuscione
        _ <- revealNow (player1 tg) Treacheries.mgZealForTheCause
        choose "resolve Amelia's defeat ability first" (isTargetChoice $ MinionRef amelia)
        chooseIf (isTargetChoice $ MinionRef unuscione)
        selectAny (IdentityWithId (player1 tg) <> ConfusedIdentity) `shouldReturn` True
        selectAny (MinionWithId amelia) `shouldReturn` True
        selectAny (MinionWithId unuscione) `shouldReturn` True
        Villain.villainTough . toAttrs <$> getVillain `shouldReturn` True

    it "Amelia places two threat instead of confusing an already confused player" do
      tg <- testGame ["spider-man"]
      inGame tg do
        amelia <- addMinion (player1 tg) Minions.mgAmeliaVoght
        resolve [GiveStatus (toRef $ player1 tg) ConfusedStatus]
        before <- mainSchemeThreatNow
        resolve [MinionMessage amelia MinionDefeated]
        mainSchemeThreatNow `shouldReturn` before + 2

    it "Unuscione heals four if the villain is already tough" do
      tg <- testGame ["spider-man"]
      inGame tg do
        unuscione <- addMinion (player1 tg) Minions.mgUnuscione
        vid <- villainIdNow
        resolve [VillainMessage vid (VillainDamaged (toSource $ player1 tg) $ toDamage 6 FromAbility), VillainMessage vid VillainBecomeTough]
        resolve [MinionMessage unuscione MinionDefeated]
        Villain.villainDamage <$> getVillain `shouldReturn` 2

  describe "villain attachment effects" do
    for_
      [ (Attachments.mgAdamantiumClaws, [AttackModifier 1, KeywordModifier Keyword.Piercing])
      , (Attachments.mgAnimalFerocity, [KeywordModifier Keyword.Stalwart])
      , (Attachments.mgGauntletBeam, [AttackModifier 1, KeywordModifier Keyword.Piercing, KeywordModifier Keyword.Ranged])
      , (Attachments.mgLearningAI, [SchemeModifier 1, KeywordModifier $ Keyword.Retaliate 1])
      , (Attachments.mgMagnetosArmor, [AttackModifier 1, CannotBeStunned])
      , (Attachments.mgMagneticBubble, [KeywordModifier $ Keyword.Retaliate 1])
      ] \(def, expected) ->
        it ("applies the attached villain's actual modifiers: " <> show (toCardCode def)) do
          tg <- testGame ["spider-man"]
          inGame tg do
            _ <- revealNow (player1 tg) def
            vid <- villainIdNow
            mods <- getModifiers (VillainRef vid)
            mods `shouldSatisfy` (\xs -> all (`elem` xs) expected)

    it "Magneto's Armor prevents stun while still allowing confusion" do
      tg <- testGame ["spider-man"]
      inGame tg do
        vid <- villainIdNow
        _ <- revealNow (player1 tg) Attachments.mgMagnetosArmor
        resolve [VillainMessage vid (VillainStunned $ toSource $ player1 tg)]
        Villain.villainStunned . toAttrs <$> getVillain `shouldReturn` False
        resolve [VillainMessage vid (VillainConfused $ toSource $ player1 tg)]
        Villain.villainConfused . toAttrs <$> getVillain `shouldReturn` True

    it "Magnetic Bubble absorbs the whole packet and discards without spillover" do
      tg <- testGame ["spider-man"]
      inGame tg do
        vid <- villainIdNow
        _ <- revealNow (player1 tg) Attachments.mgMagneticBubble
        messages <- Lifted.runQueueT $ Lifted.capture $ Lifted.dealDamage vid (player1 tg) (toDamage 12 FromAbility)
        resolve messages
        Villain.villainDamage <$> getVillain `shouldReturn` 0
        selectAny (AttachmentIs Attachments.mgMagneticBubble) `shouldReturn` False

  describe "Sentinel attachments" do
    for_
      [ (Attachments.mgHomoSuperior, [HitPointModifier 5])
      , (Attachments.mgEnergyBarrier, [AttackModifier 2])
      , (Attachments.mgUnitUpgrade, [AttackModifier 1, SchemeModifier 1, HitPointModifier 2, KeywordModifier $ Keyword.Retaliate 1])
      , (Attachments.mgStunBeam, [AttackModifier 1])
      ] \(def, expected) ->
        it ("modifies the chosen minion after being revealed: " <> show (toCardCode def)) do
          tg <- testGame ["spider-man"]
          inGame tg do
            sentinel <- addMinion (player1 tg) Minions.mgSentinelMarkIv
            _ <- revealNow (player1 tg) def
            mods <- getModifiers (MinionRef sentinel)
            mods `shouldSatisfy` (\xs -> all (`elem` xs) expected)

    for_ [Attachments.mgHomoSuperior, Attachments.mgEnergyBarrier] \def ->
      it ("gives the attached minion tough: " <> show (toCardCode def)) do
        tg <- testGame ["spider-man"]
        inGame tg do
          sentinel <- addMinion (player1 tg) Minions.mgSentinelMarkIv
          _ <- revealNow (player1 tg) def
          selectAny (CharacterWithId (MinionCharacter sentinel) <> CharacterWithStatus ToughStatus) `shouldReturn` True

  describe "side scheme modifiers and revealed threat" do
    it "The Brotherhood grants quickstrike to Brotherhood minions" do
      tg <- testGame ["spider-man"]
      inGame tg do
        blob <- addMinion (player1 tg) Minions.mgBlob
        _ <- revealNow (player1 tg) Schemes.mgTheBrotherhood
        getModifiers (MinionRef blob) >>= (`shouldSatisfy` elem (KeywordModifier Keyword.Quickstrike))

    it "The Acolytes grants guard only to Acolyte minions" do
      tg <- testGame ["spider-man"]
      inGame tg do
        senyaka <- addMinion (player1 tg) Minions.mgSenyaka
        blob <- addMinion (player1 tg) Minions.mgBlob
        _ <- revealNow (player1 tg) Schemes.mgTheAcolytes
        getModifiers (MinionRef senyaka) >>= (`shouldSatisfy` elem (KeywordModifier Keyword.Guard))
        getModifiers (MinionRef blob) >>= (`shouldSatisfy` notElem (KeywordModifier Keyword.Guard))

    it "Magnetically Sealed adds two threat per ally, including other players' allies" do
      tg <- testGame ["spider-man", "captain-marvel"]
      inGame tg do
        _ <- putIntoPlay (player1 tg) "01083"
        _ <- putIntoPlay (player2 tg) "01084"
        card <- revealNow (player1 tg) Schemes.mgMagneticallySealed
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 6

    it "Under Siege counts both Brotherhood villains and minions" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- addMinion (player1 tg) Minions.mgBlob
        card <- revealNow (player1 tg) Schemes.mgUnderSiege
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 3

  describe "captured cards" do
    it "Seized places six top cards from each player's deck under itself" do
      tg <- testGame ["spider-man", "captain-marvel"]
      inGame tg do
        card <- revealNow (player1 tg) Schemes.mgSeized
        getsGame (fmap (length . sideSchemeHeldCards . toAttrs) . HashMap.lookup (sideSchemeOf card) . gameSideSchemes) `shouldReturn` Just 12

    it "Bastion's Machinations captures nine top cards from each deck" do
      tg <- testGame ["spider-man", "captain-marvel"]
      inGame tg do
        card <- revealNow (player1 tg) Schemes.mgBastionsMachinations
        getsGame (fmap (length . sideSchemeHeldCards . toAttrs) . HashMap.lookup (sideSchemeOf card) . gameSideSchemes) `shouldReturn` Just 18

  describe "Brotherhood attack riders" do
    it "Avalanche exhausts a controlled character after attacking" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.mgAvalanche
        resolve [CheckWindows [W.Window W.After $ W.EnemyAttack (EnemyMinionId mid) (player1 tg)]]
        isExhaustedNow (player1 tg) `shouldReturn` True

    it "Blob stuns an ally damaged by his attack" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.mgBlob
        ally <- putIntoPlay (player1 tg) "01084"
        resolve [CheckWindows [W.Window W.After $ W.EnemyAttacksAndDamages (EnemyMinionId mid) (AllyCharacter $ allyIdOf ally) 1]]
        selectAny (CharacterWithId (AllyCharacter $ allyIdOf ally) <> CharacterWithStatus StunnedStatus) `shouldReturn` True

    it "Blob does not stun when his attack deals no damage" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.mgBlob
        resolve [CheckWindows [W.Window W.After $ W.EnemyAttack (EnemyMinionId mid) (player1 tg)]]
        selectAny (IdentityWithId (player1 tg) <> StunnedIdentity) `shouldReturn` False

    it "Pyro counts all printed resource icons on the two discarded cards" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.mgPyro
        _ <- stackDeck (player1 tg) ["01088", "01089"]
        before <- damageOf (player1 tg)
        resolve [CheckWindows [W.Window W.After $ W.EnemyAttack (EnemyMinionId mid) (player1 tg)]]
        damageOf (player1 tg) `shouldReturn` before + 4

    it "Toad discards one random card only after damaging a controlled character" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.mgToad
        before <- length <$> handOf (player1 tg)
        resolve [CheckWindows [W.Window W.After $ W.EnemyAttacksAndDamages (EnemyMinionId mid) (IdentityCharacter $ player1 tg) 1]]
        length <$> handOf (player1 tg) `shouldReturn` before - 1

    it "Toad does not discard a card after an attack that dealt no damage" do
      tg <- testGame ["spider-man"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.mgToad
        before <- length <$> handOf (player1 tg)
        resolve [CheckWindows [W.Window W.After $ W.EnemyAttack (EnemyMinionId mid) (player1 tg)]]
        length <$> handOf (player1 tg) `shouldReturn` before

  describe "Acolyte defeat timing" do
    it "Fabian's replacement does not use defeated Fabian to trigger teamwork" do
      tg <- testGame ["spider-man"]
      inGame tg do
        fabian <- addMinion (player1 tg) Minions.mgFabianCortez
        _ <- setEncounterDeck [Minions.mgSenyaka, Treacheries.advance] []
        before <- mainSchemeThreatNow
        resolve [MinionMessage fabian MinionDefeated]
        selectAny (MinionWithId fabian) `shouldReturn` False
        selectAny (MinionIs Minions.mgSenyaka) `shouldReturn` True
        mainSchemeThreatNow `shouldReturn` before

    it "Zeal leaves Fabian in play so his replacement can trigger teamwork" do
      tg <- testGame ["spider-man"]
      inGame tg do
        fabian <- addMinion (player1 tg) Minions.mgFabianCortez
        _ <- setEncounterDeck [Minions.mgSenyaka, Treacheries.advance] []
        before <- mainSchemeThreatNow
        _ <- revealNow (player1 tg) Treacheries.mgZealForTheCause
        choose "resolve Fabian's defeat ability" (isTargetChoice $ MinionRef fabian)
        selectAny (MinionWithId fabian) `shouldReturn` True
        [replacement] <- selectList (MinionIs Minions.mgSenyaka)
        choose "resolve the replacement Senyaka's teamwork" (isAbilityOf $ MinionRef replacement)
        mainSchemeThreatNow `shouldReturn` before + 1

  describe "Magneto objective chain" do
    it "Boarding Party limits total sustained damage to six per player" do
      tg <- testGameAgainst "32138" ["spider-man"]
      inGame tg do
        vid <- villainIdNow
        resolve [RemoveStatus (VillainRef vid) ToughStatus]
        resolve [VillainMessage vid (VillainDamaged (toSource $ player1 tg) $ toDamage 15 FromAbility)]
        Villain.villainDamage <$> getVillain `shouldReturn` 6
        resolve [VillainMessage vid (VillainDamaged (toSource $ player1 tg) $ toDamage 4 FromAbility)]
        Villain.villainDamage <$> getVillain `shouldReturn` 6

    it "defeating Boarding Party raises the sustained-damage ceiling to twelve" do
      tg <- testGameAgainst "32138" ["spider-man"]
      inGame tg do
        [boarding] <- selectList (SideSchemeIs Schemes.mgBoardingParty)
        resolve [SideSchemeMessage boarding DefeatSideScheme]
        selectAny (SideSchemeIs Schemes.mgSabotageMasterMold) `shouldReturn` True
        vid <- villainIdNow
        getModifiers (VillainRef vid) >>= (`shouldSatisfy` elem (MaximumSustainedDamage 12))

    it "defeating Sabotage Master Mold reveals Orbital Decay and records victory" do
      tg <- testGameAgainst "32138" ["spider-man"]
      inGame tg do
        [boarding] <- selectList (SideSchemeIs Schemes.mgBoardingParty)
        resolve [SideSchemeMessage boarding DefeatSideScheme]
        [sabotage] <- selectList (SideSchemeIs Schemes.mgSabotageMasterMold)
        resolve [SideSchemeMessage sabotage DefeatSideScheme]
        selectAny (SideSchemeIs Schemes.mgOrbitalDecay) `shouldReturn` True
        getVictoryDisplay >>= (`shouldSatisfy` any ((== "32144b") . toCardCode))
        vid <- villainIdNow
        getModifiers (VillainRef vid) >>= (`shouldSatisfy` elem (MaximumSustainedDamage 18))

    it "Physical Strain removes steady without discarding the permanent objective reward" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- revealNow (player1 tg) Attachments.mgPhysicalStrain
        vid <- villainIdNow
        getModifiers (VillainRef vid) >>= (`shouldSatisfy` elem (LoseKeyword Keyword.Steady))
        selectAny (AttachmentIs Attachments.mgPhysicalStrain) `shouldReturn` True

  describe "Relentless Robots" do
    it "keeps an engaged player's thwart from removing threat" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- addMinion (player1 tg) Minions.mgSentinelMarkIv
        card <- revealNow (player1 tg) Schemes.mgRelentlessRobots
        before <- sideSchemeThreat (sideSchemeOf card)
        resolve [SideSchemeMessage (sideSchemeOf card) (SideSchemeThwarted (toSource $ player1 tg) 2)]
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` before

    it "allows encounter threat removal even while that player is engaged" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- addMinion (player1 tg) Minions.mgSentinelMarkIv
        card <- revealNow (player1 tg) Schemes.mgRelentlessRobots
        resolve [SideSchemeMessage (sideSchemeOf card) (SideSchemeThreatRemoved GameRef 1)]
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 2

  describe "Stalked by Sabretooth villain-phase response" do
    it "damages Kelly for two after step one when threat is below six per player" do
      tg <- testGameAgainst "32060" ["spider-man"]
      inGame tg do
        resolve [AccelerateMainScheme, VillainPhaseStep StepEnemiesActivate]
        senators <- getsGame (filter ((== "32066") . toCardCode . getCardDef) . HashMap.elems . gameAllies)
        map (Ally.allyDamage . toAttrs) senators `shouldBe` [2]

    it "uses threat after step one so reaching six threat deals three damage" do
      tg <- testGameAgainst "32060" ["spider-man"]
      inGame tg do
        placeMainThreat 5
        resolve [AccelerateMainScheme, VillainPhaseStep StepEnemiesActivate]
        mainSchemeThreatNow `shouldReturn` 6
        senators <- getsGame (filter ((== "32066") . toCardCode . getCardDef) . HashMap.elems . gameAllies)
        map (Ally.allyDamage . toAttrs) senators `shouldBe` [3]

  describe "Master Mold's activation" do
    it "finds a Sentinel before scheming and does not consume a normal boost card" do
      tg <- testGameAgainst "32109" ["spider-man"]
      inGame tg do
        current <- selectList AnyMinion
        resolve [RemoveFromGame (MinionRef mid) | mid <- current]
        _ <- setEncounterDeck [Minions.mgSentinelMarkIv, Treacheries.advance] []
        before <- mainSchemeThreatNow
        vid <- villainIdNow
        resolve [VillainMessage vid (VillainSchemesAgainst $ player1 tg)]
        selectAny (MinionIs Minions.mgSentinelMarkIv <> MinionEngagedWith (IdentityWithId $ player1 tg)) `shouldReturn` True
        mainSchemeThreatNow `shouldReturn` before + 1
        map toCardCode <$> getEncounterDeck `shouldReturn` ["01186"]

    it "cannot be confused because of stalwart" do
      tg <- testGameAgainst "32109" ["spider-man"]
      inGame tg do
        current <- selectList AnyMinion
        resolve [RemoveFromGame (MinionRef mid) | mid <- current]
        vid <- villainIdNow
        _ <- setEncounterDeck [Minions.mgSentinelMarkIv, Treacheries.advance] []
        before <- mainSchemeThreatNow
        resolve [VillainMessage vid (VillainConfused $ toSource $ player1 tg)]
        Villain.villainConfused . toAttrs <$> getVillain `shouldReturn` False
        mainSchemeThreatNow `shouldReturn` before

  describe "Sabretooth healing" do
    it "heals discarded boost icons after a completed scheme" do
      tg <- testGameAgainst "32060" ["spider-man"]
      inGame tg do
        vid <- villainIdNow
        resolve [VillainMessage vid (VillainDamaged (toSource $ player1 tg) $ toDamage 5 FromAbility)]
        _ <- setEncounterDeck [Treacheries.advance, Schemes.mgFeralRage, Treacheries.assault] []
        resolve [VillainMessage vid (VillainSchemesAgainst $ player1 tg)]
        Villain.villainDamage <$> getVillain `shouldReturn` 2

    it "does not heal or discard a card when confusion replaces his scheme" do
      tg <- testGameAgainst "32060" ["spider-man"]
      inGame tg do
        vid <- villainIdNow
        resolve [VillainMessage vid (VillainDamaged (toSource $ player1 tg) $ toDamage 5 FromAbility), VillainMessage vid (VillainConfused $ toSource $ player1 tg)]
        _ <- setEncounterDeck [Schemes.mgFeralRage, Treacheries.assault] []
        before <- getEncounterDeck
        resolve [VillainMessage vid (VillainSchemesAgainst $ player1 tg)]
        Villain.villainDamage <$> getVillain `shouldReturn` 5
        getEncounterDeck `shouldReturn` before

  describe "Pyro discards across deck exhaustion" do
    it "counts both discarded cards after reshuffling a one-card deck" do
      tg <- testGame ["spider-man"]
      inGame tg do
        pyro <- addMinion (player1 tg) Minions.mgPyro
        [first, second] <- stackDeck (player1 tg) ["01088", "01089"]
        withGame_ $ entitiesL . playersL . ix (player1 tg) %~ \(PlayerIdentity a) -> PlayerIdentity a {playerIdentityDeck = Deck [first], playerIdentityDiscard = Discard [second]}
        before <- damageOf (player1 tg)
        resolve [CheckWindows [W.Window W.After $ W.EnemyAttack (EnemyMinionId pyro) (player1 tg)]]
        damageOf (player1 tg) `shouldReturn` before + 4

    it "also counts both cards for the villain Pyro's attack rider" do
      tg <- testGame ["spider-man"]
      inGame tg do
        vid <- villainIdNow
        resolve [ReplaceVillain vid "32123a" Advanced]
        [first, second] <- stackDeck (player1 tg) ["01088", "01089"]
        withGame_ $ entitiesL . playersL . ix (player1 tg) %~ \(PlayerIdentity a) -> PlayerIdentity a {playerIdentityDeck = Deck [first], playerIdentityDiscard = Discard [second]}
        before <- damageOf (player1 tg)
        resolve [CheckWindows [W.Window W.After $ W.EnemyAttack (EnemyVillainId vid) (player1 tg)]]
        damageOf (player1 tg) `shouldReturn` before + 4

  describe "granted keywords act when a card enters play" do
    it "The Brotherhood makes an entering Blob immediately attack the hero" do
      tg <- testGame ["spider-man"]
      inGame tg do
        changeForm (player1 tg)
        _ <- revealNow (player1 tg) Schemes.mgTheBrotherhood
        before <- damageOf (player1 tg)
        _ <- revealNow (player1 tg) Minions.mgBlob
        pass
        damageOf (player1 tg) `shouldReturn` before + 2

    it "The Basketball Court gives a newly entering ally tough" do
      tg <- testGame ["spider-man"]
      inGame tg do
        old <- mainSchemeIdNow
        resolve [ReplaceMainScheme old "32128"]
        ally <- putIntoPlay (player1 tg) "01084"
        selectAny (CharacterWithId (AllyCharacter $ allyIdOf ally) <> CharacterWithStatus ToughStatus) `shouldReturn` True

    it "The Basketball Court gives a newly entering minion tough" do
      tg <- testGame ["spider-man"]
      inGame tg do
        old <- mainSchemeIdNow
        resolve [ReplaceMainScheme old "32128"]
        mid <- addMinion (player1 tg) Minions.mgBlob
        selectAny (CharacterWithId (MinionCharacter mid) <> CharacterWithStatus ToughStatus) `shouldReturn` True

    it "The Basketball Court does not give an existing ally a tough card just by appearing" do
      tg <- testGame ["spider-man"]
      inGame tg do
        ally <- putIntoPlay (player1 tg) "01084"
        old <- mainSchemeIdNow
        resolve [ReplaceMainScheme old "32128"]
        selectAny (CharacterWithId (AllyCharacter $ allyIdOf ally) <> CharacterWithStatus ToughStatus) `shouldReturn` False

  describe "Warn the Others remains in play" do
    it "stays in the player's play area after revelation" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- revealNow (player1 tg) Obligations.mgWarnTheOthers
        getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameObligations) >>= (`shouldSatisfy` elem "32099")

    it "is tucked under Operation Zero Tolerance after that player's turn ends" do
      tg <- testGame ["spider-man"]
      inGame tg do
        zero <- revealNow (player1 tg) Schemes.mgOperationZeroTolerance
        _ <- revealNow (player1 tg) Obligations.mgWarnTheOthers
        resolve [CheckWindows [W.Window W.After $ W.TurnEnded (player1 tg)]]
        getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameObligations) >>= (`shouldSatisfy` notElem "32099")
        getsGame (fmap (length . sideSchemeHeldCards . toAttrs) . HashMap.lookup (sideSchemeOf zero) . gameSideSchemes) `shouldReturn` Just 1

  describe "Robert Kelly restrictions" do
    it "does not offer basic attack or thwart actions for either printed dash" do
      tg <- testGameAgainst "32060" ["spider-man"]
      inGame tg do
        [findSenator] <- selectList (SideSchemeIs Schemes.mgFindTheSenator)
        resolve [SideSchemeMessage findSenator DefeatSideScheme]
        kelly <- allyIn "32066"
        offers (isAllyAttack kelly) `shouldReturn` False
        offers (isAllyThwart kelly) `shouldReturn` False

    it "Medical Emergency heals Kelly despite his prohibition on player-card healing" do
      tg <- testGameAgainst "32060" ["spider-man"]
      inGame tg do
        [kelly] <- selectList (AllyIs Allies.mgRobertKelly)
        resolve [AllyMessage kelly (AllySetDamage 4)]
        card <- revealNow (player1 tg) Schemes.mgMedicalEmergency
        resolve [SideSchemeMessage (sideSchemeOf card) DefeatSideScheme]
        Ally.allyDamage <$> allyIn "32066" `shouldReturn` 2

  describe "Cannonball consequential damage" do
    it "takes one less consequential damage when his attack defeats a minion" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- putIntoPlay (player1 tg) "32091"
        cannonball <- allyIn "32091"
        bomber <- addMinion (player1 tg) Minions.hydraBomber
        choose "attack with Cannonball" (isAllyAttack cannonball)
        choose "attack the Hydra Bomber" (isTargetChoice $ MinionRef bomber)
        Ally.allyDamage <$> allyIn "32091" `shouldReturn` 0

    it "still takes consequential damage when his attack does not defeat a minion" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- putIntoPlay (player1 tg) "32091"
        cannonball <- allyIn "32091"
        choose "attack with Cannonball" (isAllyAttack cannonball)
        chooseVillain
        Ally.allyDamage <$> allyIn "32091" `shouldReturn` 1

  describe "after-attack responses when the target is defeated" do
    it "Rictor still damages the villain and other engaged minions after his target dies" do
      tg <- testGame ["spider-man"]
      inGame tg do
        card <- putIntoPlay (player1 tg) "32089"
        rictor <- allyIn "32089"
        target <- addMinion (player1 tg) Minions.hydraBomber
        resolve [MinionMessage target (MinionDamaged (toSource $ player1 tg) $ toDamage 1 FromAbility)]
        other <- addMinion (player1 tg) Minions.hydraMercenary
        before <- villainDamageNow
        choose "attack with Rictor" (isAllyAttack rictor)
        choose "defeat the Hydra Bomber" (isTargetChoice $ MinionRef target)
        selectAny (MinionWithId target) `shouldReturn` False
        choose "Rictor's response" (isAbilityOfCard card)
        villainDamageNow `shouldReturn` before + 1
        minionDamageOf other `shouldReturn` Just 1

    it "Boom Boom does not put a bomb counter on a defeated target" do
      tg <- testGame ["spider-man"]
      inGame tg do
        card <- putIntoPlay (player1 tg) "32090"
        boomBoom <- allyIn "32090"
        target <- addMinion (player1 tg) Minions.hydraBomber
        resolve [MinionMessage target (MinionDamaged (toSource $ player1 tg) $ toDamage 1 FromAbility)]
        choose "attack with Boom Boom" (isAllyAttack boomBoom)
        choose "defeat the Hydra Bomber" (isTargetChoice $ MinionRef target)
        selectAny (MinionWithId target) `shouldReturn` False
        chooseIf (isAbilityOfCard card)
        getsGame gameBombCounters `shouldReturn` []

  describe "Sentinel Mark VIII attachment targeting ruling" do
    for_
      [ (Attachments.mgAdaptiveArmor, [], [HitPointModifier 8])
      , (Attachments.mgLearningAI, [SchemeModifier 1], [KeywordModifier $ Keyword.Retaliate 1])
      , (Attachments.mgGauntletBeam, [AttackModifier 1], [KeywordModifier Keyword.Piercing, KeywordModifier Keyword.Ranged])
      ] \(def, minionBenefits, villainBenefits) ->
        it ("keeps explicit villain benefits on the villain: " <> show (toCardCode def)) do
          tg <- testGame ["spider-man"]
          inGame tg do
            _ <- setEncounterDeck [Treacheries.advance] [def]
            sentinel <- addMinion (player1 tg) Minions.mgSentinelMarkViii
            vid <- villainIdNow
            minionMods <- getModifiers (MinionRef sentinel)
            villainMods <- getModifiers (VillainRef vid)
            minionMods `shouldSatisfy` (\ms -> all (`elem` ms) minionBenefits)
            minionMods `shouldSatisfy` (\ms -> all (`notElem` ms) villainBenefits)
            villainMods `shouldSatisfy` (\ms -> all (`elem` ms) villainBenefits)

  describe "players choose encounter-effect order" do
    it "Master Mold's Children offers each engaged minion's activation to order" do
      tg <- testGame ["spider-man"]
      inGame tg do
        first <- addMinion (player1 tg) Minions.mgSentinelMarkIv
        second <- addMinion (player1 tg) Minions.mgBlob
        _ <- revealNow (player1 tg) Treacheries.mgMasterMoldsChildren
        offers (isTargetChoice $ MinionRef first) `shouldReturn` True
        offers (isTargetChoice $ MinionRef second) `shouldReturn` True
        choose "activate Blob first" (isTargetChoice $ MinionRef second)

    it "Zeal for the Cause offers each engaged Acolyte's ability to order" do
      tg <- testGame ["spider-man"]
      inGame tg do
        first <- addMinion (player1 tg) Minions.mgAmeliaVoght
        second <- addMinion (player1 tg) Minions.mgSenyaka
        _ <- revealNow (player1 tg) Treacheries.mgZealForTheCause
        offers (isTargetChoice $ MinionRef first) `shouldReturn` True
        offers (isTargetChoice $ MinionRef second) `shouldReturn` True
        choose "resolve Senyaka first" (isTargetChoice $ MinionRef second)

  describe "Operation Zero Tolerance loss condition" do
    it "loses at four captured cards with one player, without losing at three" do
      tg <- testGame ["spider-man"]
      inGame tg do
        zero <- revealNow (player1 tg) Schemes.mgOperationZeroTolerance
        messages <- Lifted.runQueueT $ Lifted.capture $ Lifted.putTopCardsUnder (player1 tg) (sideSchemeOf zero) 3
        resolve messages
        getsGame gameState `shouldReturn` InProgress
        lastCard <- Lifted.runQueueT $ Lifted.capture $ Lifted.putTopCardsUnder (player1 tg) (sideSchemeOf zero) 1
        resolve lastCard
        getsGame gameState `shouldReturn` Finished Lost

    it "raises the loss threshold to five cards for two players" do
      tg <- testGame ["spider-man", "captain-marvel"]
      inGame tg do
        zero <- revealNow (player1 tg) Schemes.mgOperationZeroTolerance
        messages <- Lifted.runQueueT $ Lifted.capture $ Lifted.putTopCardsUnder (player1 tg) (sideSchemeOf zero) 4
        resolve messages
        getsGame gameState `shouldReturn` InProgress
        lastCard <- Lifted.runQueueT $ Lifted.capture $ Lifted.putTopCardsUnder (player2 tg) (sideSchemeOf zero) 1
        resolve lastCard
        getsGame gameState `shouldReturn` Finished Lost

    it "captures an ally defeated by an enemy's attack damage" do
      tg <- testGame ["spider-man"]
      inGame tg do
        zero <- revealNow (player1 tg) Schemes.mgOperationZeroTolerance
        card <- putIntoPlay (player1 tg) "01084"
        vid <- villainIdNow
        resolve [AllyMessage (allyIdOf card) (AllyDamaged (VillainRef vid) $ toDamage 9 FromAttack)]
        held <- Lifted.heldCardsAt (sideSchemeOf zero)
        map pcCardId held `shouldSatisfy` elem (pcCardId card)
        discardOf (player1 tg) >>= (`shouldSatisfy` notElem card)

    it "does not capture an ally defeated by consequential damage" do
      tg <- testGame ["spider-man"]
      inGame tg do
        zero <- revealNow (player1 tg) Schemes.mgOperationZeroTolerance
        card <- putIntoPlay (player1 tg) "01084"
        resolve [AllyMessage (allyIdOf card) (AllyDamaged (AllyRef $ allyIdOf card) $ toDamage 9 FromConsequential)]
        held <- Lifted.heldCardsAt (sideSchemeOf zero)
        held `shouldBe` []
        discardOf (player1 tg) >>= (`shouldSatisfy` elem card)

  describe "singular villain effects in a multi-villain scenario" do
    it "Rictor's splash damages one chosen villain rather than all four Wrecking Crew members" do
      tg <- testGameAgainst "07001" ["spider-man"]
      inGame tg do
        card <- putIntoPlay (player1 tg) "32089"
        let aid = allyIdOf card
        villains <- selectList AnyVillain
        let first = fromMaybe (error "Wrecking Crew missing") (headMay villains)
        runNow [CheckWindows [W.Window W.After $ W.AllyAttack aid (EnemyVillainId first)]]
        choose "Rictor's splash response" (isAbilityOfCard card)
        chooseIf (isTargetChoice $ VillainRef first)
        damage <- getsGame (map Villain.villainDamage . HashMap.elems . gameVillains)
        sum damage `shouldBe` 1
        length (filter (> 0) damage) `shouldBe` 1

  describe "an effect that attaches a card skips its attach-to instruction" do
    it "Mark VIII attaching Energy Barrier does not give the minion tough" do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- setEncounterDeck [Treacheries.advance] [Attachments.mgEnergyBarrier]
        sentinel <- addMinion (player1 tg) Minions.mgSentinelMarkViii
        getModifiers (MinionRef sentinel) >>= (`shouldSatisfy` elem (AttackModifier 2))
        selectAny (CharacterWithId (MinionCharacter sentinel) <> CharacterWithStatus ToughStatus) `shouldReturn` False

  describe "Mutant Detected attack order" do
    it "resolves the villain first, then lets the player choose which engaged minion attacks next" do
      tg <- testGame ["spider-man"]
      inGame tg do
        first <- addMinion (player1 tg) Minions.mgBlob
        second <- addMinion (player1 tg) Minions.mgSentinelMarkIv
        vid <- villainIdNow
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.assault] []
        before <- damageOf (player1 tg)
        _ <- revealNow (player1 tg) Treacheries.mgMutantDetected
        choose "let all enemies attack" (isLabel "$label.mgAllEnemiesAttack")
        pass
        after <- damageOf (player1 tg)
        after `shouldSatisfy` (> before)
        offers (isTargetChoice $ VillainRef vid) `shouldReturn` False
        offers (isTargetChoice $ MinionRef first) `shouldReturn` True
        offers (isTargetChoice $ MinionRef second) `shouldReturn` True
        choose "let Blob attack before the other minion" (isTargetChoice $ MinionRef first)

    it "does not include another player's minion after the villain finishes attacking" do
      tg <- testGame ["spider-man", "captain-marvel"]
      inGame tg do
        own <- addMinion (player1 tg) Minions.mgBlob
        other <- addMinion (player2 tg) Minions.mgSentinelMarkIv
        vid <- villainIdNow
        _ <- setEncounterDeck [Treacheries.advance, Treacheries.assault] []
        before <- damageOf (player1 tg)
        _ <- revealNow (player1 tg) Treacheries.mgMutantDetected
        choose "let all enemies attack" (isLabel "$label.mgAllEnemiesAttack")
        pass
        after <- damageOf (player1 tg)
        after `shouldSatisfy` (> before)
        offers (isTargetChoice $ VillainRef vid) `shouldReturn` False
        offers (isTargetChoice $ MinionRef own) `shouldReturn` True
        offers (isTargetChoice $ MinionRef other) `shouldReturn` False
