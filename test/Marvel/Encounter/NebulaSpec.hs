-- | Nebula (The Galaxy's Most Wanted, scenario 4) and the Space Pirates modular set, each
-- card played as a table would.
module Marvel.Encounter.NebulaSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as AttachmentCards
import Marvel.Attachment.Types (Attrs (..))
import Marvel.EncounterSet (EncounterSet (SpacePirates))
import Marvel.Environment.Cards qualified as EnvironmentCards
import Marvel.Environment.Environments.NebulasShip (getEvasionCounters)
import Marvel.Environment.Types (Environment)
import Marvel.Keyword (Keyword (Quickstrike))
import Marvel.MainScheme.Cards qualified as MainSchemeCards
import Marvel.Minion.Cards qualified as MinionCards
import Marvel.Minion.Types qualified as Minion
import Marvel.SideScheme.Cards qualified as SideSchemeCards
import Marvel.Support.Cards qualified as SupportCards
import Marvel.Trait (Trait (Technique))
import Marvel.Treachery.Cards qualified as TreacheryCards
import Marvel.Villain.Cards qualified as VillainCards
import Marvel.Villain.Types (getModifiedAttack, villainBoostCards, villainConfused, villainStunned, villainTough)
import TestImport

nebulaGame :: [Text] -> IO TestGame
nebulaGame = testGameAgainst "16088"

-- | Nebula in expert mode: Nebula (II) and (III).
nebulaExpert :: IO TestGame
nebulaExpert = do
  (app, ran, logged) <- dealtGameLogged (ScenarioSetup "16088" [SpacePirates] Expert) [starter "spider-man"] 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

ship :: GameAppT Environment
ship = getsGame (filter ((== EnvironmentCards.nebulasShip) . getCardDef) . HashMap.elems . gameEnvironments) >>= \case
  [s] -> pure s
  ss -> error $ "expected Nebula's Ship, found " <> show (length ss)

placeEvasion :: Natural -> GameAppT ()
placeEvasion n = ship >>= \s -> resolve [PlaceCounters (toRef s) n]

-- | The Technique attachments on the villain.
techniques :: GameAppT [CardCode]
techniques = getsGame (map (toCardCode . getCardDef) . filter isTechnique . HashMap.elems . gameAttachments)
 where
  isTechnique a = Technique `member` cdTraits (getCardDef a) && isJust (attachmentEnemy (toAttrs a))

-- | Take away the Techniques the setup attached, for a test to start from none.
clearTechniques :: GameAppT ()
clearTechniques = do
  ids <- getsGame (map toId . filter ((Technique `member`) . cdTraits . getCardDef) . HashMap.elems . gameAttachments)
  resolve [RemoveFromGame (AttachmentRef aid) | aid <- ids]

-- | A Technique attached to the villain, not revealed.
attachTechnique :: CardDef -> GameAppT AttachmentId
attachTechnique def = do
  card <- encounterCard def
  vid <- villainIdNow
  resolve [AttachEncounterCard card (EnemyVillainId vid)]
  pure $ AttachmentId (unCardId (ecCardId card))

resolveSpecialNow :: AttachmentId -> IdentityId -> GameAppT ()
resolveSpecialNow aid you = resolve [cardSpecific (AttachmentRef aid) "techniqueSpecial" you]

villainAttrs :: GameAppT (Attrs Villain)
villainAttrs = toAttrs <$> getVillain

villainCode :: GameAppT CardCode
villainCode = toCardCode . getCardDef <$> getVillain

-- | An attack of the hero's on the villain, of this much damage.
heroAttackFor :: IdentityId -> Natural -> GameAppT ()
heroAttackFor you n = do
  vid <- villainIdNow
  runNow [Ask you (ChooseOne [DamageEnemy (EnemyRef (EnemyVillainId vid)) (toSource you) (toDamage n (FromPlayerAttack you))])]
  choose "the attack" (const True)
  resolve []

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass

inDiscard :: CardDef -> GameAppT Bool
inDiscard def = elem (toCardCode def) <$> encounterDiscardCodes

spec :: Spec
spec = do
  setupSpec
  shipSpec
  villainSpec
  techniqueSpec
  encounterSpec
  spacePiratesSpec

setupSpec :: Spec
setupSpec = describe "Nebula: setup" do
  it "puts Nebula's Ship and the Milano into play, the Power Stone on Nebula, and attaches each Technique among the top 2 per player discarded" do
    tg <- nebulaGame ["spider-man"]
    inGame tg do
      s <- ship
      counters <- getEvasionCounters
      milanos <- supportsWith (toCardCode SupportCards.milano)
      vid <- villainIdNow
      holder <- getPowerStoneHolder
      attached <- techniques
      discard <- encounterDiscard
      threat <- mainSchemeThreatNow
      code <- villainCode
      liftIO do
        getCardDef s `shouldBe` EnvironmentCards.nebulasShip
        counters `shouldBe` 0
        length milanos `shouldBe` 1
        holder `shouldBe` Just (VillainCharacter vid)
        code `shouldBe` toCardCode VillainCards.nebula1
        -- the 2 discarded: each Technique attached, the rest in the discard pile
        length attached + length discard `shouldBe` 2
        any ((Technique `member`) . cdTraits . getCardDef) discard `shouldBe` False
        -- The Art of Evasion, 2 per player
        threat `shouldBe` 2

  it "in expert mode, Nebula (II) is the first villain" do
    tg <- nebulaExpert
    inGame tg do
      code <- villainCode
      liftIO $ code `shouldBe` toCardCode VillainCards.nebula2

shipSpec :: Spec
shipSpec = describe "Nebula's Ship" do
  it "gets an evasion counter as the villain phase begins" do
    tg <- nebulaGame ["spider-man"]
    inGame tg do
      clearTechniques
      _ <- stackEncounterDeck (replicate 4 MinionCards.hydraMercenary)
      before <- getEvasionCounters
      endTurn
      resolve []
      after <- getEvasionCounters
      liftIO $ (before, after) `shouldBe` (0, 1)

  it "The Art of Evasion accelerates by X per player, X its evasion counters" do
    tg <- nebulaGame ["spider-man", "captain-marvel"]
    inGame tg do
      placeEvasion 2
      before <- mainSchemeThreatNow
      resolve [AccelerateMainScheme]
      after <- mainSchemeThreatNow
      liftIO $ after - before `shouldBe` 4

  it "Shoot the Thrusters!: the first player exhausts the Milano and spends 2 resources to remove 2 evasion counters" do
    tg <- nebulaGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      placeEvasion 3
      s <- ship
      clearHand you
      strength <- putInHand you "01090" -- Strength: two physical resources
      offered <- asked
      let twoOf = \case
            UseAbility ab -> abilitySource ab == toSource s && abilityIndex ab == 2
            _ -> False
      liftIO $ fmap (length . filter (isAbilityOf s) . snd) offered `shouldBe` Just 2
      choose "shoot the thrusters, 2" twoOf
      payWith [strength]
      counters <- getEvasionCounters
      [m] <- supportsWith (toCardCode SupportCards.milano)
      liftIO $ (counters, m.supportExhausted) `shouldBe` (1, True)

  it "Shoot the Thrusters! spending 1 resource removes 1, and needs the Milano ready" do
    tg <- nebulaGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      placeEvasion 3
      s <- ship
      clearHand you
      shooter <- putInHand you "01008" -- Web-Shooter: one physical resource
      let oneOf = \case
            UseAbility ab -> abilitySource ab == toSource s && abilityIndex ab == 1
            _ -> False
      choose "shoot the thrusters, 1" oneOf
      payWith [shooter]
      counters <- getEvasionCounters
      offeredAgain <- offers (isAbilityOf s)
      liftIO $ (counters, offeredAgain) `shouldBe` (2, False)

villainSpec :: Spec
villainSpec = describe "Nebula" do
  it "Nebula (I) defeated, Nebula (II) enters; Nebula (II) defeated, the players win" do
    tg <- nebulaGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      resolve [VillainMessage vid VillainDefeated]
      code <- villainCode
      liftIO $ code `shouldBe` toCardCode VillainCards.nebula2
      vid' <- villainIdNow
      resolve [VillainMessage vid' VillainDefeated]
      state <- getsGame gameState
      liftIO $ state `shouldBe` Finished Won

  it "expert: Nebula (II) defeated, Nebula (III) enters" do
    tg <- nebulaExpert
    inGame tg do
      vid <- villainIdNow
      resolve [VillainMessage vid VillainDefeated]
      code <- villainCode
      liftIO $ code `shouldBe` toCardCode VillainCards.nebula3

  it "Nebula (I) initiating a scheme against you resolves each Technique's Special, then discards them all" do
    tg <- nebulaGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearTechniques
      _ <- attachTechnique AttachmentCards.weaponMastery
      _ <- attachTechnique AttachmentCards.wideStance
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      hand <- length <$> handOf you
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      dmg <- damageOf you
      hand' <- length <$> handOf you
      left <- techniques
      discarded <- (&&) <$> inDiscard AttachmentCards.weaponMastery <*> inDiscard AttachmentCards.wideStance
      liftIO $ (dmg, hand - hand', left, discarded) `shouldBe` (1, 1, [], True)

  it "Nebula (I) initiating an attack resolves them too" do
    tg <- nebulaGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearTechniques
      asHero you
      _ <- attachTechnique AttachmentCards.wideStance
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      hand <- length <$> handOf you
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks you)]
      pass
      resolve []
      hand' <- length <$> handOf you
      left <- techniques
      liftIO $ (hand - hand', left) `shouldBe` (1, [])

  it "a stunned Nebula's attack is not initiated: no Special resolves" do
    tg <- nebulaGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearTechniques
      asHero you
      _ <- attachTechnique AttachmentCards.weaponMastery
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainStunned (toSource you))]
      runNow [VillainMessage vid (VillainAttacks you)]
      resolve []
      dmg <- damageOf you
      left <- techniques
      stunned <- villainStunned <$> villainAttrs
      liftIO $ (dmg, left, stunned) `shouldBe` (0, [toCardCode AttachmentCards.weaponMastery], False)

  it "with no Technique in play, nothing is resolved or asked" do
    tg <- nebulaGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearTechniques
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      before <- mainSchemeThreatNow
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      after <- mainSchemeThreatNow
      dmg <- damageOf you
      -- her 1 SCH and the Mercenary's boost icon
      liftIO $ (after - before, dmg) `shouldBe` (2, 0)

  it "Nebula (II): the player chooses 1 of the Techniques to discard" do
    tg <- nebulaGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      resolve [VillainMessage vid VillainDefeated]
      clearTechniques
      a <- attachTechnique AttachmentCards.cutthroatAmbition
      _ <- attachTechnique AttachmentCards.unyieldingPersistence
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      vid' <- villainIdNow
      runNow [VillainMessage vid' VillainSchemes]
      pass
      choose "discard Cutthroat Ambition" (isTargetChoice (AttachmentRef a))
      resolve []
      left <- techniques
      tough <- villainTough <$> villainAttrs
      liftIO $ (left, tough) `shouldBe` ([toCardCode AttachmentCards.unyieldingPersistence], True)

  it "Nebula (III): the player may remove the top card of their deck from the game to discard 1" do
    tg <- nebulaExpert
    let you = player1 tg
    inGame tg do
      vid <- villainIdNow
      resolve [VillainMessage vid VillainDefeated]
      clearTechniques
      a <- attachTechnique AttachmentCards.unyieldingPersistence
      [top] <- stackDeck you ["01090"]
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      vid' <- villainIdNow
      runNow [VillainMessage vid' VillainSchemes]
      pass
      declineOffered <- offers (isLabel "$label.doNotRemoveTopCard")
      choose "remove the top card to discard it" (isTargetChoice (AttachmentRef a))
      resolve []
      left <- techniques
      deck <- unDeck . (.playerIdentityDeck) <$> playerAttrs you
      discard <- discardOf you
      liftIO do
        (declineOffered, left) `shouldBe` (True, [])
        (top `elem` deck, top `elem` discard) `shouldBe` (False, False)

  it "Nebula (III): declining keeps the Technique and the deck" do
    tg <- nebulaExpert
    let you = player1 tg
    inGame tg do
      vid <- villainIdNow
      resolve [VillainMessage vid VillainDefeated]
      clearTechniques
      _ <- attachTechnique AttachmentCards.unyieldingPersistence
      [top] <- stackDeck you ["01090"]
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      vid' <- villainIdNow
      runNow [VillainMessage vid' VillainSchemes]
      pass
      choose "decline" (isLabel "$label.doNotRemoveTopCard")
      resolve []
      left <- techniques
      deck <- unDeck . (.playerIdentityDeck) <$> playerAttrs you
      liftIO $ (left, take 1 deck) `shouldBe` ([toCardCode AttachmentCards.unyieldingPersistence], [top])

  it "the first Technique revealed each round gains surge, the next that round does not" do
    tg <- nebulaGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearTechniques
      _ <- stackEncounterDeck (replicate 6 MinionCards.hydraMercenary)
      dealt <- length <$> dealtTo you
      _ <- revealNow you AttachmentCards.weaponMastery
      first <- length <$> dealtTo you
      _ <- revealNow you AttachmentCards.wideStance
      second <- length <$> dealtTo you
      attached <- techniques
      liftIO do
        (first - dealt, second - first) `shouldBe` (1, 0)
        length attached `shouldBe` 2

  it "a Technique revealed in the next round gains surge again" do
    tg <- nebulaGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearTechniques
      _ <- revealNow you AttachmentCards.weaponMastery
      clearTechniques
      _ <- stackEncounterDeck (replicate 8 MinionCards.hydraMercenary)
      endTurn
      resolve []
      clearTechniques
      dealt <- length <$> dealtTo you
      _ <- revealNow you AttachmentCards.wideStance
      after <- length <$> dealtTo you
      liftIO $ after - dealt `shouldBe` 1

techniqueSpec :: Spec
techniqueSpec = describe "Nebula's Techniques" do
  it "attach to Nebula as they are revealed" do
    tg <- nebulaGame ["spider-man"]
    inGame tg do
      clearTechniques
      _ <- revealNow (player1 tg) AttachmentCards.cutthroatAmbition
      resolve []
      attached <- techniques
      liftIO $ attached `shouldBe` [toCardCode AttachmentCards.cutthroatAmbition]

  it "Cutthroat Ambition: no more than 5 damage from a single attack; other damage is not capped; Special places 1 threat" do
    tg <- nebulaGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearTechniques
      a <- attachTechnique AttachmentCards.cutthroatAmbition
      heroAttackFor you 8
      attacked <- villainDamageNow
      vid <- villainIdNow
      runNow [Ask you (ChooseOne [DamageEnemy (EnemyRef (EnemyVillainId vid)) (toSource you) (toDamage 7 FromAbility)])]
      choose "the damage" (const True)
      resolve []
      both <- villainDamageNow
      before <- mainSchemeThreatNow
      resolveSpecialNow a you
      after <- mainSchemeThreatNow
      liftIO $ (attacked, both - attacked, after - before) `shouldBe` (5, 7, 1)

  it "Wide Stance: each attack deals Nebula 1 less; Special discards a card at random from your hand" do
    tg <- nebulaGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearTechniques
      a <- attachTechnique AttachmentCards.wideStance
      heroAttackFor you 3
      dmg <- villainDamageNow
      hand <- length <$> handOf you
      resolveSpecialNow a you
      hand' <- length <$> handOf you
      liftIO $ (dmg, hand - hand') `shouldBe` (2, 1)

  it "Wide Stance and Cutthroat Ambition: reduced, then capped" do
    tg <- nebulaGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearTechniques
      _ <- attachTechnique AttachmentCards.wideStance
      _ <- attachTechnique AttachmentCards.cutthroatAmbition
      heroAttackFor you 6
      dmg <- villainDamageNow
      liftIO $ dmg `shouldBe` 5

  it "Weapon Mastery: Nebula retaliates 1 against the hero who attacks her; Special deals 1 damage" do
    tg <- nebulaGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearTechniques
      asHero you
      a <- attachTechnique AttachmentCards.weaponMastery
      choose "attack" (isHeroAttack you)
      chooseVillain
      pass
      retaliated <- damageOf you
      resolveSpecialNow a you
      special <- damageOf you
      liftIO $ (retaliated, special) `shouldBe` (1, 2)

  it "Evasive Maneuvering: Nebula is stalwart; Special stuns you, or gives her a boost card if you are stunned" do
    tg <- nebulaGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearTechniques
      a <- attachTechnique AttachmentCards.evasiveManeuvering
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainStunned (toSource you))]
      nebulaStunned <- villainStunned <$> villainAttrs
      resolveSpecialNow a you
      stunned <- isStunned you
      boosts <- length . villainBoostCards <$> villainAttrs
      resolveSpecialNow a you
      boosts' <- length . villainBoostCards <$> villainAttrs
      stillStunned <- isStunned you
      liftIO $ (nebulaStunned, stunned, boosts' - boosts, stillStunned) `shouldBe` (False, True, 1, True)

  it "Evasive Maneuvering attached to a stunned Nebula removes her stun (stalwart)" do
    tg <- nebulaGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearTechniques
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainStunned (toSource you))]
      before <- villainStunned <$> villainAttrs
      _ <- attachTechnique AttachmentCards.evasiveManeuvering
      after <- villainStunned <$> villainAttrs
      liftIO $ (before, after) `shouldBe` (True, False)

  it "Unyielding Persistence: Nebula is stalwart; Special makes her tough, or gives her a boost card if she is" do
    tg <- nebulaGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearTechniques
      a <- attachTechnique AttachmentCards.unyieldingPersistence
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainConfused (toSource you))]
      confused <- villainConfused <$> villainAttrs
      resolveSpecialNow a you
      tough <- villainTough <$> villainAttrs
      boosts <- length . villainBoostCards <$> villainAttrs
      resolveSpecialNow a you
      boosts' <- length . villainBoostCards <$> villainAttrs
      liftIO $ (confused, tough, boosts' - boosts) `shouldBe` (False, True, 1)

  it "[star] Boost: after the activation, the Technique attaches to Nebula and its Special resolves" do
    tg <- nebulaGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearTechniques
      _ <- stackEncounterDeck [AttachmentCards.weaponMastery]
      before <- mainSchemeThreatNow
      vid <- villainIdNow
      dealt <- length <$> dealtTo you
      resolve [VillainMessage vid VillainSchemes]
      after <- mainSchemeThreatNow
      dmg <- damageOf you
      attached <- techniques
      discarded <- inDiscard AttachmentCards.weaponMastery
      dealt' <- length <$> dealtTo you
      liftIO do
        -- her 1 SCH, the Technique no boost icons
        after - before `shouldBe` 1
        (dmg, attached, discarded) `shouldBe` (1, [toCardCode AttachmentCards.weaponMastery], False)
        -- attached, not revealed: no surge
        dealt' - dealt `shouldBe` 0

encounterSpec :: Spec
encounterSpec = describe "Nebula's encounter cards" do
  it "Lethal Intent: 2 per player, a hazard icon, and reveals the first Technique discarded from the encounter deck" do
    tg <- nebulaGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearTechniques
      _ <- stackEncounterDeck [MinionCards.hydraMercenary, AttachmentCards.wideStance, MinionCards.hydraMercenary]
      card <- revealNow you SideSchemeCards.lethalIntent
      resolve []
      threat <- sideSchemeThreat (sideSchemeOf card)
      attached <- techniques
      mercenary <- inDiscard MinionCards.hydraMercenary
      liftIO do
        threat `shouldBe` 2
        cdHazards SideSchemeCards.lethalIntent `shouldBe` 1
        (attached, mercenary) `shouldBe` ([toCardCode AttachmentCards.wideStance], True)

  it "Barrel Roll: incite 1, surge and 1 evasion counter; its boost places 1 more" do
    tg <- nebulaGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      before <- mainSchemeThreatNow
      dealt <- length <$> dealtTo you
      _ <- revealNow you TreacheryCards.barrelRoll
      resolve []
      after <- mainSchemeThreatNow
      dealt' <- length <$> dealtTo you
      counters <- getEvasionCounters
      vid <- villainIdNow
      card <- encounterCard TreacheryCards.barrelRoll
      resolve [RevealBoostCard card (EnemyVillainId vid)]
      boosted <- getEvasionCounters
      liftIO $ (after - before, dealt' - dealt, counters, boosted) `shouldBe` (1, 1, 1, 2)

  it "Combat Ready reveals the first Technique discarded, then resolves its Special" do
    tg <- nebulaGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearTechniques
      _ <- stackEncounterDeck [MinionCards.hydraMercenary, AttachmentCards.weaponMastery, MinionCards.hydraMercenary]
      _ <- revealNow you TreacheryCards.combatReady
      resolve []
      attached <- techniques
      dmg <- damageOf you
      liftIO $ (attached, dmg) `shouldBe` ([toCardCode AttachmentCards.weaponMastery], 1)

  it "Ruthless (alter-ego): Nebula schemes; threat placed, an evasion counter" do
    tg <- nebulaGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearTechniques
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      before <- mainSchemeThreatNow
      _ <- revealNow you TreacheryCards.ruthless
      resolve []
      after <- mainSchemeThreatNow
      counters <- getEvasionCounters
      liftIO $ (after - before, counters) `shouldBe` (2, 1)

  it "Ruthless (hero): Nebula attacks you; damage dealt, an evasion counter" do
    tg <- nebulaGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearTechniques
      asHero you
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      _ <- revealNow you TreacheryCards.ruthless
      pass
      pressWhenRevealed
      pass
      resolve []
      dmg <- damageOf you
      counters <- getEvasionCounters
      liftIO $ (dmg > 0, counters) `shouldBe` (True, 1)

  it "Ruthless (hero): no damage dealt, no evasion counter" do
    tg <- nebulaGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearTechniques
      asHero you
      resolve [IdentityMessage you IdentityBecomeTough]
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      _ <- revealNow you TreacheryCards.ruthless
      pass
      resolve []
      dmg <- damageOf you
      counters <- getEvasionCounters
      liftIO $ (dmg, counters) `shouldBe` (0, 0)

  it "Warp Drive Initiated: 2 evasion counters, then 2 cards of each deck discarded for each counter; completed, the players lose" do
    tg <- nebulaGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      placeEvasion 1
      deck <- length . unDeck . (.playerIdentityDeck) <$> playerAttrs you
      encounter <- encounterDeckSize
      m <- mainSchemeIdNow
      resolve [AdvanceMainScheme m]
      counters <- getEvasionCounters
      deck' <- length . unDeck . (.playerIdentityDeck) <$> playerAttrs you
      encounter' <- encounterDeckSize
      code <- getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameMainSchemes)
      liftIO do
        counters `shouldBe` 3
        code `shouldBe` [toCardCode MainSchemeCards.warpDriveInitiated]
        (deck - deck', encounter - encounter') `shouldBe` (6, 6)
      m' <- mainSchemeIdNow
      resolve [AdvanceMainScheme m']
      state <- getsGame gameState
      liftIO $ state `shouldBe` Finished Lost

spacePiratesSpec :: Spec
spacePiratesSpec = describe "Space Pirates" do
  it "Pirate Lackey: attacking and damaging you, the top card of your deck is removed from the game" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      m <- addMinion you MinionCards.pirateLackey
      asHero you
      [top] <- stackDeck you ["01090"]
      runNow [MinionMessage m (MinionAttacks you)]
      pass
      choose "its forced response" (isAbilityOf m)
      dmg <- damageOf you
      deck <- unDeck . (.playerIdentityDeck) <$> playerAttrs you
      discard <- discardOf you
      liftIO $ (dmg, top `elem` deck, top `elem` discard) `shouldBe` (2, False, False)

  it "Pirate Commander: attacking and damaging you, a card at random from your hand is removed from the game" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      m <- addMinion you MinionCards.pirateCommander
      asHero you
      clearHand you
      card <- putInHand you "01090"
      runNow [MinionMessage m (MinionAttacks you)]
      pass
      choose "its forced response" (isAbilityOf m)
      hand <- handOf you
      discard <- discardOf you
      liftIO $ (card `elem` hand, card `elem` discard) `shouldBe` (False, False)

  it "Pirate Lackey and Commander have quickstrike" do
    ((Quickstrike `member` cdKeywords MinionCards.pirateLackey, Quickstrike `member` cdKeywords MinionCards.pirateCommander) `shouldBe` (True, True) :: IO ())

  it "a pirate's boost gives the villain another boost card" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      card <- encounterCard MinionCards.pirateLackey
      before <- length . villainBoostCards <$> villainAttrs
      runNow [RevealBoostCard card (EnemyVillainId vid)]
      after <- length . villainBoostCards <$> villainAttrs
      liftIO $ after - before `shouldBe` 1

  it "a pirate's boost turned up for a minion's activation gives the villain a boost card, for his next" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      m <- addMinion you MinionCards.headhuntersHenchman
      card <- encounterCard MinionCards.pirateCommander
      before <- length . villainBoostCards <$> villainAttrs
      runNow [RevealBoostCard card (EnemyMinionId m)]
      after <- length . villainBoostCards <$> villainAttrs
      liftIO $ after - before `shouldBe` 1

  it "Sound the Alarms: each enemy gets +1 ATK; its boost reveals it" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      m <- addMinion you MinionCards.pirateLackey
      before <- getModifiedAttack =<< villainAttrs
      minionBefore <- minionAttack m
      _ <- revealNow you SideSchemeCards.soundTheAlarms
      after <- getModifiedAttack =<< villainAttrs
      minionAfter <- minionAttack m
      vid <- villainIdNow
      card <- encounterCard SideSchemeCards.soundTheAlarms
      resolve [RevealBoostCard card (EnemyVillainId vid)]
      schemes <- getsGame (length . filter ((== SideSchemeCards.soundTheAlarms) . getCardDef) . HashMap.elems . gameSideSchemes)
      liftIO $ (after - before, minionAfter - minionBefore, schemes) `shouldBe` (1, 1, 2)

  it "Honor Among Thieves reveals the first Criminal minion discarded, gives it a tough status card and the villain a boost card" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      _ <- stackEncounterDeck [MinionCards.hydraMercenary, MinionCards.pirateLackey, MinionCards.hydraMercenary]
      boosts <- length . villainBoostCards <$> villainAttrs
      _ <- revealNow you TreacheryCards.honorAmongThieves
      resolve []
      lackeys <- getsGame (filter ((== MinionCards.pirateLackey) . getCardDef) . HashMap.elems . gameMinions)
      boosts' <- length . villainBoostCards <$> villainAttrs
      mercenary <- inDiscard MinionCards.hydraMercenary
      liftIO do
        map (Minion.minionTough . toAttrs) lackeys `shouldBe` [True]
        (boosts' - boosts, mercenary) `shouldBe` (1, True)
 where
  minionAttack m = getsGame (HashMap.lookup m . gameMinions) >>= maybe (pure 0) (Minion.getModifiedAttack . toAttrs)
