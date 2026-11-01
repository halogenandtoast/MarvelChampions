-- | Thanos (The Mad Titan's Shadow, scenario 3), the Infinity Gauntlet set and the
-- Children of Thanos modular set, each card played as a table would.
module Marvel.Encounter.ThanosSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Area (DeckKey (..), Pile (..))
import Marvel.Attachment.Cards qualified as AttachmentCards
import Marvel.Attachment.Types (Attachment, Attrs (..))
import Marvel.EncounterSet (EncounterSet (BlackOrderSet, ChildrenOfThanos, InfinityGauntlet))
import Marvel.Environment.Cards qualified as EnvironmentCards
import Marvel.Environment.Types (Environment)
import Marvel.MainScheme.Cards qualified as MainSchemeCards
import Marvel.Minion.Cards qualified as MinionCards
import Marvel.SideScheme.Cards qualified as SideSchemeCards
import Marvel.Trait (Trait (InfinityStone))
import Marvel.Treachery.Cards qualified as TreacheryCards
import Marvel.Villain.Cards qualified as VillainCards
import Marvel.Hp (HP (..))
import Marvel.SideScheme.Types (isCrisis)
import Marvel.Villain.Types (getModifiedAttack, getModifiedScheme, villainBoostCards, villainHp, villainId, villainStunned, villainTough)
import TestImport

thanosGame :: [Text] -> IO TestGame
thanosGame = testGameAgainst "21111"

-- | A game of the scenario given, its modular sets these.
gameOf :: ScenarioSetup -> IO TestGame
gameOf setup = do
  (app, ran, logged) <- dealtGameLogged setup [starter "spider-man"] 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

thanosExpert :: IO TestGame
thanosExpert = gameOf (ScenarioSetup "21111" [BlackOrderSet, ChildrenOfThanos] Expert)

villainAttrs :: GameAppT (Attrs Villain)
villainAttrs = toAttrs <$> getVillain

villainCode :: GameAppT CardCode
villainCode = toCardCode . getCardDef <$> getVillain

boostsOnThanos :: GameAppT Int
boostsOnThanos = length . villainBoostCards <$> villainAttrs

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass

attachmentsOf :: CardDef -> GameAppT [Attrs Attachment]
attachmentsOf def = getsGame (filter ((== toCardCode def) . toCardCode . attachmentCardDef) . map toAttrs . HashMap.elems . gameAttachments)

gauntlet :: GameAppT (Attrs Attachment)
gauntlet = attachmentsOf AttachmentCards.infinityGauntlet >>= \case
  [g] -> pure g
  gs -> error $ "expected the Infinity Gauntlet, found " <> show (length gs)

-- | Take the Gauntlet away, for a card tested without its response.
removeGauntlet :: GameAppT ()
removeGauntlet = gauntlet >>= \g -> resolve [RemoveFromGame (AttachmentRef $ attachmentId g)]

-- | An attachment revealed to the player, as from the encounter deck.
attachNow :: IdentityId -> CardDef -> GameAppT AttachmentId
attachNow you def = do
  card <- revealNow you def
  resolve []
  pure $ AttachmentId (unCardId (ecCardId card))

stonePile :: GameAppT Pile
stonePile = getPile InfinityStoneDeckKey

stonesInPlay :: GameAppT [Environment]
stonesInPlay = getsGame (filter ((InfinityStone `member`) . cdTraits . getCardDef) . HashMap.elems . gameEnvironments)

stoneCodesInPlay :: GameAppT [CardCode]
stoneCodesInPlay = map (toCardCode . getCardDef) <$> stonesInPlay

stoneDiscardCodes :: GameAppT [CardCode]
stoneDiscardCodes = map toCardCode . pileDiscard <$> stonePile

-- | Every stone in play discarded, to the stone discard pile.
clearStones :: GameAppT ()
clearStones = stonesInPlay >>= \es -> resolve [RemoveFromPlay (toRef e) | e <- es]

-- | The stone, not in play, on top of the stone deck.
stoneOnTop :: CardDef -> GameAppT ()
stoneOnTop def = do
  resolve [UsingDeck InfinityStoneDeckKey ShuffleEncounterDiscardIntoDeck]
  Pile deck _ <- stonePile
  case filter ((== toCardCode def) . toCardCode) deck of
    [c] -> resolve [UsingDeck InfinityStoneDeckKey (PutOnTopOfEncounterDeck [c])]
    _ -> error $ "not in the stone deck: " <> show (cdName def)

-- | The stone put into play from the stone deck.
stoneIntoPlay :: IdentityId -> CardDef -> GameAppT ()
stoneIntoPlay you def = do
  stoneOnTop def
  resolve [UsingDeck InfinityStoneDeckKey (PutTopOfEncounterDeckIntoPlay you)]

-- | The stone's Special resolved for the player, as the Gauntlet resolves it.
resolveStone :: IdentityId -> CardDef -> GameAppT ()
resolveStone you def =
  stonesInPlay >>= \es -> case filter ((== def) . getCardDef) es of
    [e] -> resolve [cardSpecific (toRef e) "infinityStoneSpecial" you]
    _ -> error "the stone is not in play"

sanctuaryId :: GameAppT (Maybe SideSchemeId)
sanctuaryId = getsGame (headMay . HashMap.keys . HashMap.filter ((== SideSchemeCards.sanctuary) . getCardDef) . gameSideSchemes)

removeSanctuary :: GameAppT ()
removeSanctuary = sanctuaryId >>= traverse_ \sid -> resolve [RemoveFromGame (SideSchemeRef sid)]

sideSchemeCodes :: GameAppT [CardCode]
sideSchemeCodes = getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameSideSchemes)

-- | Damage of an attack of the hero's on the villain, its target already chosen.
heroAttackFor :: IdentityId -> Natural -> GameAppT ()
heroAttackFor you n = do
  vid <- villainIdNow
  runNow [Ask you (ChooseOne [DamageEnemy (EnemyRef (EnemyVillainId vid)) (toSource you) (toDamage n (FromPlayerAttack you))])]
  choose "the attack" (const True)
  resolve []

-- | Damage to the villain from no player card.
damageFromGame :: Natural -> GameAppT ()
damageFromGame n = do
  vid <- villainIdNow
  you <- headMay <$> players >>= maybe (error "no players") pure
  runNow [Ask you (ChooseOne [DamageEnemy (EnemyRef (EnemyVillainId vid)) GameRef (toDamage n FromAbility)])]
  choose "the damage" (const True)
  resolve []

makeThanosTough :: GameAppT ()
makeThanosTough = villainIdNow >>= \vid -> resolve [VillainMessage vid VillainBecomeTough]

-- | Decline each response offered until one matching is offered, or nothing more is.
answerUntil :: (Choice -> Bool) -> GameAppT Bool
answerUntil p = go (20 :: Int)
 where
  go 0 = pure False
  go n =
    offers p >>= \case
      True -> pure True
      False ->
        offers isNoResponse >>= \case
          True -> choose "no response" isNoResponse >> go (n - 1)
          False -> pure False

isPayWithCode :: CardCode -> Choice -> Bool
isPayWithCode code = \case
  PayWithCard c -> toCardCode c == code
  _ -> False

finished :: GameAppT (Maybe FinishedStatus)
finished = getsGame \g -> case gameState g of
  Finished status -> Just status
  _ -> Nothing

spec :: Spec
spec = do
  setupSpec
  stoneDeckSpec
  villainSpec
  gauntletSpec
  stonesSpec
  sanctuarySpec
  thanosSetSpec
  childrenOfThanosSpec

setupSpec :: Spec
setupSpec = describe "Thanos (setup)" do
  it "the Infinity Gauntlet on Thanos, the six stones its own deck, one into play by 1B, and Sanctuary revealed" do
    tg <- thanosGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      g <- gauntlet
      Pile deck discard <- stonePile
      inPlay <- stoneCodesInPlay
      encounter <- map toCardCode <$> scenarioEncounterDeckNow
      schemes <- sideSchemeCodes
      code <- getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameMainSchemes)
      threat <- maybe (pure 0) sideSchemeThreat =<< sanctuaryId
      liftIO do
        attachmentEnemy g `shouldBe` Just (EnemyVillainId vid)
        (length deck, length discard, length inPlay) `shouldBe` (5, 0, 1)
        encounter `shouldSatisfy` all (`notElem` map toCardCode [EnvironmentCards.mindStone, EnvironmentCards.infinityPowerStone, EnvironmentCards.realityStone, EnvironmentCards.soulStone, EnvironmentCards.spaceStone, EnvironmentCards.timeStone])
        schemes `shouldBe` [toCardCode SideSchemeCards.sanctuary]
        -- 3, and hinder 1 per player
        threat `shouldBe` 4
        code `shouldBe` [toCardCode MainSchemeCards.theInfinityStones]

  it "Thanos (I) is stalwart, 16 hit points per player, and the Gauntlet gives him +1 ATK and +1 SCH" do
    tg <- thanosGame ["spider-man"]
    inGame tg do
      a <- villainAttrs
      atk <- getModifiedAttack a
      sch <- getModifiedScheme a
      liftIO do
        toCardCode (getCardDef a) `shouldBe` toCardCode VillainCards.thanos1
        villainHp a `shouldBe` HP 16
        (atk, sch) `shouldBe` (3, 2)
      resolve [VillainMessage (villainId a) (VillainStunned GameRef)]
      stunned <- villainStunned <$> villainAttrs
      liftIO $ stunned `shouldBe` False

  it "expert: Thanos (II), tough, reveals Thanos's Helmet as setup ends" do
    tg <- thanosExpert
    inGame tg do
      a <- villainAttrs
      helmets <- attachmentsOf AttachmentCards.thanossHelmet
      liftIO do
        toCardCode (getCardDef a) `shouldBe` toCardCode VillainCards.thanos2
        villainTough a `shouldBe` True
        map attachmentEnemy helmets `shouldBe` [Just (EnemyVillainId $ villainId a)]

stoneDeckSpec :: Spec
stoneDeckSpec = describe "The Infinity Stone deck" do
  it "a stone discarded from play goes to the stone discard pile, not the encounter discard pile" do
    tg <- thanosGame ["spider-man"]
    inGame tg do
      [code] <- stoneCodesInPlay
      clearStones
      stones <- stoneDiscardCodes
      encounter <- encounterDiscardCodes
      liftIO do
        stones `shouldBe` [code]
        encounter `shouldSatisfy` notElem code

  it "run out, it is made again from its discard pile without an acceleration token, and Thanos is given a boost card" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearStones
      tokens <- accelerationTokensNow
      -- 6 in the deck, 5 of them discarded
      resolve [UsingDeck InfinityStoneDeckKey ShuffleEncounterDiscardIntoDeck]
      resolve [UsingDeck InfinityStoneDeckKey (DiscardTopOfEncounterDeck 5 Nothing)]
      boosts <- boostsOnThanos
      _ <- revealNow you TreacheryCards.theMadTitan
      resolve []
      Pile deck discard <- stonePile
      boosts' <- boostsOnThanos
      tokens' <- accelerationTokensNow
      inPlay <- stoneCodesInPlay
      liftIO do
        (length deck, length discard, length inPlay) `shouldBe` (5, 0, 1)
        boosts' - boosts `shouldBe` 1
        tokens' `shouldBe` tokens

  it "emptied with nothing in its discard pile, it runs out once: no boost card until it has had a card again" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      boosts <- boostsOnThanos
      -- the 5 left put into play, then one more asked for
      resolve (replicate 6 $ UsingDeck InfinityStoneDeckKey (PutTopOfEncounterDeckIntoPlay you))
      inPlay <- length <$> stoneCodesInPlay
      boosts' <- boostsOnThanos
      -- one discarded from play: the deck is made again, but did not run out
      stonesInPlay >>= \case
        e : _ -> resolve [RemoveFromPlay (toRef e)]
        [] -> error "no stone"
      Pile deck _ <- stonePile
      boosts'' <- boostsOnThanos
      liftIO $ (inPlay, boosts' - boosts, length deck, boosts'' - boosts) `shouldBe` (6, 1, 1, 1)

villainSpec :: Spec
villainSpec = describe "Thanos" do
  it "Thanos (I) defeated, Thanos (II) enters and reveals Thanos's Helmet; Thanos (II) defeated, the players win" do
    tg <- thanosGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      resolve [VillainMessage vid VillainDefeated]
      code <- villainCode
      helmets <- length <$> attachmentsOf AttachmentCards.thanossHelmet
      liftIO $ (code, helmets) `shouldBe` (toCardCode VillainCards.thanos2, 1)
      vid' <- villainIdNow
      resolve [VillainMessage vid' VillainDefeated]
      finished `shouldReturn` Just Won

  it "expert: Thanos (II) defeated, Thanos (III) enters" do
    tg <- thanosExpert
    inGame tg do
      vid <- villainIdNow
      resolve [VillainMessage vid VillainDefeated]
      villainCode `shouldReturn` toCardCode VillainCards.thanos3

  it "Balance the Scales: each player shuffles their discard pile in and removes the top half of their deck; completed, the players lose" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      _ <- stackDiscard you ["01090", "01088", "01089"]
      before <- (+) <$> (length <$> discardOf you) <*> (length . unDeck . (.playerIdentityDeck) <$> playerAttrs you)
      mid <- mainSchemeIdNow
      resolve [AdvanceMainScheme mid]
      code <- getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameMainSchemes)
      deck <- length . unDeck . (.playerIdentityDeck) <$> playerAttrs you
      discard <- length <$> discardOf you
      liftIO do
        code `shouldBe` [toCardCode MainSchemeCards.balanceTheScales]
        (deck, discard) `shouldBe` (before - before `div` 2, 0)
      mid' <- mainSchemeIdNow
      resolve [AdvanceMainScheme mid']
      finished `shouldReturn` Just Lost

gauntletSpec :: Spec
gauntletSpec = describe "The Infinity Gauntlet" do
  it "after Thanos schemes against you, the Special of each stone in play resolves, in the order they entered play, each to the stone discard pile" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearStones
      stoneIntoPlay you EnvironmentCards.mindStone
      stoneIntoPlay you EnvironmentCards.infinityPowerStone
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      confused <- isConfused you
      stunned <- isStunned you
      inPlay <- stoneCodesInPlay
      discarded <- stoneDiscardCodes
      liftIO do
        (confused, stunned, inPlay) `shouldBe` (True, True, [])
        -- the Mind Stone first, so it is under the Power Stone
        discarded `shouldBe` map toCardCode [EnvironmentCards.infinityPowerStone, EnvironmentCards.mindStone]

  it "after Thanos attacks you, the same, after the attack and its boost cards are done with" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      asHero you
      clearStones
      stoneIntoPlay you EnvironmentCards.mindStone
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks you)]
      pass
      resolve []
      dmg <- damageOf you
      confused <- isConfused you
      discarded <- stoneDiscardCodes
      boostDiscarded <- elem (toCardCode MinionCards.hydraMercenary) <$> encounterDiscardCodes
      liftIO $ (dmg > 0, confused, discarded, boostDiscarded) `shouldBe` (True, True, [toCardCode EnvironmentCards.mindStone], True)

  it "with no stone in play, the top card of the stone deck is put into play instead, nothing resolved" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearStones
      stoneOnTop EnvironmentCards.mindStone
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      inPlay <- stoneCodesInPlay
      confused <- isConfused you
      liftIO $ (inPlay, confused) `shouldBe` ([toCardCode EnvironmentCards.mindStone], False)

  it "Avatar of Death in alter-ego form: Thanos schemes, which is an activation against you" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearStones
      stoneOnTop EnvironmentCards.mindStone
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      _ <- revealNow you TreacheryCards.avatarOfDeath
      resolve []
      stoneCodesInPlay `shouldReturn` [toCardCode EnvironmentCards.mindStone]

  it "Avatar of Death revealed by another player than the active one: the scheme is against them, and so the Specials" do
    tg <- thanosGame ["spider-man", "captain-marvel"]
    let p1 = player1 tg
        p2 = player2 tg
    inGame tg do
      clearStones
      stoneIntoPlay p1 EnvironmentCards.mindStone
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      _ <- revealNow p2 TreacheryCards.avatarOfDeath
      resolve []
      c1 <- isConfused p1
      c2 <- isConfused p2
      liftIO $ (c1, c2) `shouldBe` (False, True)

  it "the player Thanos's attack eliminates has no Specials resolved: nothing happens" do
    tg <- thanosGame ["spider-man", "captain-marvel"]
    let p1 = player1 tg
        p2 = player2 tg
    inGame tg do
      asHero p1
      clearStones
      stoneIntoPlay p1 EnvironmentCards.mindStone
      hp <- hitPointsOf p1
      setDamage p1 (hp - 1)
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks p1)]
      pass
      resolve []
      left <- players
      inPlay <- stoneCodesInPlay
      over <- finished
      liftIO $ (left, inPlay, over) `shouldBe` ([p2], [toCardCode EnvironmentCards.mindStone], Nothing)

  it "is used in a scenario with one villain: attached to Rhino, the stones its own deck" do
    tg <- gameOf (ScenarioSetup "01094" [InfinityGauntlet] Normal)
    inGame tg do
      vid <- villainIdNow
      g <- gauntlet
      Pile deck _ <- stonePile
      liftIO $ (attachmentEnemy g, length deck) `shouldBe` (Just (EnemyVillainId vid), 6)

  it "is not used with more than one villain at the start: the Gauntlet and its stones are out of play" do
    tg <- gameOf (ScenarioSetup "21098" [InfinityGauntlet] Normal)
    inGame tg do
      gauntlets <- attachmentsOf AttachmentCards.infinityGauntlet
      keys <- getDeckKeys
      encounter <- map toCardCode <$> scenarioEncounterDeckNow
      liftIO do
        gauntlets `shouldBe` []
        keys `shouldSatisfy` notElem InfinityStoneDeckKey
        encounter `shouldSatisfy` notElem (toCardCode EnvironmentCards.timeStone)

  it "a stunned villain's attack is no activation: nothing resolves" do
    tg <- gameOf (ScenarioSetup "01094" [InfinityGauntlet] Normal)
    let you = player1 tg
    inGame tg do
      asHero you
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainStunned (toSource you))]
      runNow [VillainMessage vid (VillainAttacks you)]
      resolve []
      stoneCodesInPlay `shouldReturn` []

stonesSpec :: Spec
stonesSpec = describe "The Infinity Stones" do
  it "Mind Stone: you are confused; already confused, a card at random is discarded from your hand" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearStones
      stoneIntoPlay you EnvironmentCards.mindStone
      hand <- length <$> handOf you
      resolveStone you EnvironmentCards.mindStone
      confused <- isConfused you
      hand' <- length <$> handOf you
      stoneIntoPlay you EnvironmentCards.mindStone
      resolveStone you EnvironmentCards.mindStone
      hand'' <- length <$> handOf you
      liftIO $ (confused, hand - hand', hand' - hand'') `shouldBe` (True, 0, 1)

  it "Power Stone: you are stunned; already stunned, you take 3 damage" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearStones
      stoneIntoPlay you EnvironmentCards.infinityPowerStone
      resolveStone you EnvironmentCards.infinityPowerStone
      stunned <- isStunned you
      dmg <- damageOf you
      stoneIntoPlay you EnvironmentCards.infinityPowerStone
      resolveStone you EnvironmentCards.infinityPowerStone
      dmg' <- damageOf you
      liftIO $ (stunned, dmg, dmg') `shouldBe` (True, 0, 3)

  it "Reality Stone: discard an ally, upgrade or support you control; with none, nothing is asked" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearStones
      stoneIntoPlay you EnvironmentCards.realityStone
      resolveStone you EnvironmentCards.realityStone
      _ <- putIntoPlay you "01002"
      stoneIntoPlay you EnvironmentCards.realityStone
      resolveStone you EnvironmentCards.realityStone
      allies <- alliesWith "01002"
      discarded <- map toCardCode <$> discardOf you
      liftIO $ (allies, "01002" `elem` discarded) `shouldBe` ([], True)

  it "Soul Stone: heal 3 damage from the villain and give it a facedown boost card" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearStones
      removeSanctuary
      damageFromGame 5
      boosts <- boostsOnThanos
      stoneIntoPlay you EnvironmentCards.soulStone
      resolveStone you EnvironmentCards.soulStone
      dmg <- villainDamageNow
      boosts' <- boostsOnThanos
      liftIO $ (dmg, boosts' - boosts) `shouldBe` (2, 1)

  it "Space Stone: encounter cards discarded until a minion, which enters play engaged with you" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearStones
      _ <- stackEncounterDeck [TreacheryCards.advance, MinionCards.hydraMercenary]
      stoneIntoPlay you EnvironmentCards.spaceStone
      resolveStone you EnvironmentCards.spaceStone
      engaged <- getsGame (map (minionEngagedIdentity . toAttrs) . HashMap.elems . gameMinions)
      advanceDiscarded <- elem (toCardCode TreacheryCards.advance) <$> encounterDiscardCodes
      liftIO $ (engaged, advanceDiscarded) `shouldBe` ([you], True)

  it "Time Stone: the top 4 cards of your deck discarded, 1 threat on the main scheme for each card type among them" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearStones
      -- a resource, an event, two upgrades: three types
      _ <- stackDeck you ["01090", "01087", "01093", "01008"]
      before <- mainSchemeThreatNow
      stoneIntoPlay you EnvironmentCards.timeStone
      resolveStone you EnvironmentCards.timeStone
      after <- mainSchemeThreatNow
      discarded <- stoneDiscardCodes
      liftIO $ (after - before, discarded) `shouldBe` (3, [toCardCode EnvironmentCards.timeStone])

sanctuarySpec :: Spec
sanctuarySpec = describe "Sanctuary" do
  it "Thanos takes no damage from a hero's basic attack while it is in play" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      asHero you
      choose "attack" (isHeroAttack you)
      chooseVillain
      pass
      resolve []
      villainDamageNow `shouldReturn` 0

  it "nor from an event" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      asHero you
      clearHand you
      haymaker <- putInHand you "01087"
      _ <- putInHand you "01088"
      playWith haymaker []
      chooseIf (isPayWithCode "01088")
      chooseIf isFinishPayment
      chooseVillain
      resolve []
      villainDamageNow `shouldReturn` 0

  it "but damage from no player card is dealt" do
    tg <- thanosGame ["spider-man"]
    inGame tg do
      damageFromGame 2
      villainDamageNow `shouldReturn` 2

  it "defeated: to the victory display; each player may spend physical resources from hand, 2 damage each, ignoring tough" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearHand you
      strength <- putInHand you "01090"
      makeThanosTough
      sid <- maybe (error "no Sanctuary") pure =<< sanctuaryId
      runNow [SideSchemeMessage sid (SideSchemeThwarted (IdentityRef you) 10)]
      _ <- answerUntil (isLabel (countLabel "spendPhysicalResources" 2))
      -- Strength's 2 physical resources: 3 cannot be paid
      three <- offers (isLabel (countLabel "spendPhysicalResources" 3))
      choose "spend 2" (isLabel (countLabel "spendPhysicalResources" 2))
      payWith [strength]
      resolve []
      dmg <- villainDamageNow
      tough <- villainTough <$> villainAttrs
      victory <- map toCardCode <$> getVictoryDisplay
      liftIO do
        three `shouldBe` False
        (dmg, tough) `shouldBe` (4, True)
        victory `shouldSatisfy` elem (toCardCode SideSchemeCards.sanctuary)

  it "defeated with two players: what both spend is one amount, Thanos's Armor reducing it once" do
    tg <- thanosGame ["spider-man", "captain-marvel"]
    let p1 = player1 tg
        p2 = player2 tg
    inGame tg do
      clearHand p1
      clearHand p2
      s1 <- putInHand p1 "01090"
      s2 <- putInHand p2 "01090"
      _ <- attachNow p1 AttachmentCards.thanossArmor
      sid <- maybe (error "no Sanctuary") pure =<< sanctuaryId
      runNow [SideSchemeMessage sid (SideSchemeThwarted (IdentityRef p1) 10)]
      _ <- answerUntil (isLabel (countLabel "spendPhysicalResources" 2))
      choose "player 1 spends 2" (isLabel (countLabel "spendPhysicalResources" 2))
      payWith [s1]
      _ <- answerUntil (isLabel (countLabel "spendPhysicalResources" 2))
      before <- villainDamageNow
      choose "player 2 spends 2" (isLabel (countLabel "spendPhysicalResources" 2))
      payWith [s2]
      resolve []
      dmg <- villainDamageNow
      -- 2 for each of 4, less 1: not 3 and 3
      liftIO $ (before, dmg) `shouldBe` (0, 7)

  it "defeated with no physical resource in hand: nothing is asked" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearHand you
      _ <- putInHand you "01089"
      sid <- maybe (error "no Sanctuary") pure =<< sanctuaryId
      runNow [SideSchemeMessage sid (SideSchemeThwarted (IdentityRef you) 10)]
      spend <- answerUntil (isLabel "$label.spendNothing")
      resolve []
      dmg <- villainDamageNow
      liftIO $ (spend, dmg) `shouldBe` (False, 0)

thanosSetSpec :: Spec
thanosSetSpec = describe "The Thanos set" do
  it "Thanos's Armor: every amount of damage to Thanos is 1 less; tough, he loses only the tough status card" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      removeSanctuary
      _ <- attachNow you AttachmentCards.thanossArmor
      heroAttackFor you 3
      attacked <- villainDamageNow
      damageFromGame 3
      both <- villainDamageNow
      makeThanosTough
      damageFromGame 1
      tough <- villainTough <$> villainAttrs
      after <- villainDamageNow
      liftIO $ (attacked, both - attacked, tough, after - both) `shouldBe` (2, 2, False, 0)

  it "Thanos's Armor reduces the overkill damage an attack on a minion carries on to him" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      removeSanctuary
      _ <- attachNow you AttachmentCards.thanossArmor
      m <- addMinion you MinionCards.hydraMercenary
      hp <- maybe 0 (unHp . minionHitPoints . toAttrs) <$> getsGame (HashMap.lookup m . gameMinions)
      runNow [Ask you (ChooseOne [DamageEnemy (MinionRef m) (toSource you) (withOverkill $ toDamage (hp + 3) (FromPlayerAttack you))])]
      choose "the attack" (const True)
      resolve []
      villainDamageNow `shouldReturn` 2

  it "Thanos's Armor: after your hero's basic attack against Thanos, spend [energy][physical] to discard it" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      removeSanctuary
      asHero you
      clearHand you
      energy <- putInHand you "01088"
      strength <- putInHand you "01090"
      armor <- attachNow you AttachmentCards.thanossArmor
      choose "attack" (isHeroAttack you)
      chooseVillain
      offered <- answerUntil (isAbilityOf (AttachmentRef armor))
      liftIO $ offered `shouldBe` True
      choose "discard the Armor" (isAbilityOf (AttachmentRef armor))
      payWith [energy, strength]
      resolve []
      left <- attachmentsOf AttachmentCards.thanossArmor
      liftIO $ left `shouldBe` []

  it "Thanos's Armor is not offered for an attack event" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      removeSanctuary
      asHero you
      clearHand you
      haymaker <- putInHand you "01087"
      _ <- putInHand you "01088"
      _ <- putInHand you "01090"
      _armor <- attachNow you AttachmentCards.thanossArmor
      playWith haymaker []
      chooseIf (isPayWithCode "01088")
      chooseIf isFinishPayment
      chooseVillain
      -- were it offered, the first choice would use it
      resolve []
      dmg <- villainDamageNow
      left <- attachmentsOf AttachmentCards.thanossArmor
      liftIO $ (dmg, length left) `shouldBe` (2, 1)

  it "Thanos's Helmet: Thanos retaliates 1; after your basic attack, spend [mental][physical] to discard it" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      removeSanctuary
      asHero you
      clearHand you
      genius <- putInHand you "01089"
      strength <- putInHand you "01090"
      helmet <- attachNow you AttachmentCards.thanossHelmet
      choose "attack" (isHeroAttack you)
      chooseVillain
      offered <- answerUntil (isAbilityOf (AttachmentRef helmet))
      retaliated <- damageOf you
      liftIO $ (offered, retaliated) `shouldBe` (True, 1)
      choose "discard the Helmet" (isAbilityOf (AttachmentRef helmet))
      payWith [genius, strength]
      resolve []
      attachmentsOf AttachmentCards.thanossHelmet `shouldReturn` []

  it "Master of the Stones: +1 ATK and SCH; a stone into play as Thanos activates, which the Gauntlet resolves; discarded at the end" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearStones
      _ <- attachNow you AttachmentCards.masterOfTheStones
      atk <- getModifiedAttack =<< villainAttrs
      stoneOnTop EnvironmentCards.mindStone
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      confused <- isConfused you
      inPlay <- stoneCodesInPlay
      discarded <- elem (toCardCode AttachmentCards.masterOfTheStones) <$> encounterDiscardCodes
      liftIO $ (atk, confused, inPlay, discarded) `shouldBe` (4, True, [], True)

  it "Avatar of Death in hero form: Thanos attacks you, the attack gaining piercing: your tough status card is discarded and you take damage" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      removeGauntlet
      asHero you
      resolve [IdentityMessage you IdentityBecomeTough]
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      _ <- revealNow you TreacheryCards.avatarOfDeath
      pass
      resolve []
      dmg <- damageOf you
      tough <- (.playerIdentityTough) <$> playerAttrs you
      liftIO $ (dmg, tough) `shouldBe` (3, False)

  it "Avatar of Death in alter-ego form: Thanos schemes" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      removeGauntlet
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      before <- mainSchemeThreatNow
      _ <- revealNow you TreacheryCards.avatarOfDeath
      resolve []
      after <- mainSchemeThreatNow
      -- his 1 SCH and the Mercenary's boost icon
      liftIO $ after - before `shouldBe` 2

  it "Deviant Syndrome: incite 1, and Thanos is given a tough status card; already tough, 2 more threat instead" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      before <- mainSchemeThreatNow
      _ <- revealNow you TreacheryCards.deviantSyndrome
      resolve []
      mid' <- mainSchemeThreatNow
      tough <- villainTough <$> villainAttrs
      _ <- revealNow you TreacheryCards.deviantSyndrome
      resolve []
      after <- mainSchemeThreatNow
      liftIO $ (mid' - before, tough, after - mid') `shouldBe` (1, True, 3)

  it "Deviant Syndrome's boost gives Thanos a tough status card" do
    tg <- thanosGame ["spider-man"]
    inGame tg do
      removeGauntlet
      _ <- stackEncounterDeck [TreacheryCards.deviantSyndrome]
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      tough <- villainTough <$> villainAttrs
      liftIO $ tough `shouldBe` True

  it "\"I Am Inevitable\": Thanos is given a facedown boost card" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      boosts <- boostsOnThanos
      _ <- revealNow you TreacheryCards.iAmInevitable
      resolve []
      boosts' <- boostsOnThanos
      liftIO $ boosts' - boosts `shouldBe` 1

  it "\"I Am Inevitable\"'s boost: the top stone is discarded, its boost icons counted for the activation" do
    tg <- thanosGame ["spider-man"]
    inGame tg do
      removeGauntlet
      clearStones
      stoneOnTop EnvironmentCards.timeStone
      _ <- stackEncounterDeck [TreacheryCards.iAmInevitable]
      before <- mainSchemeThreatNow
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      after <- mainSchemeThreatNow
      discarded <- stoneDiscardCodes
      -- his 1 SCH and the Time Stone's 4 boost icons
      liftIO $ (after - before, discarded) `shouldBe` (5, [toCardCode EnvironmentCards.timeStone])

  it "The Mad Titan: the top stone is put into play" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearStones
      _ <- revealNow you TreacheryCards.theMadTitan
      resolve []
      length <$> stoneCodesInPlay `shouldReturn` 1

  it "The Mad Titan's boost: an ally the attack defeats puts the top stone into play" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      removeGauntlet
      clearStones
      asHero you
      cat <- allyIdOf <$> putIntoPlay you "01002"
      let isCatDefending = \case
            AllyDefend a _ -> a == cat
            _ -> False
      _ <- stackEncounterDeck [TreacheryCards.theMadTitan]
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks you)]
      _ <- answerUntil isCatDefending
      choose "Black Cat defends" isCatDefending
      resolve []
      allies <- alliesWith "01002"
      inPlay <- length <$> stoneCodesInPlay
      liftIO $ (allies, inPlay) `shouldBe` ([], 1)

  it "The Mad Titan's boost: no ally defeated, no stone" do
    tg <- thanosGame ["spider-man"]
    let _you = player1 tg
    inGame tg do
      removeGauntlet
      clearStones
      _ <- stackEncounterDeck [TreacheryCards.theMadTitan]
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      stoneCodesInPlay `shouldReturn` []

  it "The Titan's Throne: an Infinity Stone in play is chosen and discarded" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearStones
      stoneIntoPlay you EnvironmentCards.mindStone
      stoneIntoPlay you EnvironmentCards.soulStone
      _ <- revealNow you SideSchemeCards.theTitansThrone
      soul <- stonesInPlay <&> filter ((== EnvironmentCards.soulStone) . getCardDef)
      case soul of
        [s] -> choose "discard the Soul Stone" (isTargetChoice (toRef s))
        _ -> error "no Soul Stone"
      resolve []
      inPlay <- stoneCodesInPlay
      liftIO $ inPlay `shouldBe` [toCardCode EnvironmentCards.mindStone]

childrenOfThanosSpec :: Spec
childrenOfThanosSpec = describe "Children of Thanos" do
  it "Corvus Glaive enters play tough; his boost discards an ally or support you control" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      removeGauntlet
      m <- addMinion you MinionCards.corvusGlaiveMinion
      tough <- getsGame (maybe False (minionTough . toAttrs) . HashMap.lookup m . gameMinions)
      _ <- putIntoPlay you "01002"
      _ <- stackEncounterDeck [MinionCards.corvusGlaiveMinion]
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      allies <- alliesWith "01002"
      liftIO $ (tough, allies) `shouldBe` (True, [])

  it "Proxima Midnight's attacks gain piercing; her boost discards an ally or upgrade you control" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      removeGauntlet
      asHero you
      resolve [IdentityMessage you IdentityBecomeTough]
      m <- addMinion you MinionCards.proximaMidnightMinion
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      runNow [MinionMessage m (MinionAttacks you)]
      pass
      resolve []
      tough <- (.playerIdentityTough) <$> playerAttrs you
      dmg <- damageOf you
      shooter <- putIntoPlay you "01008"
      _ <- stackEncounterDeck [MinionCards.proximaMidnightMinion]
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      upgrades <- upgradesWith (toCardCode shooter)
      liftIO $ (tough, dmg > 0, upgrades) `shouldBe` (False, True, [])

  it "Ebony Maw is villainous; his boost gives the enemy activating another boost card" do
    tg <- thanosGame ["spider-man"]
    inGame tg do
      removeGauntlet
      _ <- stackEncounterDeck [MinionCards.ebonyMawMinion, MinionCards.hydraMercenary]
      before <- mainSchemeThreatNow
      vid <- villainIdNow
      resolve [VillainMessage vid VillainSchemes]
      after <- mainSchemeThreatNow
      -- his 1 SCH, Ebony Maw's boost icon and the Mercenary's
      liftIO $ after - before `shouldBe` 3

  it "Ebony Maw in play is villainous: scheming, he is given a boost card" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      m <- addMinion you MinionCards.ebonyMawMinion
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      before <- mainSchemeThreatNow
      resolve [MinionMessage m MinionSchemes]
      after <- mainSchemeThreatNow
      boostDiscarded <- elem (toCardCode MinionCards.hydraMercenary) <$> encounterDiscardCodes
      -- his 1 SCH and the Mercenary's boost icon
      liftIO $ (after - before, boostDiscarded) `shouldBe` (2, True)

  it "Tribute: a crisis icon; defeated, the player who defeated it is dealt an encounter card" do
    tg <- thanosGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      card <- revealNow you SideSchemeCards.tribute
      resolve []
      crisis <- getsGame (fmap isCrisis . HashMap.lookup (sideSchemeOf card) . gameSideSchemes)
      liftIO $ crisis `shouldBe` Just True
      dealt <- length <$> dealtTo you
      runNow [SideSchemeMessage (sideSchemeOf card) (SideSchemeThwarted (IdentityRef you) 10)]
      resolve []
      dealt' <- length <$> dealtTo you
      liftIO $ dealt' - dealt `shouldBe` 1
