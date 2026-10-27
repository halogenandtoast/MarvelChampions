-- | The Galaxy's Most Wanted modular sets the engine foundation brings: Ship Command, the
-- Power Stone and the Badoon Headhunter, each card played as a table would.
module Marvel.Encounter.GalaxysMostWantedModularsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as AttachmentCards
import Marvel.Attachment.Types (Attrs (..))
import Marvel.EncounterSet (EncounterSet (BadoonHeadhunter, PowerStone, ShipCommand))
import Marvel.Environment.Cards qualified as EnvironmentCards
import Marvel.Minion.Cards qualified as MinionCards
import Marvel.SideScheme.Cards qualified as SideSchemeCards
import Marvel.Support.Cards qualified as SupportCards
import Marvel.Treachery.Cards qualified as TreacheryCards
import Marvel.Phase (Phase (VillainPhase))
import Marvel.Villain.Types (villainBoostCards)
import TestImport

-- | Against Rhino with the modular sets given in place of his, the starter decks of the
-- heroes given, played to the first player's first turn.
gmwGame :: [EncounterSet] -> [Text] -> IO TestGame
gmwGame sets heroes = do
  (app, ran, logged) <- dealtGameLogged (ScenarioSetup "01094" sets Normal) (map starter heroes) 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

milanoAttrs :: GameAppT (Attrs Support)
milanoAttrs = supportsWith (toCardCode SupportCards.milano) >>= \case
  [m] -> pure m
  ms -> error $ "expected the Milano, found " <> show (length ms)

milanoExhausted :: GameAppT Bool
milanoExhausted = (.supportExhausted) <$> milanoAttrs

exhaustTheMilano :: GameAppT ()
exhaustTheMilano = milanoAttrs >>= \m -> resolve [SupportMessage (supportId m) ExhaustedSupport]

-- | The villain's boost cards for the activation under way.
villainBoosts :: GameAppT Int
villainBoosts = length . villainBoostCards . toAttrs <$> getVillain

spec :: Spec
spec = do
  shipCommand
  powerStoneSpec
  badoonHeadhunter

shipCommand :: Spec
shipCommand = describe "Ship Command" do
  it "the Milano begins the game in play, under the first player's control (setup)" do
    tg <- gmwGame [ShipCommand] ["spider-man", "captain-marvel"]
    inGame tg do
      m <- milanoAttrs
      deck <- scenarioEncounterDeckNow
      supports <- (.playerIdentitySupports) <$> playerAttrs (player1 tg)
      liftIO do
        m.supportController `shouldBe` player1 tg
        supportId m `elem` supports `shouldBe` True
        any ((== toCardCode SupportCards.milano) . toCardCode) deck `shouldBe` False

  it "the Milano follows the first player token as it passes" do
    tg <- gmwGame [ShipCommand] ["spider-man", "captain-marvel"]
    inGame tg do
      resolve [PassFirstPlayer]
      m <- milanoAttrs
      first <- headMay <$> players
      second <- (.playerIdentitySupports) <$> playerAttrs (player2 tg)
      firstSupports <- (.playerIdentitySupports) <$> playerAttrs (player1 tg)
      liftIO do
        first `shouldBe` Just (player2 tg)
        m.supportController `shouldBe` player2 tg
        supportId m `elem` second `shouldBe` True
        supportId m `elem` firstSupports `shouldBe` False

  it "the Milano stays in play when the first player is eliminated, for the next first player (permanent)" do
    tg <- gmwGame [ShipCommand] ["spider-man", "captain-marvel"]
    inGame tg do
      resolve [IdentityMessage (player1 tg) IdentityDefeated]
      m <- milanoAttrs
      discard <- discardOf (player2 tg)
      liftIO do
        m.supportController `shouldBe` player2 tg
        any ((== toCardCode SupportCards.milano) . toCardCode) discard `shouldBe` False

  it "the Milano's wild resource is for any player, not only the first player" do
    tg <- gmwGame [ShipCommand] ["spider-man", "captain-marvel"]
    inGame tg do
      other <- getAvailableResourcesFor (player2 tg) Nothing
      exhaustTheMilano
      afterwards <- getAvailableResourcesFor (player2 tg) Nothing
      liftIO $ (length (filter (== Wild) other) - length (filter (== Wild) afterwards)) `shouldBe` 1

  it "with peril, only the Milano's controller may use it for the player resolving the card" do
    tg <- gmwGame [ShipCommand] ["spider-man", "captain-marvel"]
    inGame tg do
      let wilds p = length . filter (== Wild) <$> getAvailableResourcesFor p Nothing
      before <- wilds (player2 tg)
      ownBefore <- wilds (player1 tg)
      resolve [SetPeril (Just (player2 tg))]
      during <- wilds (player2 tg)
      own <- wilds (player1 tg)
      -- the first player, not resolving it, may not trigger it either
      liftIO $ (before - during, ownBefore - own) `shouldBe` (1, 1)

  it "Blind Side: its revealing player may exhaust the Milano they do not control (FAQ), alone (peril)" do
    tg <- gmwGame [ShipCommand] ["spider-man", "captain-marvel"]
    inGame tg do
      card <- encounterCard TreacheryCards.blindSide
      runNow [RevealEncounterCard (player2 tg) card]
      peril <- getsGame gamePeril
      offered <- offers (isLabel "$label.exhaustTheMilano")
      stunOffered <- offers (isLabel "$label.stunTheFirstPlayer")
      choose "exhaust the Milano" (isLabel "$label.exhaustTheMilano")
      exhausted <- milanoExhausted
      after <- getsGame gamePeril
      liftIO do
        peril `shouldBe` Just (player2 tg)
        (offered, stunOffered, exhausted, after) `shouldBe` (True, True, True, Nothing)

  it "Blind Side: with the Milano exhausted and no physical resources, the first player is stunned" do
    tg <- gmwGame [ShipCommand] ["spider-man"]
    let you = player1 tg
    inGame tg do
      exhaustTheMilano
      clearHand you
      _ <- revealNow you TreacheryCards.blindSide
      stunned <- isStunned you
      liftIO $ stunned `shouldBe` True

  it "Blind Side: the resources may be spent instead" do
    tg <- gmwGame [ShipCommand] ["spider-man"]
    let you = player1 tg
    inGame tg do
      exhaustTheMilano
      clearHand you
      a <- putInHand you "01090" -- Strength: two physical resources
      _ <- revealNow you TreacheryCards.blindSide
      choose "spend the resources" (isLabel (countLabel "spendPhysicalResources" 2))
      payWith [a]
      stunned <- isStunned you
      discard <- discardOf you
      liftIO $ (stunned, a `elem` discard) `shouldBe` (False, True)

  it "Hull Breach deals 3 damage to the first player, not the revealing one" do
    tg <- gmwGame [ShipCommand] ["spider-man", "captain-marvel"]
    inGame tg do
      card <- encounterCard TreacheryCards.hullBreach
      runNow [RevealEncounterCard (player2 tg) card]
      choose "damage the first player" (isLabel (countLabel "dealDamageToTheFirstPlayer" 3))
      dmg1 <- damageOf (player1 tg)
      dmg2 <- damageOf (player2 tg)
      liftIO $ (dmg1, dmg2) `shouldBe` (3, 0)

  it "Power Siphon discards a card at random from the first player's hand" do
    tg <- gmwGame [ShipCommand] ["spider-man"]
    let you = player1 tg
    inGame tg do
      exhaustTheMilano
      before <- length <$> handOf you
      _ <- revealNow you TreacheryCards.powerSiphon
      chooseIf (isLabel "$label.discardRandomCardFromFirstPlayersHand")
      after <- length <$> handOf you
      liftIO $ before - after `shouldSatisfy` (>= 1)

  it "Special Delivery: a hero who does not exhaust the Milano is attacked with +1 ATK" do
    tg <- gmwGame [ShipCommand] ["spider-man"]
    let you = player1 tg
    inGame tg do
      changeForm you
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      _ <- revealNow you TreacheryCards.specialDelivery
      chooseIf isNoResponse
      pressWhenRevealed
      choose "do not exhaust the Milano" (isLabel "$label.doNotExhaustTheMilano")
      chooseIf isNoResponse
      chooseIf isNoDefender
      chooseIf isNoResponse
      dmg <- damageOf you
      -- Rhino's 2 ATK, the boost card's 1 icon and the 1 more
      liftIO $ dmg `shouldBe` 4

  it "Special Delivery: exhausting the Milano, nothing else happens" do
    tg <- gmwGame [ShipCommand] ["spider-man"]
    let you = player1 tg
    inGame tg do
      before <- mainSchemeThreatNow
      _ <- revealNow you TreacheryCards.specialDelivery
      choose "exhaust the Milano" (isLabel "$label.exhaustTheMilano")
      after <- mainSchemeThreatNow
      exhausted <- milanoExhausted
      liftIO $ (after - before, exhausted) `shouldBe` (0, True)

  it "Special Delivery: in alter-ego form, with the Milano exhausted, the villain schemes with +1 SCH" do
    tg <- gmwGame [ShipCommand] ["spider-man"]
    let you = player1 tg
    inGame tg do
      exhaustTheMilano
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      before <- mainSchemeThreatNow
      _ <- revealNow you TreacheryCards.specialDelivery
      resolve []
      after <- mainSchemeThreatNow
      -- Rhino's 1 SCH, the boost card's 1 icon and the 1 more
      liftIO $ after - before `shouldBe` 3

  it "Rogue Vessel surges, and deals each player 1 damage as the villain phase ends" do
    tg <- gmwGame [ShipCommand] ["spider-man", "captain-marvel"]
    inGame tg do
      dealt <- length <$> dealtTo (player1 tg)
      _ <- revealNow (player1 tg) EnvironmentCards.rogueVessel
      surged <- length <$> dealtTo (player1 tg)
      resolve [EndPhase VillainPhase]
      d1 <- damageOf (player1 tg)
      d2 <- damageOf (player2 tg)
      liftIO $ (surged - dealt, d1, d2) `shouldBe` (1, 1, 1)

  it "Rogue Vessel: the first player exhausts the Milano and spends 2 resources to discard it" do
    tg <- gmwGame [ShipCommand] ["spider-man"]
    let you = player1 tg
    inGame tg do
      card <- revealNow you EnvironmentCards.rogueVessel
      let vessel = EnvironmentId (unCardId (ecCardId card))
      clearHand you
      a <- putInHand you "01090"
      choose "use Rogue Vessel" (isAbilityOf vessel)
      payWith [a]
      gone <- getsGame (not . HashMap.member vessel . gameEnvironments)
      exhausted <- milanoExhausted
      liftIO $ (gone, exhausted) `shouldBe` (True, True)

  it "Rogue Vessel's 2 resources are not paid with the Milano exhausted for it" do
    tg <- gmwGame [ShipCommand] ["spider-man"]
    let you = player1 tg
    inGame tg do
      card <- revealNow you EnvironmentCards.rogueVessel
      let vessel = EnvironmentId (unCardId (ecCardId card))
      -- Peter Parker's own resource and the Milano's
      clearHand you
      oneAndMilano <- offers (isAbilityOf vessel)
      _ <- putInHand you "01008" -- Web-Shooter: one physical resource
      two <- offers (isAbilityOf vessel)
      liftIO $ (oneAndMilano, two) `shouldBe` (False, True)

  it "Rogue Vessel's action is the first player's only, and needs the Milano ready" do
    tg <- gmwGame [ShipCommand] ["spider-man", "captain-marvel"]
    inGame tg do
      card <- revealNow (player1 tg) EnvironmentCards.rogueVessel
      let vessel = EnvironmentId (unCardId (ecCardId card))
      firstOffered <- offers (isAbilityOf vessel)
      exhaustTheMilano
      afterExhausting <- offers (isAbilityOf vessel)
      liftIO $ (firstOffered, afterExhausting) `shouldBe` (True, False)
      endTurn
      secondOffered <- offers (isAbilityOf vessel)
      liftIO $ secondOffered `shouldBe` False

  it "Cannonade: Hinder 3 per player on its 1, and the first player's Milano removes 3" do
    tg <- gmwGame [ShipCommand] ["spider-man"]
    let you = player1 tg
    inGame tg do
      card <- revealNow you SideSchemeCards.cannonade
      let sid = sideSchemeOf card
      threat <- sideSchemeThreat sid
      choose "use Cannonade" (isAbilityOf sid)
      after <- sideSchemeThreat sid
      exhausted <- milanoExhausted
      liftIO $ (threat, after, exhausted) `shouldBe` (4, 1, True)

  it "Cannonade's amplify icon adds a boost icon to each boost card turned up for an activation" do
    tg <- gmwGame [ShipCommand] ["spider-man"]
    let you = player1 tg
    inGame tg do
      _ <- revealNow you SideSchemeCards.cannonade
      vid <- villainIdNow
      _ <- stackEncounterDeck [MinionCards.hydraMercenary]
      before <- mainSchemeThreatNow
      resolve [VillainMessage vid VillainSchemes]
      after <- mainSchemeThreatNow
      -- Rhino's 1 SCH, the boost card's 1 icon and the amplify icon's 1
      liftIO $ after - before `shouldBe` 3

  it "an amplify icon adds nothing to boost icons counted outside an activation" do
    tg <- gmwGame [ShipCommand] ["spider-man"]
    let you = player1 tg
    inGame tg do
      _ <- revealNow you SideSchemeCards.cannonade
      _ <- stackEncounterDeck [MinionCards.whiplash, MinionCards.hydraMercenary]
      _ <- revealNow you TreacheryCards.lightningBolt
      damage <- damageOf you
      liftIO $ damage `shouldBe` 3

powerStoneSpec :: Spec
powerStoneSpec = describe "Power Stone" do
  let holder = getPowerStoneHolder
  it "begins the game attached to the villain (setup)" do
    tg <- gmwGame [PowerStone] ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      h <- holder
      liftIO $ h `shouldBe` Just (VillainCharacter vid)

  it "a hero's single attack of 3 or more damage on the villain holding it takes it" do
    tg <- gmwGame [PowerStone] ["spider-man"]
    let you = player1 tg
    inGame tg do
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainDamaged (toSource you) (toDamage 2 (FromPlayerAttack you)))]
      kept <- holder
      resolve [VillainMessage vid (VillainDamaged (toSource you) (toDamage 3 FromAbility))]
      notAttack <- holder
      resolve [VillainMessage vid (VillainDamaged (toSource you) (toDamage 3 (FromPlayerAttack you)))]
      taken <- holder
      mine <- controlsPowerStone you
      liftIO do
        kept `shouldBe` Just (VillainCharacter vid)
        notAttack `shouldBe` Just (VillainCharacter vid)
        (taken, mine) `shouldBe` (Just (IdentityCharacter you), True)

  it "the villain's attack of 3 or more damage on the hero holding it takes it back" do
    tg <- gmwGame [PowerStone] ["spider-man"]
    let you = player1 tg
    inGame tg do
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainDamaged (toSource you) (toDamage 3 (FromPlayerAttack you)))]
      changeForm you
      -- Shocker's two boost icons on Rhino's 2 ATK
      _ <- stackEncounterDeck [MinionCards.shocker]
      resolve [VillainMessage vid (VillainAttacks you)]
      h <- holder
      attached <- getsGame (map (attachmentEnemy . toAttrs) . HashMap.elems . gameAttachments)
      liftIO $ (h, attached) `shouldBe` (Just (VillainCharacter vid), [Just (EnemyVillainId vid)])

  it "is permanent: discarded, it attaches to the villain again" do
    tg <- gmwGame [PowerStone] ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      stone <- maybe (error "no Power Stone") pure =<< getPowerStone
      resolve [RemoveFromPlay (AttachmentRef stone)]
      h <- holder
      discard <- encounterDiscardNow
      liftIO do
        h `shouldBe` Just (VillainCharacter vid)
        any ((== toCardCode AttachmentCards.powerStone) . toCardCode) discard `shouldBe` False

badoonHeadhunter :: Spec
badoonHeadhunter = describe "Badoon Headhunter" do
  it "Badoon Headhunter, defeated, goes to the victory display worth 2 victory points" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      m <- addMinion you MinionCards.badoonHeadhunter
      resolve [MinionMessage m (MinionDamaged (toSource you) (toDamage 7 FromAbility))]
      display <- map toCardCode <$> getVictoryDisplay
      points <- getVictoryPoints
      discard <- map toCardCode <$> encounterDiscardNow
      liftIO do
        display `shouldBe` [toCardCode MinionCards.badoonHeadhunter]
        points `shouldBe` 2
        toCardCode MinionCards.badoonHeadhunter `elem` discard `shouldBe` False

  it "Badoon Headhunter discarded, not defeated, goes to the discard pile" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      m <- addMinion you MinionCards.badoonHeadhunter
      resolve [RemoveFromPlay (MinionRef m)]
      display <- getVictoryDisplay
      discard <- map toCardCode <$> encounterDiscardNow
      liftIO $ (null display, toCardCode MinionCards.badoonHeadhunter `elem` discard) `shouldBe` (True, True)

  it "Badoon Headhunter's boost puts it into play engaged with the player" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      vid <- villainIdNow
      card <- encounterCard MinionCards.badoonHeadhunter
      resolve [RevealBoostCard card (EnemyVillainId vid), ClearBoosts]
      engaged <- getsGame (fmap (minionEngagedIdentity . toAttrs) . HashMap.lookup (minionOf card) . gameMinions)
      liftIO $ engaged `shouldBe` Just you

  it "Badoon Headhunter is villainous: it gets a boost card as it activates" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      m <- addMinion you MinionCards.badoonHeadhunter
      _ <- stackEncounterDeck [MinionCards.shocker]
      before <- mainSchemeThreatNow
      resolve [MinionMessage m MinionSchemes]
      after <- mainSchemeThreatNow
      -- its 1 SCH and Shocker's 2 boost icons
      liftIO $ after - before `shouldBe` 3

  it "On the Hunt turned up for Badoon Headhunter's activation still gives the villain a boost card, for his next" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      m <- addMinion you MinionCards.badoonHeadhunter
      _ <- stackEncounterDeck [TreacheryCards.onTheHunt, MinionCards.shocker]
      before <- villainBoosts
      threat <- mainSchemeThreatNow
      resolve [MinionMessage m MinionSchemes]
      after <- villainBoosts
      threat' <- mainSchemeThreatNow
      -- its 1 SCH and On the Hunt's 1 boost icon; Shocker waits on the villain
      liftIO $ (after - before, threat' - threat) `shouldBe` (1, 2)

  it "Headhunter's Henchman surges, and while engaged keeps its player from thwarting the main scheme (patrol)" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let you = player1 tg
    inGame tg do
      placeMainThreat 3
      changeForm you
      mainOffered <- offers (isHeroThwart you)
      dealt <- length <$> dealtTo you
      _ <- addMinion you MinionCards.headhuntersHenchman
      surged <- length <$> dealtTo you
      patrolledOffered <- offers (isHeroThwart you)
      liftIO $ (mainOffered, surged - dealt, patrolledOffered) `shouldBe` (True, 1, False)

  it "a player engaged with a patrol minion may still thwart a side scheme, but not the main scheme" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      placeMainThreat 3
      changeForm you
      _ <- addMinion you MinionCards.headhuntersHenchman
      card <- revealNow you SideSchemeCards.fugitiveRecovery
      m <- mainSchemeIdNow
      choose "thwart" (isHeroThwart you)
      mainChoice <- offers (isTargetChoice m)
      sideChoice <- offers (isTargetChoice (sideSchemeOf card))
      liftIO $ (mainChoice, sideChoice) `shouldBe` (False, True)

  it "patrol is the engaged player's only: another player may thwart the main scheme" do
    tg <- testGame ["spider-man", "captain-marvel"]
    inGame tg do
      placeMainThreat 3
      _ <- addMinion (player2 tg) MinionCards.headhuntersHenchman
      changeForm (player1 tg)
      offered <- offers (isHeroThwart (player1 tg))
      liftIO $ offered `shouldBe` True

  it "On the Hunt surges, and its player takes 2 damage or discards at random" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      dealt <- length <$> dealtTo you
      _ <- revealNow you TreacheryCards.onTheHunt
      choose "take 2 damage" (isLabel (countLabel "takeDamage" 2))
      surged <- length <$> dealtTo you
      dmg <- damageOf you
      liftIO $ (surged - dealt, dmg) `shouldBe` (1, 2)

  it "On the Hunt with an empty hand: only the damage" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearHand you
      _ <- revealNow you TreacheryCards.onTheHunt
      resolve []
      dmg <- damageOf you
      liftIO $ dmg `shouldBe` 2

  it "On the Hunt's boost gives the villain another boost card" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      card <- encounterCard TreacheryCards.onTheHunt
      before <- villainBoosts
      runNow [RevealBoostCard card (EnemyVillainId vid)]
      after <- villainBoosts
      liftIO $ after - before `shouldBe` 1

  it "Dead to Rights exhausts its player's identity, or places 2 threat when it cannot" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      before <- mainSchemeThreatNow
      _ <- revealNow you TreacheryCards.deadToRights
      exhausted <- exhaustedNow you
      mid <- mainSchemeThreatNow
      _ <- revealNow you TreacheryCards.deadToRights
      after <- mainSchemeThreatNow
      liftIO $ (exhausted, mid - before, after - mid) `shouldBe` (True, 0, 2)

  it "Fugitive Recovery surges, with Hinder 3 per player on its 2" do
    tg <- testGame ["spider-man"]
    let you = player1 tg
    inGame tg do
      dealt <- length <$> dealtTo you
      card <- revealNow you SideSchemeCards.fugitiveRecovery
      surged <- length <$> dealtTo you
      threat <- sideSchemeThreat (sideSchemeOf card)
      liftIO $ (surged - dealt, threat) `shouldBe` (1, 5)

  it "the Badoon Headhunter set is a modular set the encounter deck is built with" do
    tg <- gmwGame [BadoonHeadhunter] ["spider-man"]
    inGame tg do
      deck <- map toCardCode <$> scenarioEncounterDeckNow
      dealt <- concatMap (map toCardCode) <$> (traverse dealtTo =<< players)
      discard <- map toCardCode <$> encounterDiscardNow
      liftIO $ toCardCode MinionCards.badoonHeadhunter `elem` (deck <> dealt <> discard) `shouldBe` True
