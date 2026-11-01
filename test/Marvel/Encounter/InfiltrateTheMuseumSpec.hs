-- | Infiltrate the Museum (The Galaxy's Most Wanted, scenario 2): Collector and The
-- Collection, The Grand Collection, the scenario's encounter cards, and the Galactic
-- Artifacts and Menagerie Medley modular sets, each played as a table would.
module Marvel.Encounter.InfiltrateTheMuseumSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.EncounterSet (EncounterSet (GalacticArtifacts, MenagerieMedley))
import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Game.Public (publicGame)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import Marvel.Window.Types (Window (..), WindowTiming (After), WindowType (TurnBegan))
import TestImport

museum :: [Text] -> IO TestGame
museum = testGameAgainst "16070"

-- | Expert: Collector II and III; Collector II's When Revealed answered with the first
-- choice (the top card into The Collection) as setup plays out.
museumExpert :: [Text] -> IO TestGame
museumExpert heroes = do
  (app, ran, logged) <- dealtGameLogged (ScenarioSetup "16070" [MenagerieMedley] Expert) (map starter heroes) 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

-- | Rhino with the modular sets given in place of his.
rhinoWith :: [EncounterSet] -> [Text] -> IO TestGame
rhinoWith sets heroes = do
  (app, ran, logged) <- dealtGameLogged (ScenarioSetup "01094" sets Normal) (map starter heroes) 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

collectionNow :: GameAppT [Card]
collectionNow = getsGame gameCollection

collectionIds :: GameAppT [CardId]
collectionIds = map toCardId <$> collectionNow

emptyCollection :: GameAppT ()
emptyCollection = withGame_ (collectionL .~ [])

-- | New copies of player cards put into The Collection, as the player's.
collect :: IdentityId -> [CardCode] -> GameAppT [PlayerCard]
collect you codes = do
  cards <- for codes \code -> do
    cardId <- getRandom
    pure $ MkPlayerCard cardId (lookupPlayerCard code) (Just you) (Just you)
  withGame_ (collectionL <>~ map PlayerCard cards)
  pure cards

villainStageNow :: GameAppT Natural
villainStageNow = Villain.villainStage . toAttrs <$> getVillain

deckOf :: IdentityId -> GameAppT [PlayerCard]
deckOf p = unDeck . (.playerIdentityDeck) <$> playerAttrs p

attachmentsWith :: CardDef -> GameAppT [Attachment.Attrs Attachment.Attachment]
attachmentsWith def = getsGame (filter ((== def) . Attachment.attachmentCardDef) . map toAttrs . HashMap.elems . gameAttachments)

theAttachment :: (HasCallStack) => CardDef -> GameAppT (Attachment.Attrs Attachment.Attachment)
theAttachment def = attachmentsWith def >>= \case
  [a] -> pure a
  as -> error $ "expected one attachment, found " <> show (length as)

isAbilityOfAttachment :: Attachment.Attrs Attachment.Attachment -> Choice -> Bool
isAbilityOfAttachment a = \case
  UseAbility ab -> abilitySource ab == AttachmentRef (Attachment.attachmentId a)
  _ -> False

isMainSchemeAbility :: MainSchemeId -> Choice -> Bool
isMainSchemeAbility m = \case
  UseAbility ab -> abilitySource ab == MainSchemeRef m
  _ -> False

victoryCodes :: GameAppT [CardCode]
victoryCodes = map toCardCode <$> getsGame gameVictoryDisplay

spec :: Spec
spec = do
  setup
  collector
  grandCollection
  treacheries
  biogramImage
  galacticArtifacts
  menagerieMedley

setup :: Spec
setup = describe "Infiltrate the Museum setup" do
  it "is Collector I and The Grand Collection, 4 threat per player, the top card of each deck in The Collection" do
    tg <- museum ["spider-man", "captain-marvel"]
    inGame tg do
      villainStageNow `shouldReturn` 1
      mainSchemeThreatNow `shouldReturn` 8
      cards <- collectionNow
      liftIO $ length cards `shouldBe` 2
      owners <- pure [pcOwner c | PlayerCard c <- cards]
      liftIO $ owners `shouldMatchList` map Just [player1 tg, player2 tg]
      -- shown on the board
      view <- liftIO . publicGame =<< getGame
      let shown = [c | Object o <- [view], Just (Array cs) <- [KeyMap.lookup "collection" o], c <- toList cs]
      liftIO $ length shown `shouldBe` 2

  it "shuffles Infiltrate the Museum, Galactic Artifacts and Menagerie Medley into the encounter deck" do
    tg <- museum ["spider-man"]
    inGame tg do
      deck <- scenarioEncounterDeckNow
      discard <- encounterDiscardNow
      let codes = map toCardCode (deck <> discard)
      liftIO do
        codes `shouldSatisfy` elem (toCardCode Treacheries.viewTheCosmos)
        codes `shouldSatisfy` elem (toCardCode SideSchemes.crystalBall)
        codes `shouldSatisfy` elem (toCardCode Minions.starshark)

  it "in expert mode is Collector II, whose When Revealed asks each player for the top card of their deck or 3 damage" do
    tg <- museumExpert ["spider-man"]
    inGame tg do
      villainStageNow `shouldReturn` 2
      -- setup's top card, and Collector II's as the first choice
      length <$> collectionNow `shouldReturn` 2

collector :: Spec
collector = describe "Collector" do
  it "I: a minion discarded from play goes faceup into The Collection instead of the discard pile" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      emptyCollection
      m <- addMinion you Minions.servantBot
      resolve [RemoveFromPlay (MinionRef m)]
      ids <- collectionIds
      liftIO $ ids `shouldBe` [CardId (unMinionId m)]
      encounterDiscardCodes >>= (`shouldSatisfy` notElem (toCardCode Minions.servantBot))

  it "I: a defeated minion and a defeated ally go into The Collection" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      emptyCollection
      m <- addMinion you Minions.psionicGhost
      resolve [MinionMessage m (MinionDamaged (IdentityRef you) (toDamage 10 FromAbility))]
      cat <- putIntoPlay you "01002"
      resolve [AllyMessage (allyIdOf cat) (AllyDamaged (MinionRef m) (toDamage 10 FromAbility))]
      ids <- collectionIds
      liftIO $ ids `shouldMatchList` [CardId (unMinionId m), pcCardId cat]
      map pcCardId <$> discardOf you >>= (`shouldSatisfy` notElem (pcCardId cat))

  it "does not take an obligation resolved and discarded, never having entered play" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      emptyCollection
      -- exhausted, Peter Parker can only discard a card and have it gain surge
      resolve [IdentityMessage you ExhaustedIdentity]
      _ <- stackEncounterDeck [Minions.servantBot]
      notice <- encounterCard Obligations.evictionNotice
      resolve [RevealEncounterCard you notice]
      collectionNow `shouldReturn` []
      encounterDiscardCodes >>= (`shouldSatisfy` elem (toCardCode Obligations.evictionNotice))

  it "does not take a card that is not discarded from play: a treachery, a card discarded from the encounter deck, a side scheme to the victory display" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      emptyCollection
      clearHand you
      -- View the Cosmos with an empty hand: resolved and discarded, never in play
      _ <- revealNow you Treacheries.viewTheCosmos
      _ <- stackEncounterDeck [Minions.servantBot]
      resolve [DiscardTopOfEncounterDeck 1 Nothing]
      codes <- encounterDiscardCodes
      liftIO do
        codes `shouldSatisfy` elem (toCardCode Treacheries.viewTheCosmos)
        codes `shouldSatisfy` elem (toCardCode Minions.servantBot)
      -- Victory 0: defeated, to the victory display
      card <- encounterCard SideSchemes.philosophersStone
      runNow [RevealEncounterCard you card]
      resolve [SideSchemeMessage (sideSchemeOf card) (SideSchemeThwarted (IdentityRef you) 10)]
      chooseIf (isLabel "$label.pass")
      victoryCodes >>= (`shouldSatisfy` elem (toCardCode SideSchemes.philosophersStone))
      collectionNow `shouldReturn` []

  it "does not take a card discarded from a hand" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      emptyCollection
      clearHand you
      cat <- putInHand you "01002"
      _ <- revealNow you Treacheries.viewTheCosmos
      choose "discard it for threat" (isLabel "$label.discardHighestCostForThreat")
      map pcCardId <$> discardOf you >>= (`shouldSatisfy` elem (pcCardId cat))
      collectionNow `shouldReturn` []

  it "I and II place no threat as they take a card; III places 1 threat for each" do
    tg <- museumExpert ["spider-man"]
    let you = player1 tg
    inGame tg do
      vid <- villainIdNow
      resolve [VillainMessage vid VillainDefeated]
      villainStageNow `shouldReturn` 3
      before <- mainSchemeThreatNow
      m <- addMinion you Minions.servantBot
      resolve [RemoveFromPlay (MinionRef m)]
      mainSchemeThreatNow `shouldReturn` before + 1
      ids <- collectionIds
      liftIO $ ids `shouldSatisfy` elem (CardId (unMinionId m))

  it "III's When Revealed puts the top card of each deck into The Collection, then 1 threat for each card there" do
    tg <- museumExpert ["spider-man"]
    let you = player1 tg
    inGame tg do
      emptyCollection
      _ <- collect you ["01002", "01003"]
      top <- take 1 <$> deckOf you
      before <- mainSchemeThreatNow
      vid <- villainIdNow
      resolve [VillainMessage vid VillainDefeated]
      ids <- collectionIds
      liftIO $ ids `shouldSatisfy` elem (pcCardId (fromMaybe (error "no deck") (headMay top)))
      mainSchemeThreatNow `shouldReturn` before + 3

  it "II's When Revealed: a player may take 3 damage instead" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      emptyCollection
      vid <- villainIdNow
      runNow [VillainMessage vid VillainDefeated]
      chooseIf isAnyAbility
      choose "take 3 damage" (isLabel (countLabel "takeDamage" 3))
      damageOf you `shouldReturn` 3
      collectionNow `shouldReturn` []

  it "standard Collector II, defeated, is the last stage" do
    tg <- museum ["spider-man"]
    inGame tg do
      vid <- villainIdNow
      resolve [VillainMessage vid VillainDefeated]
      villainStageNow `shouldReturn` 2
      vid' <- villainIdNow
      resolve [VillainMessage vid' VillainDefeated]
      getsGame gameState `shouldReturn` Finished Won

grandCollection :: Spec
grandCollection = describe "The Grand Collection" do
  it "the players lose with 5 cards per player in The Collection" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      emptyCollection
      _ <- collect you ["01002", "01003", "01004"]
      top <- deckOf you
      resolve [PlaceInCollection (PlayerCard c) | c <- take 1 top]
      getsGame gameState >>= (`shouldSatisfy` (/= Finished Lost))
      resolve [PlaceInCollection (PlayerCard c) | c <- take 1 (drop 1 top)]
      getsGame gameState `shouldReturn` Finished Lost

  it "the players lose when the stage is completed" do
    tg <- museum ["spider-man"]
    inGame tg do
      placeMainThreat 20
      getsGame gameState `shouldReturn` Finished Lost

  it "Hero Action: exhaust your hero to discard a card from The Collection to its owner's discard pile, once per round" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      emptyCollection
      [cat, flip'] <- collect you ["01002", "01003"]
      m <- mainSchemeIdNow
      offers (isMainSchemeAbility m) `shouldReturn` False -- alter-ego form
      changeForm you
      clearHand you
      choose "the main scheme's action" (isMainSchemeAbility m)
      choose "Black Cat" (isTargetChoice (CardIdRef $ pcCardId cat))
      exhaustedNow you `shouldReturn` True
      ids <- collectionIds
      liftIO $ ids `shouldBe` [pcCardId flip']
      map pcCardId <$> discardOf you >>= (`shouldSatisfy` elem (pcCardId cat))
      -- the limit holds for its other cost too
      _ <- putInHand you "01090"
      offers (isMainSchemeAbility m) `shouldReturn` False

  it "Hero Action: once per round per player, each player's own" do
    tg <- museum ["spider-man", "she-hulk"]
    let (you, them) = (player1 tg, player2 tg)
    inGame tg do
      emptyCollection
      [cat, flip'] <- collect you ["01002", "01003"]
      m <- mainSchemeIdNow
      changeForm you
      choose "the main scheme's action" (isMainSchemeAbility m)
      choose "Black Cat" (isTargetChoice (CardIdRef $ pcCardId cat))
      offers (isMainSchemeAbility m) `shouldReturn` False
      endTurn
      changeForm them
      pass
      choose "the main scheme's action, the other player's" (isMainSchemeAbility m)
      choose "the last card" (isTargetChoice (CardIdRef $ pcCardId flip'))
      collectionNow `shouldReturn` []

  it "Hero Action: or spend 2 resources of any type" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      emptyCollection
      [cat] <- collect you ["01002"]
      m <- mainSchemeIdNow
      changeForm you
      resolve [IdentityMessage you ExhaustedIdentity]
      clearHand you
      offers (isMainSchemeAbility m) `shouldReturn` False -- exhausted, nothing to spend
      strength <- putInHand you "01090"
      choose "the main scheme's action" (isMainSchemeAbility m)
      payWith [strength]
      chooseIf (isTargetChoice (CardIdRef $ pcCardId cat))
      collectionNow `shouldReturn` []

  it "Hero Action: not offered with The Collection empty" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      emptyCollection
      changeForm you
      m <- mainSchemeIdNow
      offers (isMainSchemeAbility m) `shouldReturn` False

treacheries :: Spec
treacheries = describe "Infiltrate the Museum treacheries" do
  it "Inconspicuous Box puts the lowest cost card you control into The Collection, the player picking among ties" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      emptyCollection
      tracer <- putIntoPlay you "01007"
      shooter <- putIntoPlay you "01008"
      _ <- putIntoPlay you "01002"
      _ <- revealNow you Treacheries.inconspicuousBox
      choose "Web-Shooter" (isTargetChoice (UpgradeRef $ upgradeIdOf shooter))
      ids <- collectionIds
      liftIO $ ids `shouldBe` [pcCardId shooter]
      upgradeCodes `shouldReturn` ["01007"]
      liftIO $ pcCardId tracer `notElem` ids `shouldBe` True

  it "Inconspicuous Box gains surge with no card to put there" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      before <- length <$> dealtTo you
      _ <- stackEncounterDeck [Minions.servantBot]
      _ <- revealNow you Treacheries.inconspicuousBox
      chooseIf isNoResponse
      minions <- getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameMinions)
      dealt <- length <$> dealtTo you
      liftIO $ (toCardCode Minions.servantBot `elem` minions || dealt > before) `shouldBe` True

  it "Inconspicuous Box's boost puts the top card of your deck into The Collection with 3 per player or fewer there" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      emptyCollection
      _ <- collect you ["01002", "01003", "01004"]
      changeForm you
      _ <- stackEncounterDeck [Treacheries.inconspicuousBox]
      top <- take 1 <$> deckOf you
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainAttacks you)]
      ids <- collectionIds
      liftIO $ map pcCardId top `shouldSatisfy` all (`elem` ids)
      -- four now: no more
      _ <- stackEncounterDeck [Treacheries.inconspicuousBox]
      resolve [VillainMessage vid (VillainAttacks you)]
      length <$> collectionNow `shouldReturn` 4

  it "View the Cosmos: the highest cost card from your hand into The Collection" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      emptyCollection
      clearHand you
      _ <- putInHand you "01003"
      kick <- putInHand you "01005"
      _ <- revealNow you Treacheries.viewTheCosmos
      choose "into The Collection" (isLabel "$label.highestCostIntoTheCollection")
      ids <- collectionIds
      liftIO $ ids `shouldBe` [pcCardId kick]
      map pcCardId <$> handOf you >>= (`shouldSatisfy` notElem (pcCardId kick))

  it "View the Cosmos: or discard it, threat on the main scheme for its printed cost" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearHand you
      kick <- putInHand you "01005"
      before <- mainSchemeThreatNow
      _ <- revealNow you Treacheries.viewTheCosmos
      choose "discard for threat" (isLabel "$label.discardHighestCostForThreat")
      mainSchemeThreatNow `shouldReturn` before + 3
      map pcCardId <$> discardOf you >>= (`shouldSatisfy` elem (pcCardId kick))

  it "Stay Awhile (alter-ego): spend [physical][physical], or the top card of your deck into The Collection" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      emptyCollection
      clearHand you
      strength <- putInHand you "01090"
      _ <- revealNow you Treacheries.stayAwhile
      choose "spend" (isLabel (countLabel "spendPhysicalResources" 2))
      payWith [strength]
      collectionNow `shouldReturn` []
      top <- take 1 <$> deckOf you
      _ <- revealNow you Treacheries.stayAwhile
      ids <- collectionIds
      -- nothing to spend: the top card it is
      liftIO $ ids `shouldBe` map pcCardId top

  it "Stay Awhile (hero): Collector attacks with +1 ATK; damage taken puts the top card of your deck into The Collection" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      emptyCollection
      changeForm you
      _ <- stackEncounterDeck [SideSchemes.crystalBall]
      top <- take 1 <$> deckOf you
      _ <- revealNow you Treacheries.stayAwhile
      chooseIf isNoResponse
      chooseIf isNoDefender
      chooseIf isNoResponse
      -- Collector's 1 ATK, the boost card's 1 icon and the 1 more
      damageOf you `shouldReturn` 3
      ids <- collectionIds
      liftIO $ ids `shouldBe` map pcCardId top

  it "Stay Awhile (hero): no damage taken, no card" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      emptyCollection
      changeForm you
      resolve [IdentityMessage you IdentityBecomeTough]
      _ <- stackEncounterDeck [SideSchemes.crystalBall]
      _ <- revealNow you Treacheries.stayAwhile
      chooseIf isNoResponse
      chooseIf isNoDefender
      chooseIf isNoResponse
      damageOf you `shouldReturn` 0
      collectionNow `shouldReturn` []

  it "Caught Off Guard: the support or upgrade it discards from play goes into The Collection" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      emptyCollection
      may <- putIntoPlay you "01006"
      _ <- revealNow you Treacheries.caughtOffGuardMuseum
      chooseIf (isTargetChoice (SupportRef $ supportIdOf may))
      -- Collector's forced interrupt
      chooseIf isAnyAbility
      ids <- collectionIds
      liftIO $ ids `shouldBe` [pcCardId may]
      map pcCardId <$> discardOf you >>= (`shouldSatisfy` notElem (pcCardId may))

  it "Caught Off Guard: with no upgrade or support, it gains surge" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      before <- length <$> dealtTo you
      _ <- stackEncounterDeck [Minions.servantBot]
      _ <- revealNow you Treacheries.caughtOffGuardMuseum
      chooseIf isNoResponse
      minions <- getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameMinions)
      dealt <- length <$> dealtTo you
      liftIO $ (toCardCode Minions.servantBot `elem` minions || dealt > before) `shouldBe` True

biogramImage :: Spec
biogramImage = describe "Biogram Image" do
  it "attaches to Collector; as he would take damage it goes into The Collection, preventing it, the damage placed as threat" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      emptyCollection
      _ <- revealNow you Attachments.biogramImage
      image <- theAttachment Attachments.biogramImage
      vid <- villainIdNow
      liftIO $ Attachment.attachmentEnemy image `shouldBe` Just (EnemyVillainId vid)
      changeForm you
      before <- mainSchemeThreatNow
      choose "attack" (isHeroAttack you)
      choose "Collector" (isTargetChoice (VillainRef vid))
      chooseIf isAnyAbility
      pass
      villainDamageNow `shouldReturn` 0
      -- Spider-Man's 2 ATK
      mainSchemeThreatNow `shouldReturn` before + 2
      ids <- collectionIds
      liftIO $ ids `shouldBe` [CardId (unAttachmentId (Attachment.attachmentId image))]
      attachmentsWith Attachments.biogramImage `shouldReturn` []

  it "a tough Collector's tough status card prevents the damage first: the image stays, no threat" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      emptyCollection
      _ <- revealNow you Attachments.biogramImage
      vid <- villainIdNow
      resolve [VillainMessage vid VillainBecomeTough]
      changeForm you
      before <- mainSchemeThreatNow
      choose "attack" (isHeroAttack you)
      choose "Collector" (isTargetChoice (VillainRef vid))
      offers isAnyAbility `shouldReturn` False
      pass
      villainDamageNow `shouldReturn` 0
      Villain.villainTough . toAttrs <$> getVillain `shouldReturn` False
      mainSchemeThreatNow `shouldReturn` before
      collectionNow `shouldReturn` []
      length <$> attachmentsWith Attachments.biogramImage `shouldReturn` 1

  it "its boost reveals it after the activation" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      changeForm you
      _ <- stackEncounterDeck [Attachments.biogramImage]
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainAttacks you)]
      length <$> attachmentsWith Attachments.biogramImage `shouldReturn` 1

galacticArtifacts :: Spec
galacticArtifacts = describe "Galactic Artifacts" do
  it "Cloak of Hercules: with enemies tied for the lowest ATK, the first player picks, whoever revealed it" do
    tg <- rhinoWith [GalacticArtifacts] ["spider-man", "captain-marvel"]
    let (you, them) = (player1 tg, player2 tg)
    inGame tg do
      m1 <- addMinion them Minions.servantBot
      m2 <- addMinion them Minions.servantBot
      _ <- revealNow them Attachments.cloakOfHercules
      (fmap fst <$> asked) `shouldReturn` Just you
      choose "the second bot" (isTargetChoice (MinionRef m2))
      cloak <- theAttachment Attachments.cloakOfHercules
      liftIO $ Attachment.attachmentEnemy cloak `shouldBe` Just (EnemyMinionId m2)
      liftIO $ m1 `shouldNotBe` m2

  it "Cloak of Hercules attaches to the enemy with the lowest ATK, +3 ATK, discarded for [physical][physical][physical]" do
    tg <- rhinoWith [GalacticArtifacts] ["spider-man"]
    let you = player1 tg
    inGame tg do
      m <- addMinion you Minions.servantBot
      _ <- revealNow you Attachments.cloakOfHercules
      cloak <- theAttachment Attachments.cloakOfHercules
      liftIO $ Attachment.attachmentEnemy cloak `shouldBe` Just (EnemyMinionId m)
      getModifiers (MinionRef m) >>= (`shouldSatisfy` elem (AttackModifier 3))
      changeForm you
      clearHand you
      offers (isAbilityOfAttachment cloak) `shouldReturn` False
      s1 <- putInHand you "01090"
      s2 <- putInHand you "01008"
      choose "discard the cloak" (isAbilityOfAttachment cloak)
      payWith [s1, s2]
      attachmentsWith Attachments.cloakOfHercules `shouldReturn` []

  it "Obedience Potion: -1 THW, ATK and DEF to your identity; any player may take 1 damage and spend [mental][mental] to discard it" do
    tg <- rhinoWith [GalacticArtifacts] ["spider-man", "captain-marvel"]
    let you = player1 tg
        other = player2 tg
    inGame tg do
      _ <- revealNow you Attachments.obediencePotion
      potion <- theAttachment Attachments.obediencePotion
      liftIO $ Attachment.attachmentIdentity potion `shouldBe` Just you
      getModifiers (IdentityRef you) >>= (`shouldSatisfy` \ms -> all (`elem` ms) [ThwartPenalty 1, AttackPenalty 1, DefensePenalty 1])
      -- the other player may, in their turn
      endTurn
      changeForm other
      clearHand other
      genius <- putInHand other "01089"
      choose "discard the potion" (isAbilityOfAttachment potion)
      payWith [genius]
      damageOf other `shouldReturn` 1
      attachmentsWith Attachments.obediencePotion `shouldReturn` []

  it "The Beyonder's Blazer attaches to the highest SCH, its amplify icon in play; 2 threat and 2 resources of one type discard it" do
    tg <- rhinoWith [GalacticArtifacts] ["spider-man"]
    let you = player1 tg
    inGame tg do
      vid <- villainIdNow
      _ <- addMinion you Minions.servantBot
      _ <- revealNow you Attachments.theBeyondersBlazer
      -- Rhino and the bot tie at 1 SCH
      choose "Rhino" (isTargetChoice (VillainRef vid))
      blazer <- theAttachment Attachments.theBeyondersBlazer
      liftIO $ Attachment.attachmentEnemy blazer `shouldBe` Just (EnemyVillainId vid)
      getAmplifyCount `shouldReturn` 1
      changeForm you
      clearHand you
      strength <- putInHand you "01090"
      before <- mainSchemeThreatNow
      offers (isNamedAbility (countLabel "spendMentalResources" 2)) `shouldReturn` False
      choose "spend physical" (isNamedAbility (countLabel "spendPhysicalResources" 2))
      payWith [strength]
      mainSchemeThreatNow `shouldReturn` before + 2
      attachmentsWith Attachments.theBeyondersBlazer `shouldReturn` []

  it "The Poison: as your turn begins, a poison counter, then 1 damage for each" do
    tg <- rhinoWith [GalacticArtifacts] ["spider-man"]
    let you = player1 tg
    inGame tg do
      _ <- revealNow you Attachments.thePoison
      resolve [CheckWindows [Window After (TurnBegan you)]]
      damageOf you `shouldReturn` 1
      resolve [CheckWindows [Window After (TurnBegan you)]]
      damageOf you `shouldReturn` 3

  it "The Poison: spend 3 resources of different types to discard it" do
    tg <- rhinoWith [GalacticArtifacts] ["spider-man"]
    let you = player1 tg
    inGame tg do
      _ <- revealNow you Attachments.thePoison
      poison <- theAttachment Attachments.thePoison
      changeForm you
      clearHand you
      a <- putInHand you "01088"
      offers (isAbilityOfAttachment poison) `shouldReturn` False
      b <- putInHand you "01089"
      c <- putInHand you "01090"
      choose "discard the poison" (isAbilityOfAttachment poison)
      payWith [a, b, c]
      attachmentsWith Attachments.thePoison `shouldReturn` []

  it "Vandarian Power Wand attaches to the lowest SCH, its hazard icon counted" do
    tg <- rhinoWith [GalacticArtifacts] ["spider-man"]
    let you = player1 tg
    inGame tg do
      m <- addMinion you Minions.servantBot
      _ <- revealNow you Attachments.vandarianPowerWand
      choose "the bot" (isTargetChoice (MinionRef m))
      wand <- theAttachment Attachments.vandarianPowerWand
      liftIO $ Attachment.attachmentEnemy wand `shouldBe` Just (EnemyMinionId m)
      liftIO $ cdHazards Attachments.vandarianPowerWand `shouldBe` 1

  it "a side scheme enters with 3 and Hinder 2 per player; defeated, it goes to the victory display and its player may ready" do
    tg <- rhinoWith [GalacticArtifacts] ["spider-man", "captain-marvel"]
    let you = player1 tg
    inGame tg do
      card <- encounterCard SideSchemes.hujahdarianMonarchEgg
      runNow [RevealEncounterCard you card]
      sideSchemeThreat (sideSchemeOf card) `shouldReturn` 7
      resolve [IdentityMessage you ExhaustedIdentity]
      runNow [SideSchemeMessage (sideSchemeOf card) (SideSchemeThwarted (IdentityRef you) 7)]
      choose "ready" (isLabel "$label.readyIdentity")
      exhaustedNow you `shouldReturn` False
      victoryCodes `shouldReturn` [toCardCode SideSchemes.hujahdarianMonarchEgg]

  it "Magical Teapot may heal 4; Philosopher's Stone may draw 2" do
    tg <- rhinoWith [GalacticArtifacts] ["spider-man"]
    let you = player1 tg
    inGame tg do
      setDamage you 6
      teapot <- encounterCard SideSchemes.magicalTeapot
      runNow [RevealEncounterCard you teapot]
      runNow [SideSchemeMessage (sideSchemeOf teapot) (SideSchemeThwarted (IdentityRef you) 10)]
      choose "heal" (isLabel (countLabel "healYourIdentity" 4))
      damageOf you `shouldReturn` 2
      hand <- length <$> handOf you
      stone <- encounterCard SideSchemes.philosophersStone
      runNow [RevealEncounterCard you stone]
      runNow [SideSchemeMessage (sideSchemeOf stone) (SideSchemeThwarted (IdentityRef you) 10)]
      choose "draw" (isLabel (countLabel "drawCards" 2))
      length <$> handOf you `shouldReturn` hand + 2

  it "the side schemes' choice may be declined" do
    tg <- rhinoWith [GalacticArtifacts] ["spider-man"]
    let you = player1 tg
    inGame tg do
      hand <- length <$> handOf you
      stone <- encounterCard SideSchemes.philosophersStone
      runNow [RevealEncounterCard you stone]
      runNow [SideSchemeMessage (sideSchemeOf stone) (SideSchemeThwarted (IdentityRef you) 10)]
      choose "pass" (isLabel "$label.pass")
      length <$> handOf you `shouldReturn` hand

  it "Crystal Ball: its player may play a card from their hand, 3 resources cheaper" do
    tg <- rhinoWith [GalacticArtifacts] ["spider-man"]
    let you = player1 tg
    inGame tg do
      clearHand you
      cat <- putInHand you "01002"
      ball <- encounterCard SideSchemes.crystalBall
      runNow [RevealEncounterCard you ball]
      runNow [SideSchemeMessage (sideSchemeOf ball) (SideSchemeThwarted (IdentityRef you) 10)]
      choose "play Black Cat" (isPlay cat)
      chooseIf isFinishPayment
      length <$> alliesWith "01002" `shouldReturn` 1
      -- the reduction is gone once used
      getModifiers (IdentityRef you) >>= (`shouldSatisfy` notElem (ResourceCostReduction 3))

  it "Crystal Ball offers only cards its player may legally play now, not just afford" do
    tg <- rhinoWith [GalacticArtifacts] ["spider-man"]
    let you = player1 tg
    inGame tg do
      _ <- putIntoPlay you "01002"
      clearHand you
      cat <- putInHand you "01002" -- unique, one in play
      backflip <- putInHand you "01003" -- an interrupt
      kick <- putInHand you "01005" -- a hero action, Peter Parker in alter-ego form
      may <- putInHand you "01006"
      ball <- encounterCard SideSchemes.crystalBall
      runNow [RevealEncounterCard you ball]
      runNow [SideSchemeMessage (sideSchemeOf ball) (SideSchemeThwarted (IdentityRef you) 10)]
      offered <- for [cat, backflip, kick, may] (offers . isPlay)
      liftIO $ offered `shouldBe` [False, False, False, True]

menagerieMedley :: Spec
menagerieMedley = describe "Menagerie Medley" do
  it "Psionic Ghost confuses you; already confused, you take 1 damage" do
    tg <- rhinoWith [MenagerieMedley] ["spider-man"]
    let you = player1 tg
    inGame tg do
      _ <- addMinion you Minions.psionicGhost
      isConfused you `shouldReturn` True
      damageOf you `shouldReturn` 0
      _ <- addMinion you Minions.psionicGhost
      damageOf you `shouldReturn` 1

  it "Psionic Ghost's boost puts it into play engaged with you" do
    tg <- rhinoWith [MenagerieMedley] ["spider-man"]
    let you = player1 tg
    inGame tg do
      changeForm you
      _ <- stackEncounterDeck [Minions.psionicGhost]
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainAttacks you)]
      codes <- getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameMinions)
      liftIO $ codes `shouldBe` [toCardCode Minions.psionicGhost]

  it "Servant Bot has guard and patrol" do
    tg <- rhinoWith [MenagerieMedley] ["spider-man"]
    let you = player1 tg
    inGame tg do
      changeForm you
      _ <- addMinion you Minions.servantBot
      vid <- villainIdNow
      choose "attack" (isHeroAttack you)
      offers (isTargetChoice vid) `shouldReturn` False

  it "Starshark's attack deals indirect damage, divided among your characters" do
    tg <- rhinoWith [MenagerieMedley] ["spider-man"]
    let you = player1 tg
    inGame tg do
      cat <- putIntoPlay you "01002"
      m <- addMinion you Minions.starshark
      changeForm you
      runNow [MinionMessage m (MinionAttacks you)]
      chooseIf isNoResponse
      chooseIf isNoDefender
      chooseIf isNoResponse
      -- 3 to assign: 2 to Black Cat (all she has), the rest to the hero
      choose "Black Cat" (isTargetChoice (AllyRef $ allyIdOf cat))
      chooseIf (isTargetChoice (AllyRef $ allyIdOf cat))
      chooseIf (isTargetChoice you)
      damageOf you `shouldReturn` 1
      alliesWith "01002" `shouldReturn` []

  it "Starshark's boost deals 1 damage to each character you control" do
    tg <- rhinoWith [MenagerieMedley] ["spider-man"]
    let you = player1 tg
    inGame tg do
      changeForm you
      _ <- putIntoPlay you "01020"
      _ <- stackEncounterDeck [Minions.starshark]
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainAttacks you)]
      [hellcat] <- alliesWith "01020"
      liftIO $ allyDamage hellcat `shouldBe` 1

  it "Monark Starstalker is villainous: given a boost card as it attacks" do
    tg <- museum ["spider-man"]
    let you = player1 tg
    inGame tg do
      changeForm you
      m <- addMinion you Minions.monarkStarstalker
      _ <- stackEncounterDeck [Attachments.theBeyondersBlazer]
      before <- damageOf you
      resolve [MinionMessage m (MinionAttacks you)]
      -- 2 ATK and the boost card's 3 icons
      damageOf you `shouldReturn` before + 5
