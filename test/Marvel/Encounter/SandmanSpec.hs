module Marvel.Encounter.SandmanSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Attachment.Types qualified as Attachment
import Marvel.Environment.Environments.SmSandman (cityStreets, sandCounters)
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

settle :: GameAppT ()
settle = go (150 :: Int)
 where
  go 0 = error "Sandman did not settle"
  go n = openQuestion >>= \case
    Just (_, QuestionLabel "$label.yourTurn" _ _) -> pure ()
    Just (ident, q) -> do
      let choices = choicesOf q
          passing choice = isNoResponse choice || isNoDefender choice || isLabel "$label.declineToDefend" choice
          index = fromMaybe 0 $ headMay [i | (i, choice) <- zip [0 ..] choices, passing choice]
      answerQuestion ident index
      go (n - 1)
    Nothing -> pure ()

streets :: GameAppT EnvironmentId
streets = fromMaybe (error "City Streets absent") <$> cityStreets

resolveCityStreetsSpecial :: GameAppT ()
resolveCityStreetsSpecial = do
  eid <- streets
  openQuestion >>= \case
    Just (ident, QuestionLabel "$label.resolveSpecial" (Just target) q) -> do
      liftIO $ target `shouldBe` toTarget eid
      liftIO $ any (isTargetChoice eid) (choicesOf q) `shouldBe` True
      answerQuestion ident 0
    _ -> error "expected City Streets' Special prompt"

damageEnemy :: IdentityId -> EnemyId -> Natural -> GameAppT ()
damageEnemy you enemy n = resolve =<< L.runQueueT (L.capture $ L.dealDamage enemy you $ toDamage n FromAbility)

spec :: Spec
spec = describe "Sandman scenario" do
  it "sets up City Streets with four sand counters and starts at two threat per player" do
    tg <- testGameAgainst "27061" ["spider-man", "captain-marvel"]
    inGame tg do
      sandCounters `shouldReturn` 4
      mainSchemeThreatNow `shouldReturn` 4

  it "Surging Sands adds its counter before deciding how many encounter cards to discard" do
    tg <- testGameAgainst "27061" ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck (replicate 8 Treacheries.advance) []
      eid <- streets
      waiting <- openQuestion
      withGame_ $ questionL .~ mempty
      pushAll $ [CardSpecific (EnvironmentRef eid) "surgingSands" Null] <> [Ask ident q | Just (ident, q) <- [waiting]]
      runGameMessages
      sandCounters `shouldReturn` 4
      (length <$> scenarioEncounterDeckNow) `shouldReturn` 8
      resolveCityStreetsSpecial
      sandCounters `shouldReturn` 5
      length <$> scenarioEncounterDeckNow `shouldReturn` 3
      length <$> getEncounterDiscard `shouldReturn` 5

  it "City Streets exhausts a controlled ally as a cost and uses that ally's ATK" do
    tg <- testGameAgainst "27061" ["spider-man"]
    inGame tg do
      changeForm (player1 tg)
      cat <- putIntoPlay (player1 tg) "01002"
      eid <- streets
      choose "clear sand" (isAbilityOf eid)
      choose "exhaust Black Cat" (isTargetChoice $ allyIdOf cat)
      sandCounters `shouldReturn` 3
      [blackCat] <- alliesWith "01002"
      liftIO $ allyExhausted blackCat `shouldBe` True
      offers (isAbilityOf eid) `shouldReturn` False

  it "gives each player their own once-per-round City Streets action" do
    tg <- testGameAgainst "27061" ["spider-man", "captain-marvel"]
    inGame tg do
      changeForm (player1 tg)
      eid <- streets
      choose "clear sand" (isAbilityOf eid)
      choose "exhaust hero" (isTargetChoice $ player1 tg)
      sandCounters `shouldReturn` 2
      offers (isAbilityOf eid) `shouldReturn` False
      endTurn
      changeForm (player2 tg)
      choose "second player's sand action" (isAbilityOf eid)
      choose "exhaust second hero" (isTargetChoice $ player2 tg)
      sandCounters `shouldReturn` 0

  it "Sandman's indirect attack can be assigned entirely to an ally without surging sands" do
    tg <- testGameAgainst "27061" ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      cat <- putIntoPlay you "01002"
      _ <- setEncounterDeck [Treacheries.imTough, Treacheries.advance] []
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks you)]
      pass
      choose "assign first damage to Black Cat" (isTargetChoice $ allyIdOf cat)
      choose "assign second damage to Black Cat" (isTargetChoice $ allyIdOf cat)
      settle
      damageOf you `shouldReturn` 0
      sandCounters `shouldReturn` 4
      alliesWith "01002" `shouldReturn` []

  it "surges the sands when indirect attack damage reaches the identity" do
    tg <- testGameAgainst "27061" ["spider-man"]
    inGame tg do
      changeForm (player1 tg)
      _ <- setEncounterDeck (replicate 9 Treacheries.imTough) []
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks $ player1 tg)]
      settle
      damageOf (player1 tg) `shouldReturn` 2
      sandCounters `shouldReturn` 5

  it "stage II resolves Surging Sands when revealed" do
    tg <- testGameAgainst "27061" ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck (replicate 10 Treacheries.advance) []
      vid <- villainIdNow
      resolve [VillainMessage vid VillainDefeated]
      toCardCode . getCardDef <$> getVillain `shouldReturn` "27062"
      sandCounters `shouldReturn` 5

  it "Hapless Pedestrians deals three indirect damage when an acceleration token is placed" do
    tg <- testGameAgainst "27061" ["spider-man"]
    inGame tg do
      mid <- mainSchemeId
      resolve [PlaceAccelerationToken mid]
      damageOf (player1 tg) `shouldReturn` 3
      accelerationTokensNow `shouldReturn` 1

  it "Sand Form replaces ability damage, discards itself, and resolves Surging Sands" do
    tg <- testGameAgainst "27061" ["spider-man"]
    inGame tg do
      _ <- revealNow (player1 tg) Attachments.smSandForm
      vid <- villainIdNow
      damageEnemy (player1 tg) (EnemyVillainId vid) 4
      villainDamageNow `shouldReturn` 0
      sandCounters `shouldReturn` 5
      count <- getsGame (length . filter ((== "27066") . toCardCode . Attachment.attachmentCardDef . toAttrs) . HashMap.elems . gameAttachments)
      liftIO $ count `shouldBe` 0

  it "Sand Clone's ATK follows the sand counters and defeating it surges the sands" do
    tg <- testGameAgainst "27061" ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Minions.smSandClone
      n <- characterAttackValue (MinionCharacter mid)
      liftIO $ n `shouldBe` 4
      eid <- streets
      resolve [RemoveCounters (EnvironmentRef eid) 2]
      characterAttackValue (MinionCharacter mid) `shouldReturn` 2
      damageEnemy (player1 tg) (EnemyMinionId mid) 3
      sandCounters `shouldReturn` 3

  it "waits for the player to resolve Sand Clone's When Defeated" do
    tg <- testGameAgainst "27061" ["spider-man"]
    inGame tg do
      mid <- addMinion (player1 tg) Minions.smSandClone
      waiting <- openQuestion
      withGame_ $ questionL .~ mempty
      pushAll $ [MinionMessage mid MinionDefeated] <> [Ask ident q | Just (ident, q) <- [waiting]]
      runGameMessages
      sandCounters `shouldReturn` 4
      openQuestion >>= \case
        Just (ident, QuestionLabel "$label.resolveWhenDefeated" (Just target) q) -> do
          liftIO $ target `shouldBe` toTarget mid
          liftIO $ any (isTargetChoice mid) (choicesOf q) `shouldBe` True
          answerQuestion ident 0
        _ -> error "expected Sand Clone's When Defeated prompt"
      settle
      sandCounters `shouldReturn` 5

  it "Dirt Trap resolves two Surging Sands effects sequentially" do
    tg <- testGameAgainst "27061" ["spider-man"]
    inGame tg do
      card <- revealNow (player1 tg) SideSchemes.smDirtTrap
      _ <- setEncounterDeck (replicate 15 Treacheries.advance) []
      resolve [SideSchemeMessage (sideSchemeOf card) (SideSchemeThwarted (toSource $ player1 tg) 1)]
      sandCounters `shouldReturn` 6
      length <$> scenarioEncounterDeckNow `shouldReturn` 4

  it "resolves Dirt Trap's two specials when a player defeats it with a basic thwart" do
    tg <- testGameAgainst "27061" ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      card <- revealNow you SideSchemes.smDirtTrap
      _ <- setEncounterDeck (replicate 30 Treacheries.advance) []
      choose "make a basic thwart" (isHeroThwart you)
      choose "thwart Dirt Trap" (isTargetChoice $ sideSchemeOf card)
      -- Defeat pauses on the card before either special changes the board.
      sandCounters `shouldReturn` 4
      (length <$> scenarioEncounterDeckNow) `shouldReturn` 30
      openQuestion >>= \case
        Just (ident, QuestionLabel "$label.resolveWhenDefeated" (Just target) q) -> do
          liftIO $ target `shouldBe` toTarget (sideSchemeOf card)
          liftIO $ any (isTargetChoice $ sideSchemeOf card) (choicesOf q) `shouldBe` True
          answerQuestion ident 0
        _ -> error "expected Dirt Trap's When Defeated prompt"
      sandCounters `shouldReturn` 4
      (length <$> scenarioEncounterDeckNow) `shouldReturn` 30
      resolveCityStreetsSpecial
      sandCounters `shouldReturn` 5
      (length <$> scenarioEncounterDeckNow) `shouldReturn` 25
      resolveCityStreetsSpecial
      settle
      sandCounters `shouldReturn` 6
      -- The separate specials discard five, then six; the defeated card leaves
      -- play afterward, rather than replacing either special's discard.
      (length <$> scenarioEncounterDeckNow) `shouldReturn` 19
      present <- getsGame $ HashMap.member (sideSchemeOf card) . gameSideSchemes
      liftIO $ present `shouldBe` False

  it "Tidal Sands adds sand counters to its fixed two starting threat" do
    tg <- testGameAgainst "27061" ["spider-man", "captain-marvel"]
    inGame tg do
      card <- revealNow (player1 tg) SideSchemes.smTidalSands
      sideSchemeThreat (sideSchemeOf card) `shouldReturn` 6

  it "Sandslide adds two sand, then surges, and stuns only when its own discard finds a Sandman card" do
    tg <- testGameAgainst "27061" ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck (Minions.smSandClone : replicate 9 Treacheries.advance) []
      _ <- revealNow (player1 tg) Treacheries.smSandslide
      settle
      sandCounters `shouldReturn` 7
      isStunned (player1 tg) `shouldReturn` True

  it "Sandslide does not stun when none of its discarded cards belongs to Sandman" do
    tg <- testGameAgainst "27061" ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck (replicate 10 Treacheries.advance) []
      _ <- revealNow (player1 tg) Treacheries.smSandslide
      settle
      isStunned (player1 tg) `shouldReturn` False

  it "Sand Storm at zero sand adds three counters and returns itself to the encounter deck" do
    tg <- testGameAgainst "27061" ["spider-man"]
    inGame tg do
      eid <- streets
      resolve [RemoveCounters (EnvironmentRef eid) 4]
      _ <- revealNow (player1 tg) Treacheries.smSandStorm
      settle
      sandCounters `shouldReturn` 3
      damageOf (player1 tg) `shouldReturn` 0
      codes <- map toCardCode <$> scenarioEncounterDeckNow
      liftIO $ codes `shouldSatisfy` elem "27071"

  it "Sand Storm with sand deals that much indirect damage among players" do
    tg <- testGameAgainst "27061" ["spider-man"]
    inGame tg do
      _ <- revealNow (player1 tg) Treacheries.smSandStorm
      settle
      damageOf (player1 tg) `shouldReturn` 4
      sandCounters `shouldReturn` 4

  it "Sand Smash attacks a hero with +1 ATK" do
    tg <- testGameAgainst "27061" ["spider-man"]
    inGame tg do
      changeForm (player1 tg)
      _ <- setEncounterDeck (replicate 12 Treacheries.imTough) []
      _ <- revealNow (player1 tg) Treacheries.smSandSmash
      settle
      damageOf (player1 tg) `shouldReturn` 3

  it "Sand Smash in alter-ego surges sands and gains surge" do
    tg <- testGameAgainst "27061" ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck (replicate 12 Treacheries.imTough) []
      _ <- revealNow (player1 tg) Treacheries.smSandSmash
      settle
      sandCounters `shouldReturn` 5
      damageOf (player1 tg) `shouldReturn` 0

  it "stage III adds a sand counter before Surging Sands when revealed" do
    tg <- testGameAgainst "27061" ["spider-man"]
    inGame tg do
      _ <- setEncounterDeck (replicate 12 Treacheries.advance) []
      vid <- villainIdNow
      resolve [ReplaceVillain vid "27063" Advanced]
      sandCounters `shouldReturn` 6

  it "stage III has overkill and surges when damage spills past an ally to the identity" do
    tg <- testGameAgainst "27061" ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      -- Stage III's reveal discards six cards, the attack takes a boost,
      -- and Surging Sands then discards seven. Keep cards left so this test
      -- isolates overkill rather than also triggering Hapless Pedestrians.
      _ <- setEncounterDeck (replicate 30 Treacheries.imTough) []
      vid <- villainIdNow
      resolve [ReplaceVillain vid "27063" Advanced]
      cat <- putIntoPlay you "01002"
      runNow [VillainMessage vid (VillainAttacks you)]
      chooseIf isNoResponse
      choose "Black Cat defends" \case
        AllyDefend aid _ -> aid == allyIdOf cat
        _ -> False
      settle
      damageOf you `shouldReturn` 1
      sandCounters `shouldReturn` 7

  it "Sand Form is revealed by its boost ability" do
    tg <- testGameAgainst "27061" ["spider-man"]
    inGame tg do
      changeForm (player1 tg)
      _ <- setEncounterDeck (Attachments.smSandForm : replicate 10 Treacheries.imTough) []
      vid <- villainIdNow
      resolve [VillainMessage vid (VillainAttacks $ player1 tg)]
      count <- getsGame (length . filter ((== "27066") . toCardCode . Attachment.attachmentCardDef . toAttrs) . HashMap.elems . gameAttachments)
      liftIO $ count `shouldBe` 1

  it "Sandslide's boost resolves its When Revealed against the attacked player" do
    tg <- testGameAgainst "27061" ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- setEncounterDeck (Treacheries.smSandslide : Minions.smSandClone : replicate 14 Treacheries.imTough) []
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks you)]
      chooseIf isNoResponse
      choose "hero defends" isDefend
      settle
      sandCounters `shouldReturn` 7
      isStunned you `shouldReturn` True
