module Marvel.Hero.ValkyrieSpec (spec) where

import Data.List (find)
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Types qualified as Ally
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (currentIdentity)
import Marvel.Keyword (Keyword (Restricted))
import Marvel.Matchers (IdentityMatcher (IdentityWithTrait), identityMatches)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Trait (Trait (Aerial))
import Marvel.Villain.Types qualified as Villain
import TestImport

playPaying :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
playPaying p code costs = do
  card <- putInHand p code
  resources <- traverse (putInHand p) costs
  playWith card resources
  pass
  pure card

identityAbility :: IdentityId -> Natural -> Choice -> Bool
identityAbility p idx = \case
  UseAbility a -> abilitySource a == IdentityRef p && abilityIndex a == idx
  _ -> False

cardAbility :: PlayerCard -> Natural -> Choice -> Bool
cardAbility card idx choice = isAbilityOfCard card choice && case choice of
  UseAbility a -> abilityIndex a == idx
  _ -> False

-- The hero action plays the physical set-aside card, with its ordinary payment
-- question after choosing the enemy to which it attaches.
playGlow :: IsRef enemy => IdentityId -> enemy -> GameAppT PlayerCard
playGlow p enemy = do
  glow <- getSetAsidePlayerCards >>= \cs -> pure $ fromMaybe (error "Death-Glow is not set aside") (find ((== "25002") . toCardCode) cs)
  resource <- putInHand p "25024"
  choose "Death Perception" (identityAbility p 1)
  choose "attach Death-Glow to the enemy" (isTargetChoice enemy)
  payWith [resource]
  pass
  pure glow

resolveGlow :: PlayerCard -> GameAppT ()
resolveGlow glow = choose "resolve Death-Glow's forced interrupt" (cardAbility glow 1)

basicAttack :: IdentityId -> GameAppT ()
basicAttack p = choose "make Valkyrie's basic attack" (identityAbility p 301)

prepareFragileMinion :: IdentityId -> GameAppT MinionId
prepareFragileMinion p = do
  mid <- addMinion p Minions.hydraMercenary
  resolve [MinionMessage mid $ MinionDamaged GameRef $ toDamage 1 FromAbility]
  pure mid

killByBasicAttack :: IdentityId -> MinionId -> GameAppT ()
killByBasicAttack p mid = basicAttack p >> choose "attack the prepared minion" (isTargetChoice mid)

heroAttackOf, heroDefenseOf :: IdentityId -> GameAppT Natural
heroAttackOf p = currentIdentity <$> getPlayer p >>= \case
  HeroSide h -> Hero.getModifiedAttack (toAttrs h)
  AlterEgoSide _ -> pure 0
heroDefenseOf p = currentIdentity <$> getPlayer p >>= \case
  HeroSide h -> Hero.getModifiedDefense (toAttrs h)
  AlterEgoSide _ -> pure 0

spec :: Spec
spec = describe "Valkyrie" do
  describe "Brunnhilde and Death Perception" do
    it "uses the physical 41-card starter and sets Death-Glow aside out of every ordinary player zone" do
      sum (map snd (starter "valkyrie").cards) `shouldBe` (41 :: Int) :: IO ()
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        glows <- filter ((== "25002") . toCardCode) <$> getSetAsidePlayerCards
        length glows `shouldBe` 1
        hand <- handOf p
        discard <- discardOf p
        deck <- unDeck . playerIdentityDeck <$> playerAttrs p
        map toCardCode (hand <> discard <> deck) `shouldSatisfy` notElem "25002"
        length (hand <> discard <> deck) `shouldBe` 40
        pcOwner (testHead glows) `shouldBe` Just p

    it "runs Setup after drawing and mulliganing, without replacing a Death-Glow kept in hand" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        [glow] <- filter ((== "25002") . toCardCode) <$> getSetAsidePlayerCards
        clearHand p
        resolve [RemoveFromGame $ toRef glow, IdentityMessage p $ AddToHand glow]
        _ <- stackDeck p ["25025", "25026", "25027", "25025", "25026"]
        resolve [IdentityMessage p DealStartingHand]
        length <$> handOf p `shouldReturn` 5
        handOf p >>= (`shouldSatisfy` notElem glow)
        filter ((== "25002") . toCardCode) <$> getSetAsidePlayerCards `shouldReturn` [glow]

    it "Setup finds Death-Glow in the discard pile as well" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        [glow] <- filter ((== "25002") . toCardCode) <$> getSetAsidePlayerCards
        resolve [RemoveFromGame $ toRef glow, DiscardedCard $ PlayerCard glow]
        before <- length <$> handOf p
        resolve [RanAbility p (IdentityRef p) 1 [] NoPayment]
        discardOf p >>= (`shouldSatisfy` notElem glow)
        length <$> handOf p `shouldReturn` before
        filter ((== "25002") . toCardCode) <$> getSetAsidePlayerCards `shouldReturn` [glow]

    it "Death Perception is unavailable in alter-ego form or without a resource to pay Death-Glow's cost" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        clearHand p
        offers (isLabeledAbility "$label.deathPerception") `shouldReturn` False
        changeForm p
        offers (identityAbility p 1) `shouldReturn` False
        _ <- putInHand p "25027"
        offers (identityAbility p 1) `shouldReturn` True

    it "plays Death-Glow on a minion, paying1 and preserving its physical id and owner" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        clearHand p
        mid <- addMinion p Minions.hydraMercenary
        changeForm p
        glow <- playGlow p mid
        [live] <- upgradesWith "25002"
        upgradeId live `shouldBe` upgradeIdOf glow
        upgradeOwner live `shouldBe` p
        upgradeAttachedEnemy live `shouldBe` Just (EnemyMinionId mid)
        getSetAsidePlayerCards >>= (`shouldSatisfy` notElem glow)
        discardOf p >>= (`shouldSatisfy` any ((== "25024") . toCardCode))
        offers (identityAbility p 1) `shouldReturn` False

    it "Death-Glow may attach to a villain as well" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        vid <- villainIdNow
        _ <- playGlow p vid
        [glow] <- upgradesWith "25002"
        upgradeAttachedEnemy glow `shouldBe` Just (EnemyVillainId vid)

    it "Not this Day detaches Death-Glow, sets it aside without discarding, and permits paying to play it again" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        mid <- addMinion p Minions.hydraMercenary
        glow <- playGlow p mid
        offers (isLabeledAbility "$label.notThisDay") `shouldReturn` False
        resolve [IdentityMessage p $ ChangedToForm B]
        choose "Not this Day" (identityAbility p 2)
        pass
        upgradesWith "25002" `shouldReturn` []
        getSetAsidePlayerCards >>= (`shouldSatisfy` elem glow)
        discardOf p >>= (`shouldSatisfy` notElem glow)
        exhaustedNow p `shouldReturn` False
        offers (identityAbility p 2) `shouldReturn` False
        resolve [IdentityMessage p $ ChangedToForm A]
        vid <- villainIdNow
        replayed <- playGlow p vid
        pcCardId replayed `shouldBe` pcCardId glow

  describe "Death-Glow's forced interrupt" do
    it "a basic attack defeating its marked minion readies Valkyrie and sets Death-Glow aside before responses" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        mid <- prepareFragileMinion p
        glow <- playGlow p mid
        killByBasicAttack p mid
        exhaustedNow p `shouldReturn` True
        resolveGlow glow
        pass
        exhaustedNow p `shouldReturn` False
        upgradesWith "25002" `shouldReturn` []
        getSetAsidePlayerCards >>= (`shouldSatisfy` elem glow)
        discardOf p >>= (`shouldSatisfy` notElem glow)
        minionDamageOf mid `shouldReturn` Nothing

    for_ [("her non-attack ability", True), ("another player's attack", False)] $ \(description, shouldReady) ->
      it ("sets Death-Glow aside after " <> description <> " and attributes the defeat correctly") do
        tg <- testGame ["valkyrie", "spider-man"]
        let p = player1 tg
            other = player2 tg
        inGame tg do
          changeForm p
          mid <- addMinion p Minions.hydraMercenary
          glow <- playGlow p mid
          resolve [IdentityMessage p ExhaustedIdentity]
          let source = if shouldReady then IdentityRef p else IdentityRef other
              damage = toDamage 3 (if shouldReady then FromAbility else FromPlayerAttack other)
          runNow [MinionMessage mid $ MinionDamaged source damage]
          resolveGlow glow
          pass
          exhaustedNow p `shouldReturn` not shouldReady
          getSetAsidePlayerCards >>= (`shouldSatisfy` elem glow)

    it "an ally defeating the marked enemy does not ready Valkyrie" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        mid <- prepareFragileMinion p
        glow <- playGlow p mid
        ally <- putIntoPlay p "01051"
        resolve [IdentityMessage p ExhaustedIdentity]
        choose "attack with the ally" $ \case UseAbility a -> abilitySource a == AllyRef (allyIdOf ally) && abilityIndex a == 301; _ -> False
        choose "attack the marked minion" (isTargetChoice mid)
        resolveGlow glow
        pass
        exhaustedNow p `shouldReturn` True
        getSetAsidePlayerCards >>= (`shouldSatisfy` elem glow)

    it "does not trigger when a different enemy is defeated" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        marked <- addMinion p Minions.hydraMercenary
        other <- prepareFragileMinion p
        glow <- playGlow p marked
        killByBasicAttack p other
        offers (cardAbility glow 1) `shouldReturn` False
        pass
        exhaustedNow p `shouldReturn` True
        length <$> upgradesWith "25002" `shouldReturn` 1

    it "also responds when a villain stage is defeated" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        changeForm p
        vid <- villainIdNow
        glow <- playGlow p vid
        hp <- getVillainHitPoints vid
        resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage (hp - 1) FromAbility]
        basicAttack p
        chooseVillain
        resolveGlow glow
        pass
        exhaustedNow p `shouldReturn` False
        getSetAsidePlayerCards >>= (`shouldSatisfy` elem glow)
        getsGame (fmap (Villain.villainStage . toAttrs) . HashMap.lookup vid . gameVillains) `shouldReturn` Just 2

  describe "Annabelle Riggs" do
    it "costs2 and searches exactly the top5 for one Valkyrie card, returning the rest and exhausting" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        annabelle <- playPaying p "25003" ["25025"]
        top <- stackDeck p ["25025", "25007", "25026", "25011", "25027", "25009"]
        before <- length . unDeck . playerIdentityDeck <$> playerAttrs p
        choose "Annabelle searches" (cardAbility annabelle 1)
        offers (isTargetChoice $ top !! 1) `shouldReturn` True
        offers (isTargetChoice $ top !! 3) `shouldReturn` True
        offers (isTargetChoice $ top !! 5) `shouldReturn` False
        offers (isTargetChoice $ testHead top) `shouldReturn` False
        choose "take Aragorn" (isTargetChoice $ top !! 1)
        pass
        handOf p >>= (`shouldSatisfy` elem (top !! 1))
        after <- unDeck . playerIdentityDeck <$> playerAttrs p
        length after `shouldBe` before - 1
        all (`elem` after) [testHead top, top !! 2, top !! 3, top !! 4, top !! 5] `shouldBe` True
        getsGame (fmap (Ally.allyExhausted . toAttrs) . HashMap.lookup (allyIdOf annabelle) . gameAllies) `shouldReturn` Just True
        offers (cardAbility annabelle 1) `shouldReturn` False

    it "has no hero-form search action" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        annabelle <- playPaying p "25003" ["25025"]
        changeForm p
        offers (cardAbility annabelle 1) `shouldReturn` False

    it "returns all searched cards without drawing when the top5 contain no Valkyrie card" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        annabelle <- playPaying p "25003" ["25025"]
        top <- stackDeck p ["25025", "25026", "25027", "25025", "25026", "25007"]
        beforeHand <- length <$> handOf p
        beforeDeck <- length . unDeck . playerIdentityDeck <$> playerAttrs p
        choose "Annabelle searches" (cardAbility annabelle 1)
        choose "no match in the top5" (isLabel "$label.noMatchingCardsFound")
        pass
        length <$> handOf p `shouldReturn` beforeHand
        after <- unDeck . playerIdentityDeck <$> playerAttrs p
        length after `shouldBe` beforeDeck
        all (`elem` after) top `shouldBe` True

    it "still searches for Valkyrie cards when another alter-ego controls her" do
      tg <- testGame ["valkyrie", "spider-man"]
      let other = player2 tg
      inGame tg do
        annabelle <- putIntoPlay other "25003"
        top <- stackDeck other ["01002", "25009", "01088", "01089", "01090"]
        endTurn
        choose "Annabelle searches for her controller" (cardAbility annabelle 1)
        offers (isTargetChoice $ testHead top) `shouldReturn` False
        choose "take the Valkyrie event" (isTargetChoice $ top !! 1)
        pass
        handOf other >>= (`shouldSatisfy` elem (top !! 1))

  describe "Valhalla" do
    it "costs2 and exhausts after Valkyrie's marked attack defeat to draw1 and heal1" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        valhalla <- playPaying p "25004" ["25025"]
        changeForm p
        setDamage p 2
        mid <- prepareFragileMinion p
        glow <- playGlow p mid
        before <- length <$> handOf p
        killByBasicAttack p mid
        resolveGlow glow
        choose "Valhalla" (cardAbility valhalla 1)
        pass
        length <$> handOf p `shouldReturn` before + 1
        damageOf p `shouldReturn` 1
        getsGame (fmap (supportExhausted . toAttrs) . HashMap.lookup (supportIdOf valhalla) . gameSupports) `shouldReturn` Just True

    for_ ["ability", "ally", "other hero"] $ \kind ->
      it ("does not respond to a marked defeat caused by an " <> kind) do
        tg <- testGame ["valkyrie", "spider-man"]
        let p = player1 tg
        inGame tg do
          valhalla <- playPaying p "25004" ["25025"]
          changeForm p
          mid <- addMinion p Minions.hydraMercenary
          glow <- playGlow p mid
          ally <- putIntoPlay p "01051"
          let (source, damage) = case kind of
                "ability" -> (IdentityRef p, toDamage 3 FromAbility)
                "ally" -> (AllyRef $ allyIdOf ally, toDamage 3 $ FromAllyAttack $ allyIdOf ally)
                _ -> (IdentityRef $ player2 tg, toDamage 3 $ FromPlayerAttack $ player2 tg)
          runNow [MinionMessage mid $ MinionDamaged source damage]
          resolveGlow glow
          offers (cardAbility valhalla 1) `shouldReturn` False
          pass

    it "does not respond to defeating an unmarked enemy" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        valhalla <- playPaying p "25004" ["25025"]
        changeForm p
        marked <- addMinion p Minions.hydraMercenary
        other <- prepareFragileMinion p
        _ <- playGlow p marked
        killByBasicAttack p other
        offers (cardAbility valhalla 1) `shouldReturn` False
        pass

    it "cannot answer a second defeat while exhausted" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        valhalla <- playPaying p "25004" ["25025"]
        changeForm p
        first <- prepareFragileMinion p
        glow <- playGlow p first
        killByBasicAttack p first
        resolveGlow glow
        choose "first Valhalla response" (cardAbility valhalla 1)
        pass
        second <- prepareFragileMinion p
        again <- playGlow p second
        killByBasicAttack p second
        resolveGlow again
        offers (cardAbility valhalla 1) `shouldReturn` False
        pass

  describe "Valkyrie's Spear and Dragonfang" do
    it "Spear costs1, gives +1DEF normally and +2 instead against the marked enemy" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        _ <- playPaying p "25005" ["25024"]
        marked <- addMinion p Minions.vulture
        unmarked <- addMinion p Minions.tigerShark
        changeForm p
        _ <- playGlow p marked
        heroDefenseOf p `shouldReturn` 2
        runNow [MinionMessage marked $ MinionAttacks p]
        choose "defend Vulture" $ \case Defend (EnemyMinionId m) -> m == marked; _ -> False
        pass
        damageOf p `shouldReturn` 0
        resolve [IdentityMessage p ReadiedIdentity]
        runNow [MinionMessage unmarked $ MinionAttacks p]
        choose "defend Tiger Shark" $ \case Defend (EnemyMinionId m) -> m == unmarked; _ -> False
        pass
        damageOf p `shouldReturn` 1

    it "Dragonfang costs1, gives +1ATK normally and +2 instead against the marked enemy" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        _ <- playPaying p "25006" ["25024"]
        marked <- addMinion p Minions.titania
        unmarked <- addMinion p Minions.abomination
        changeForm p
        _ <- playGlow p marked
        heroAttackOf p `shouldReturn` 3
        basicAttack p
        choose "attack the marked minion" (isTargetChoice marked)
        pass
        minionDamageOf marked `shouldReturn` Just 4
        resolve [IdentityMessage p ReadiedIdentity]
        basicAttack p
        choose "attack the unmarked minion" (isTargetChoice unmarked)
        pass
        minionDamageOf unmarked `shouldReturn` Just 3

    it "both weapons count against the Restricted limit" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        spear <- playPaying p "25005" ["25024"]
        fang <- playPaying p "25006" ["25024"]
        Restricted `member` cdKeywords (getCardDef spear) `shouldBe` True
        Restricted `member` cdKeywords (getCardDef fang) `shouldBe` True
        third <- putInHand p "20010"
        money <- putInHand p "25024"
        playWith third [money]
        offers (isTargetChoice $ upgradeIdOf spear) `shouldReturn` True
        offers (isTargetChoice $ upgradeIdOf fang) `shouldReturn` True
        choose "discard the third restricted weapon" (isTargetChoice $ upgradeIdOf third)
        pass
        discardOf p >>= (`shouldSatisfy` elem third)

  describe "Aragorn" do
    it "costs2 and grants4 maximum HP and Aerial to both sides, reverting when discarded" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        setDamage p 2
        aragorn <- playPaying p "25007" ["25025"]
        hitPointsOf p `shouldReturn` 16
        damageOf p `shouldReturn` 2
        identityMatches (IdentityWithTrait Aerial) p `shouldReturn` True
        changeForm p
        hitPointsOf p `shouldReturn` 16
        identityMatches (IdentityWithTrait Aerial) p `shouldReturn` True
        resolve [RemoveFromPlay $ UpgradeRef $ upgradeIdOf aragorn]
        hitPointsOf p `shouldReturn` 12
        identityMatches (IdentityWithTrait Aerial) p `shouldReturn` False

  describe "Flight of the Valkyrior" do
    it "costs2, then discards to remove5 threat after another player defeats the marked enemy, including in alter-ego form" do
      tg <- testGame ["valkyrie", "spider-man"]
      let p = player1 tg
      inGame tg do
        flight <- playPaying p "25008" ["25025"]
        changeForm p
        mid <- addMinion p Minions.hydraMercenary
        glow <- playGlow p mid
        resolve [IdentityMessage p $ ChangedToForm B]
        placeMainThreat 6
        runNow [MinionMessage mid $ MinionDamaged (IdentityRef $ player2 tg) $ toDamage 3 $ FromPlayerAttack $ player2 tg]
        resolveGlow glow
        choose "Flight of the Valkyrior" (cardAbility flight 1)
        upgradesWith "25008" `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` elem flight)
        chooseMainScheme
        pass
        mainSchemeThreat `shouldReturn` 1

    it "does not discard or respond when an unmarked enemy is defeated" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        flight <- playPaying p "25008" ["25025"]
        changeForm p
        marked <- addMinion p Minions.hydraMercenary
        other <- prepareFragileMinion p
        _ <- playGlow p marked
        placeMainThreat 6
        killByBasicAttack p other
        offers (cardAbility flight 1) `shouldReturn` False
        pass
        length <$> upgradesWith "25008" `shouldReturn` 1

    it "is not offered when no scheme has threat to remove" do
      tg <- testGame ["valkyrie"]
      let p = player1 tg
      inGame tg do
        flight <- playPaying p "25008" ["25025"]
        changeForm p
        mid <- prepareFragileMinion p
        glow <- playGlow p mid
        mainSchemeThreat `shouldReturn` 0
        killByBasicAttack p mid
        resolveGlow glow
        offers (cardAbility flight 1) `shouldReturn` False
        pass
        length <$> upgradesWith "25008" `shouldReturn` 1

    for_ [True, False] $ \flightFirst ->
      it ("retains the marked defeat for both Valhalla and Flight after Death-Glow leaves; Flight first = " <> show flightFirst) do
        tg <- testGame ["valkyrie"]
        let p = player1 tg
        inGame tg do
          flight <- playPaying p "25008" ["25025"]
          valhalla <- playPaying p "25004" ["25025"]
          changeForm p
          setDamage p 1
          placeMainThreat 6
          mid <- prepareFragileMinion p
          glow <- playGlow p mid
          before <- length <$> handOf p
          killByBasicAttack p mid
          resolveGlow glow
          upgradesWith "25002" `shouldReturn` []
          offers (cardAbility flight 1) `shouldReturn` True
          offers (cardAbility valhalla 1) `shouldReturn` True
          if flightFirst
            then do
              choose "resolve Flight first" (cardAbility flight 1)
              chooseMainScheme
              choose "then Valhalla" (cardAbility valhalla 1)
            else do
              choose "resolve Valhalla first" (cardAbility valhalla 1)
              choose "then Flight" (cardAbility flight 1)
              chooseMainScheme
          pass
          mainSchemeThreat `shouldReturn` 1
          damageOf p `shouldReturn` 0
          length <$> handOf p `shouldReturn` before + 1

    it "allows both copies to answer the same marked defeat and each removes5 threat" do
      tg <- testGame ["valkyrie", "spider-man"]
      let p = player1 tg
      inGame tg do
        first <- playPaying p "25008" ["25025"]
        second <- playPaying p "25008" ["25025"]
        changeForm p
        placeMainThreat 12
        mid <- prepareFragileMinion p
        glow <- playGlow p mid
        killByBasicAttack p mid
        resolveGlow glow
        choose "first Flight" (cardAbility first 1)
        chooseMainScheme
        choose "second Flight" (cardAbility second 1)
        chooseMainScheme
        pass
        mainSchemeThreat `shouldReturn` 2
        upgradesWith "25008" `shouldReturn` []
