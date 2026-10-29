module Marvel.Hero.NebulaSignatureSpec (spec) where

import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Identity.Types (PlayerIdentity (..))
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import TestImport

newtype CostModifiers = CostModifiers (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON)
instance RunMessage CostModifiers where
  runMessage msg (CostModifiers attrs) = CostModifiers <$> runMessage msg attrs

costModifiers :: IdentityId -> [Modifier] -> GameAppT ()
costModifiers p mods = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (CostModifiers EffectAttrs
    { effectId = eid, effectCardCode = "01039", effectSource = GameRef
    , effectMatcher = IdentityEntity (IdentityWithId p), effectModifiers = mods, effectEnds = Nothing })

deckOf :: IdentityId -> GameAppT [PlayerCard]
deckOf p = unDeck . playerIdentityDeck <$> playerAttrs p

playCombatReady :: IdentityId -> GameAppT PlayerCard
playCombatReady p = do
  card <- putInHand p "22009"
  playWith card []
  pure card

playIntent :: IdentityId -> Natural -> [CardCode] -> GameAppT PlayerCard
playIntent p n resourceCodes = do
  card <- putInHand p "22010"
  payment <- traverse (putInHand p) resourceCodes
  choose "play Lethal Intent" (isPlay card)
  choose "declare X" (isLabel $ countLabel "chooseXAmount" n)
  payWith payment
  pure card

inHero :: IdentityId -> GameAppT ()
inHero p = changeForm p >> pass >> clearHand p

hasUpgrade :: IdentityId -> PlayerCard -> GameAppT Bool
hasUpgrade p card = selectAny (UpgradeWithId (upgradeIdOf card) <> UpgradeControlledBy (IdentityWithId p))

spec :: Spec
spec = describe "Nebula signature events" do
  describe "Combat Ready" do
    it "shuffles two physical copies of the same Technique into the deck" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        clearHand p
        [a, b, other, notTechnique] <- stackDiscard p ["22004", "22004", "22005", "01065"]
        before <- length <$> deckOf p
        _ <- playCombatReady p
        choose "shuffle Techniques" (isLabel "$label.combatReadyShuffle")
        offers (isTargetChoice notTechnique) `shouldReturn` False
        choose "first physical copy" (isTargetChoice a)
        offers (isTargetChoice a) `shouldReturn` False
        offers (isTargetChoice b) `shouldReturn` True
        choose "second physical copy with the same title" (isTargetChoice b)
        deck <- deckOf p
        discard <- discardOf p
        liftIO $ length deck `shouldBe` before + 2
        liftIO $ all (`elem` deck) [a, b] `shouldBe` True
        liftIO $ all (`elem` discard) [other, notTechnique] `shouldBe` True
        liftIO $ all (`notElem` discard) [a, b] `shouldBe` True

    it "allows exactly one shuffled Technique and leaves the other in discard" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        clearHand p
        [a, b] <- stackDiscard p ["22004", "22005"]
        _ <- playCombatReady p
        choose "shuffle" (isLabel "$label.combatReadyShuffle")
        choose "one Technique" (isTargetChoice a)
        choose "done with one" (isLabel "$label.done")
        deck <- deckOf p
        discard <- discardOf p
        liftIO $ a `elem` deck `shouldBe` True
        liftIO $ b `elem` discard `shouldBe` True

    it "allows zero cards without changing deck order" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- stackDiscard p ["22004"]
        before <- deckOf p
        _ <- playCombatReady p
        choose "shuffle" (isLabel "$label.combatReadyShuffle")
        choose "choose none" (isLabel "$label.done")
        deckOf p `shouldReturn` before

    it "does not let you shuffle another player's Technique" do
      tg <- testGame ["nebula", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        [ours] <- stackDiscard p ["22004"]
        [theirs] <- stackDiscard other ["22005"]
        _ <- playCombatReady p
        choose "shuffle" (isLabel "$label.combatReadyShuffle")
        offers (isTargetChoice theirs) `shouldReturn` False
        choose "our Technique" (isTargetChoice ours)
        whenM (offers $ isLabel "$label.done") $ choose "done" (isLabel "$label.done")
        discard <- discardOf other
        liftIO $ theirs `elem` discard `shouldBe` True

    it "discards until the first Technique, puts it into play, and resolves its Special without the AE draw" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        clearHand p
        [energy, intuition, technique, after] <- stackDeck p ["01088", "01065", "22006", "01090"]
        _ <- playCombatReady p
        choose "discard to a Technique" (isLabel "$label.combatReadyDiscard")
        offers (isNamedAbility "$label.cyberneticUpgrades") `shouldReturn` False
        length <$> handOf p `shouldReturn` 0
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` True
        hasUpgrade p technique `shouldReturn` True
        deck <- deckOf p
        liftIO $ head deck `shouldBe` after
        discard <- discardOf p
        liftIO $ all (`elem` discard) [energy, intuition] `shouldBe` True
        liftIO $ technique `elem` discard `shouldBe` False

    it "puts an already-tough Technique into play even when its Special cannot change the game" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        clearHand p
        resolve [GiveStatus (IdentityRef p) ToughStatus]
        [technique] <- stackDeck p ["22006"]
        _ <- playCombatReady p
        choose "discard" (isLabel "$label.combatReadyDiscard")
        hasUpgrade p technique `shouldReturn` True
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` True

    it "stops at the first Technique and leaves the next one on top" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        clearHand p
        [first, second] <- stackDeck p ["22006", "22005"]
        _ <- playCombatReady p
        choose "discard" (isLabel "$label.combatReadyDiscard")
        hasUpgrade p first `shouldReturn` True
        hasUpgrade p second `shouldReturn` False
        deck <- deckOf p
        liftIO $ head deck `shouldBe` second

    it "can discard a deck with no Technique, resetting it and dealing an encounter card" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        clearHand p
        cards <- stackDeck p ["01088", "01089"]
        withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) -> PlayerIdentity a {playerIdentityDeck = Deck cards, playerIdentityDiscard = Discard []}
        before <- length <$> dealtTo p
        _ <- playCombatReady p
        choose "discard" (isLabel "$label.combatReadyDiscard")
        length <$> dealtTo p `shouldReturn` before + 1
        upgradesWith "22006" `shouldReturn` []

    it "is unavailable in hero form" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        card <- putInHand p "22009"
        offers (isPlay card) `shouldReturn` False

  describe "Lethal Intent" do
    it "chooses up to X distinct Techniques before resolving them in a separate chosen order" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        persistence <- putIntoPlay p "22006"
        weapons <- putIntoPlay p "22007"
        _ <- playIntent p 2 ["01088"]
        choose "select Persistence first" (isTargetChoice $ upgradeIdOf persistence)
        offers (isTargetChoice $ upgradeIdOf persistence) `shouldReturn` False
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` False
        choose "select Weapons Master second" (isTargetChoice $ upgradeIdOf weapons)
        villainDamageNow `shouldReturn` 0
        choose "resolve Weapons Master first" (isTargetChoice $ upgradeIdOf weapons)
        chooseVillain
        villainDamageNow `shouldReturn` 4
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` False
        choose "resolve Persistence last" (isTargetChoice $ upgradeIdOf persistence)
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` True
        hasUpgrade p persistence `shouldReturn` True
        hasUpgrade p weapons `shouldReturn` True

    it "may choose fewer than X Techniques" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        weapons <- putIntoPlay p "22007"
        persistence <- putIntoPlay p "22006"
        _ <- playIntent p 2 ["01088"]
        choose "select one" (isTargetChoice $ upgradeIdOf weapons)
        choose "done selecting" (isLabel "$label.done")
        choose "resolve that one" (isTargetChoice $ upgradeIdOf weapons)
        chooseVillain
        villainDamageNow `shouldReturn` 4
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` False
        hasUpgrade p persistence `shouldReturn` True

    it "may choose zero Techniques after paying" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        weapons <- putIntoPlay p "22007"
        _ <- playIntent p 1 ["01088"]
        choose "choose none" (isLabel "$label.done")
        villainDamageNow `shouldReturn` 0
        hasUpgrade p weapons `shouldReturn` True

    it "does not turn overgenerated resources into extra X" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        first <- putIntoPlay p "22007"
        second <- putIntoPlay p "22007"
        _ <- playIntent p 1 ["01088"]
        choose "select the first" (isTargetChoice $ upgradeIdOf first)
        offers (isTargetChoice $ upgradeIdOf second) `shouldReturn` False
        choose "resolve the first" (isTargetChoice $ upgradeIdOf first)
        chooseVillain
        villainDamageNow `shouldReturn` 4
        hasUpgrade p first `shouldReturn` True
        hasUpgrade p second `shouldReturn` True

    it "does not permit a meaningless declared X of zero" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        _ <- putIntoPlay p "22007"
        clearHand p
        card <- putInHand p "22010"
        offers (isPlay card) `shouldReturn` False
        _ <- putInHand p "01088"
        choose "play" (isPlay card)
        offers (isLabel $ countLabel "chooseXAmount" 0) `shouldReturn` False
        offers (isLabel $ countLabel "chooseXAmount" 1) `shouldReturn` True

    it "retains X=1 when a discount reduces its payable cost to zero" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        weapons <- putIntoPlay p "22007"
        clearHand p
        costModifiers p [ResourceCostReduction 1]
        _ <- playIntent p 1 []
        choose "select Weapons Master" (isTargetChoice $ upgradeIdOf weapons)
        choose "resolve Weapons Master" (isTargetChoice $ upgradeIdOf weapons)
        chooseVillain
        villainDamageNow `shouldReturn` 4

    it "preserves declared X after a cost reduction instead of using the smaller payment" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        first <- putIntoPlay p "22007"
        second <- putIntoPlay p "22007"
        third <- putIntoPlay p "22006"
        clearHand p
        costModifiers p [ResourceCostReduction 1]
        _ <- playIntent p 3 ["01088"]
        choose "select first" (isTargetChoice $ upgradeIdOf first)
        choose "select second" (isTargetChoice $ upgradeIdOf second)
        choose "select third" (isTargetChoice $ upgradeIdOf third)
        choose "resolve first" (isTargetChoice $ upgradeIdOf first)
        chooseVillain
        choose "resolve second" (isTargetChoice $ upgradeIdOf second)
        chooseVillain
        choose "resolve third" (isTargetChoice $ upgradeIdOf third)
        villainDamageNow `shouldReturn` 8
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` True

    it "resolves only your Techniques" do
      tg <- testGame ["nebula", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        inHero p
        ours <- putIntoPlay p "22007"
        theirs <- putIntoPlay other "22006"
        _ <- playIntent p 1 ["01088"]
        offers (isTargetChoice $ upgradeIdOf theirs) `shouldReturn` False
        choose "select ours" (isTargetChoice $ upgradeIdOf ours)
        choose "resolve ours" (isTargetChoice $ upgradeIdOf ours)
        chooseVillain
        (.playerIdentityTough) <$> playerAttrs other `shouldReturn` False

    it "lets stun cancel one Special without canceling the other selected Special" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        first <- putIntoPlay p "22007"
        second <- putIntoPlay p "22007"
        resolve [GiveStatus (IdentityRef p) StunnedStatus]
        _ <- playIntent p 2 ["01088"]
        choose "select first" (isTargetChoice $ upgradeIdOf first)
        choose "select second" (isTargetChoice $ upgradeIdOf second)
        choose "resolve the stunned Special" (isTargetChoice $ upgradeIdOf first)
        isStunned p `shouldReturn` False
        villainDamageNow `shouldReturn` 0
        choose "resolve the remaining Special" (isTargetChoice $ upgradeIdOf second)
        chooseVillain
        villainDamageNow `shouldReturn` 4
        hasUpgrade p first `shouldReturn` True
        hasUpgrade p second `shouldReturn` True

    it "lets confusion cancel its thwart Special without treating the whole event as a thwart" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        cutthroat <- putIntoPlay p "22004"
        placeMainThreat 3
        before <- mainSchemeThreatNow
        resolve [GiveStatus (IdentityRef p) ConfusedStatus]
        _ <- playIntent p 1 ["01088"]
        choose "select Cutthroat" (isTargetChoice $ upgradeIdOf cutthroat)
        choose "resolve its Special" (isTargetChoice $ upgradeIdOf cutthroat)
        isConfused p `shouldReturn` False
        mainSchemeThreatNow `shouldReturn` before
        hasUpgrade p cutthroat `shouldReturn` True

    it "chooses a meaningful X when the event's resource cost is ignored" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        first <- putIntoPlay p "22007"
        second <- putIntoPlay p "22007"
        clearHand p
        costModifiers p [IgnoresResourceCost]
        _ <- playIntent p 2 []
        choose "select first" (isTargetChoice $ upgradeIdOf first)
        choose "select second" (isTargetChoice $ upgradeIdOf second)
        choose "resolve second" (isTargetChoice $ upgradeIdOf second)
        chooseVillain
        choose "resolve first" (isTargetChoice $ upgradeIdOf first)
        chooseVillain
        villainDamageNow `shouldReturn` 8

    it "is unavailable when every controlled Special is inert" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        inHero p
        _ <- putIntoPlay p "22006"
        resolve [GiveStatus (IdentityRef p) ToughStatus]
        card <- putInHand p "22010"
        _ <- putInHand p "01088"
        offers (isPlay card) `shouldReturn` False

    it "is unavailable in alter-ego form" do
      tg <- testGame ["nebula"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "22007"
        clearHand p
        card <- putInHand p "22010"
        _ <- putInHand p "01088"
        offers (isPlay card) `shouldReturn` False
