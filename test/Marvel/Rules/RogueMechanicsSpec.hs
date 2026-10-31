module Marvel.Rules.RogueMechanicsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

perform :: L.QueueT GameAppT () -> GameAppT ()
perform action = L.runQueueT (L.capture action) >>= resolve

spec :: Spec
spec = describe "Rogue general mechanics" do
  it "finds Touched in hand, moves it between enemy and friendly hosts, and retains its owner and controller" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
        q = player2 tg
    inGame tg do
      touched <- putInHand p "38002"
      mid <- addMinion p Minions.hydraBomber
      perform $ L.findAndAttachTouched p (MinionCharacter mid)
      touchedHost p `shouldReturn` Just (MinionCharacter mid)
      handOf p >>= (`shouldSatisfy` all ((/= pcCardId touched) . pcCardId))
      perform $ L.findAndAttachTouched p (IdentityCharacter q)
      touchedHost p `shouldReturn` Just (IdentityCharacter q)
      attrs <- getsGame (toAttrs . fromMaybe (error "Touched missing") . HashMap.lookup (upgradeIdOf touched) . gameUpgrades)
      upgradeController attrs `shouldBe` p
      upgradeOwner attrs `shouldBe` p
      upgradeAttachedEnemy attrs `shouldBe` Nothing
      upgrades <- getsGame (minionUpgrades . toAttrs . fromMaybe (error "host missing") . HashMap.lookup mid . gameMinions)
      upgrades `shouldSatisfy` notElem (upgradeIdOf touched)

  it "finds a discarded Touched, and setting an attached Touched aside removes the old host registration" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      touched <- putInHand p "38002"
      resolve [DiscardedCard $ PlayerCard touched]
      vid <- villainIdNow
      perform $ L.findAndAttachTouched p (VillainCharacter vid)
      touchedHost p `shouldReturn` Just (VillainCharacter vid)
      perform $ L.setAsideTouched p
      touchedHost p `shouldReturn` Nothing
      aside <- getSetAsidePlayerCards
      length (filter ((== pcCardId touched) . pcCardId) aside) `shouldBe` 1
      perform $ L.setAsideTouched p
      asideAgain <- getSetAsidePlayerCards
      length (filter ((== pcCardId touched) . pcCardId) asideAgain) `shouldBe` 1
      perform $ L.findAndAttachTouched p (VillainCharacter vid)
      touchedHost p `shouldReturn` Just (VillainCharacter vid)

  it "does not attach Touched to its own identity" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      touched <- putInHand p "38002"
      perform $ L.findAndAttachTouched p (IdentityCharacter p)
      touchedHost p `shouldReturn` Nothing
      handOf p >>= (`shouldSatisfy` elem touched)

  it "gives Karma's converted minion two consequential damage without changing Mind Control's default" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      first <- addMinion p Minions.hydraBomber
      perform $ L.takeControlOfMinionWithConsequential p first 2
      let aid = AllyId $ unMinionId first
      controlled <- getsGame (toAttrs . fromMaybe (error "controlled ally missing") . HashMap.lookup aid . gameAllies)
      allyAttackConsequentialDamage controlled `shouldBe` 2
      allyThwartConsequentialDamage controlled `shouldBe` 2
      second <- addMinion p Minions.armoredGuard
      perform $ L.takeControlOfMinion p second
      ordinary <- getsGame (toAttrs . fromMaybe (error "ordinary controlled ally missing") . HashMap.lookup (AllyId $ unMinionId second) . gameAllies)
      allyAttackConsequentialDamage ordinary `shouldBe` 1
      allyThwartConsequentialDamage ordinary `shouldBe` 1

  for_ [("drawn", True), ("added to hand", False)] $ \(description, drawn) ->
    it ("resolves Misled's forced response when " <> description <> " and preserves its physical encounter card") do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        encounter <- encounterCard Treacheries.misled
        let card = encounterCardInPlayerDeck p encounter
        if drawn
          then withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity attrs) ->
            PlayerIdentity attrs {playerIdentityDeck = Deck $ card : unDeck (playerIdentityDeck attrs)}
          else pure ()
        before <- mainSchemeThreatNow
        resolve [IdentityMessage p $ if drawn then DrawCards FromDeck 1 else AddToHand card]
        mainSchemeThreatNow `shouldReturn` before + 2
        handOf p >>= (`shouldSatisfy` elem card)
        resolve [DiscardedCard $ PlayerCard card]
        encounterDiscardNow >>= (`shouldSatisfy` any ((== ecCardId encounter) . ecCardId))
