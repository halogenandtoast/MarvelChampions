module Marvel.Rules.AttachedControlSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Marvel.Ability (HasAbilities, passesCardCriteria)
import Marvel.Ally.Types qualified as Ally
import Marvel.Card.Def
import Marvel.Event.Cards (teamUp)
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Minion.Types qualified as Minion
import Marvel.Modifier
import Marvel.Query
import Marvel.Upgrade.Types (Upgrade (..), IsUpgrade)
import TestImport

-- An ordinary card with a restricted marker for generic bookkeeping regressions.
newtype TestRestricted = TestRestricted (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)
instance RunMessage TestRestricted where
  runMessage msg (TestRestricted attrs) = TestRestricted <$> runMessage msg attrs

markRestricted :: PlayerCard -> GameAppT ()
markRestricted card = withGame_ $ entitiesL . upgradesL . ix (upgradeIdOf card) %~ \u ->
  Upgrade $ TestRestricted ((toAttrs u) {upgradeCardDef = (getCardDef u) {cdKeywords = HashSet.insert Restricted (cdKeywords $ getCardDef u)}})

upgradeAt :: PlayerCard -> GameAppT (Attrs Upgrade)
upgradeAt card = getsGame (maybe (error "missing upgrade") toAttrs . HashMap.lookup (upgradeIdOf card) . gameUpgrades)

playerHasUpgrade :: IdentityId -> PlayerCard -> GameAppT Bool
playerHasUpgrade p card = HashSet.member (upgradeIdOf card) . playerIdentityUpgrades <$> playerAttrs p

putSuit :: IdentityId -> AllyId -> GameAppT PlayerCard
putSuit p ally = do
  suit <- putInHand p "12018"
  energy <- putInHand p "01088"
  playOn suit ally [energy]
  pure suit

spec :: Spec
spec = describe "attached upgrades and team-up forms" do
  it "team-up names identify hero forms, even when alter-ego has the same title" do
    tg <- testGame ["groot", "rocket-raccoon"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      runNow [IdentityMessage other $ ChangedToForm A]
      card <- putInHand p "16020"
      passesCardCriteria p card (teamUp "Groot" "Rocket Raccoon") `shouldReturn` False
      changeForm p
      passesCardCriteria p card (teamUp "Groot" "Rocket Raccoon") `shouldReturn` True
      runNow [IdentityMessage other $ ChangedToForm B]
      passesCardCriteria p card (teamUp "Groot" "Rocket Raccoon") `shouldReturn` False

  it "a named ally still supplies the second team-up character" do
    tg <- testGame ["groot"]
    let p = player1 tg
    inGame tg do
      changeForm p
      _ <- putIntoPlay p "16019"
      card <- putInHand p "16020"
      passesCardCriteria p card (teamUp "Groot" "Rocket Raccoon") `shouldReturn` True

  it "an upgrade played on another player's ally follows its controller but keeps its owner" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      ally <- putIntoPlay other "01051"
      suit <- putSuit p (allyIdOf ally)
      attrs <- upgradeAt suit
      liftIO $ (upgradeController attrs, upgradeOwner attrs) `shouldBe` (other, p)
      playerHasUpgrade p suit `shouldReturn` False
      playerHasUpgrade other suit `shouldReturn` True
      resolve [RemoveFromPlay $ UpgradeRef $ upgradeIdOf suit]
      discard <- discardOf p
      liftIO $ any ((== pcCardId suit) . pcCardId) discard `shouldBe` True
      liftIO $ all ((== Just p) . pcOwner) (filter ((== pcCardId suit) . pcCardId) discard) `shouldBe` True

  it "an ally's later control change also moves its upgrades, without discarding them" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      ally <- putIntoPlay p "01051"
      suit <- putSuit p (allyIdOf ally)
      resolve [AllyMessage (allyIdOf ally) $ AllyChangedController other]
      attrs <- upgradeAt suit
      liftIO $ (upgradeController attrs, upgradeOwner attrs, upgradeAttachedAlly attrs) `shouldBe` (other, p, Just $ allyIdOf ally)
      playerHasUpgrade p suit `shouldReturn` False
      playerHasUpgrade other suit `shouldReturn` True
      getsGame (fmap (Ally.allyController . toAttrs) . HashMap.lookup (allyIdOf ally) . gameAllies) `shouldReturn` Just other
      getsGame (fmap (HashSet.member (upgradeIdOf suit) . Ally.allyUpgrades . toAttrs) . HashMap.lookup (allyIdOf ally) . gameAllies) `shouldReturn` Just True

  it "preserves attached upgrades but disables ally-only hit-point modifiers while the host is a minion" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      ally <- putIntoPlay p "01051"
      suit <- putSuit p (allyIdOf ally)
      let mid = MinionId $ unAllyId $ allyIdOf ally
      resolve [AllyBecomesMinion $ allyIdOf ally]
      selectAny (UpgradeWithId $ upgradeIdOf suit) `shouldReturn` True
      minion <- getsGame (maybe (error "missing transformed ally") toAttrs . HashMap.lookup mid . gameMinions)
      Minion.getModifiedHitPoints minion `shouldReturn` 3
      liftIO $ HashSet.member (upgradeIdOf suit) (Minion.minionUpgrades minion) `shouldBe` True
      attrs <- upgradeAt suit
      liftIO $ (upgradeController attrs, upgradeOwner attrs) `shouldBe` (p, p)
      playerHasUpgrade p suit `shouldReturn` True

  it "returns attached upgrades and the saved controller when the minion becomes an ally again" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      ally <- putIntoPlay p "01051"
      suit <- putSuit p (allyIdOf ally)
      resolve [AllyMessage (allyIdOf ally) $ AllyChangedController other]
      let mid = MinionId $ unAllyId $ allyIdOf ally
      resolve [AllyBecomesMinion $ allyIdOf ally, MinionBecomesAlly mid]
      attrs <- upgradeAt suit
      liftIO $ (upgradeController attrs, upgradeOwner attrs, upgradeAttachedAlly attrs) `shouldBe` (other, p, Just $ allyIdOf ally)
      restored <- getsGame (maybe (error "missing restored ally") toAttrs . HashMap.lookup (allyIdOf ally) . gameAllies)
      Ally.getModifiedHitPoints restored `shouldReturn` 5
      liftIO $ Ally.allyController restored `shouldBe` other
      liftIO $ HashSet.member (upgradeIdOf suit) (Ally.allyUpgrades restored) `shouldBe` True

  it "does not briefly count an ally-attached restricted upgrade against the payer's limit" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      first <- putIntoPlay p "01007"
      second <- putIntoPlay p "01007"
      markRestricted first
      markRestricted second
      ally <- putIntoPlay other "01051"
      suit <- putInHand p "06019"
      runNow [PutCardIntoPlay p suit (TargetPayment $ AllyRef $ allyIdOf ally) Nothing True]
      attrs <- upgradeAt suit
      liftIO $ upgradeController attrs `shouldBe` other
      playerHasUpgrade p suit `shouldReturn` False
      selectList (UpgradeControlledBy (IdentityWithId p) <> UpgradeWithKeyword Restricted) >>= (`shouldSatisfy` ((== 2) . length))

  it "checks the new controller's restricted limit after the host changes control" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      first <- putIntoPlay other "01007"
      second <- putIntoPlay other "01007"
      markRestricted first
      markRestricted second
      ally <- putIntoPlay p "01051"
      suit <- putSuit p (allyIdOf ally)
      markRestricted suit
      runNow [AllyMessage (allyIdOf ally) $ AllyChangedController other]
      fmap fst <$> asked `shouldReturn` Just other
      offers (isTargetChoice $ upgradeIdOf suit) `shouldReturn` True
      choose "discard an existing restricted upgrade" (isTargetChoice $ upgradeIdOf first)
      attrs <- upgradeAt suit
      liftIO $ (upgradeController attrs, upgradeOwner attrs) `shouldBe` (other, p)
      playerHasUpgrade p suit `shouldReturn` False
      playerHasUpgrade other suit `shouldReturn` True
      getsGame (fmap (HashSet.member (upgradeIdOf suit) . Ally.allyUpgrades . toAttrs) . HashMap.lookup (allyIdOf ally) . gameAllies) `shouldReturn` Just True

  it "keeps ally-only weapons attached but disables their attack and keyword bonuses as a minion" do
    tg <- testGame ["nebula"]
    let p = player1 tg
    inGame tg do
      gamora <- putIntoPlay p "22002"
      spear <- putInHand p "22032"
      energy <- putInHand p "01088"
      playOn spear (allyIdOf gamora) [energy]
      sidearm <- putInHand p "23035"
      genius <- putInHand p "01089"
      playOn sidearm (allyIdOf gamora) [genius]
      let mid = MinionId $ unAllyId $ allyIdOf gamora
      resolve [AllyBecomesMinion $ allyIdOf gamora]
      transformed <- getsGame (maybe (error "missing minion") toAttrs . HashMap.lookup mid . gameMinions)
      Minion.getModifiedAttack transformed `shouldReturn` 2
      keywords <- Minion.getModifiedKeywords transformed
      liftIO $ keywords `shouldSatisfy` notElem Piercing
      liftIO $ keywords `shouldSatisfy` notElem Ranged
      selectAny (UpgradeWithId $ upgradeIdOf spear) `shouldReturn` True
      selectAny (UpgradeWithId $ upgradeIdOf sidearm) `shouldReturn` True
