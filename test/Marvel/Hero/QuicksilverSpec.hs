module Marvel.Hero.QuicksilverSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (PlayerIdentitySide (..), currentIdentity)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types qualified as Minion
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as V
import Marvel.Phase (Phase (..))
import TestImport

quicksilver :: IO TestGame
quicksilver = testGame ["quicksilver"]

-- | Quicksilver in hero form, his hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = do
  clearHand p
  changeForm p
  pass

-- | The hero's basic attack on the villain, its responses left open.
attackVillain :: IdentityId -> GameAppT ()
attackVillain p = do
  vid <- villainIdNow
  choose "attack" (isHeroAttack p)
  choose "attack the villain" (isTargetChoice vid)

-- | Rhino attacks the player, his boost card giving him no more ATK.
rhinoAttacks :: IdentityId -> GameAppT ()
rhinoAttacks p = do
  _ <- stackEncounterDeck [Treacheries.imTough]
  vid <- villainIdNow
  runNow [VillainMessage vid (VillainAttacks p)]

-- | End the turn and decline everything until the player's next turn.
toNextTurn :: (HasCallStack) => GameAppT ()
toNextTurn = do
  endTurn
  go (40 :: Int)
 where
  go 0 = error "the next turn did not come"
  go n = do
    pass
    keepCards
    whenM (not <$> offers (== EndTurn)) do
      asked >>= \case
        Just (ident, _) -> answerQuestion ident 0
        Nothing -> error "nothing asked"
      go (n - 1)

-- | THW, ATK and DEF as modified.
data Stats = Stats Natural Natural Natural
  deriving stock (Show, Eq)

heroStats :: IdentityId -> GameAppT Stats
heroStats p =
  currentIdentity <$> getPlayer p >>= \case
    HeroSide h ->
      Stats
        <$> Hero.getModifiedThwart (toAttrs h)
        <*> Hero.getModifiedAttack (toAttrs h)
        <*> Hero.getModifiedDefense (toAttrs h)
    AlterEgoSide _ -> pure (Stats 0 0 0)

isSuperSpeed :: Choice -> Bool
isSuperSpeed = isNamedAbility "Super Speed"

isSiblings :: Choice -> Bool
isSiblings = isNamedAbility "Superpowered Siblings"

isWitchDiscard :: Choice -> Bool
isWitchDiscard = isLabel "$label.scarletWitchDiscardTop"

villainStunned :: GameAppT Bool
villainStunned = V.villainStunned . toAttrs <$> getVillain

minionIsStunned :: MinionId -> GameAppT Bool
minionIsStunned m = getsGame (maybe False (Minion.minionStunned . toAttrs) . HashMap.lookup m . gameMinions)

spec :: Spec
spec = describe "Quicksilver" do
  describe "Super Speed" do
    it "readies him after his basic attack, once a phase" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        attackVillain p
        isExhaustedNow p `shouldReturn` True
        choose "Super Speed" isSuperSpeed
        isExhaustedNow p `shouldReturn` False
        villainDamageNow `shouldReturn` 1
        attackVillain p
        offers isSuperSpeed `shouldReturn` False
        pass
        isExhaustedNow p `shouldReturn` True
        villainDamageNow `shouldReturn` 2

    it "readies him after his basic thwart" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 3
        threat <- mainSchemeThreatNow
        choose "thwart" (isHeroThwart p)
        chooseMainScheme
        mainSchemeThreatNow `shouldReturn` (threat - 1)
        choose "Super Speed" isSuperSpeed
        isExhaustedNow p `shouldReturn` False

    it "may be used again the next phase" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        attackVillain p
        choose "Super Speed" isSuperSpeed
        runNow [BeginPhase VillainPhase]
        rhinoAttacks p
        choose "defend" isDefend
        choose "Super Speed, again" isSuperSpeed
        isExhaustedNow p `shouldReturn` False

    it "readies Friction Resistance as it readies him" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        friction <- putIntoPlay p "14009"
        resolve [UpgradeMessage (upgradeIdOf friction) ExhaustedUpgrade]
        attackVillain p
        choose "Super Speed" isSuperSpeed
        choose "Friction Resistance" (isAbilityOfCard friction)
        upgradeIsExhausted (upgradeIdOf friction) `shouldReturn` False

    it "readies him after he defends" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        rhinoAttacks p
        choose "defend" isDefend
        isExhaustedNow p `shouldReturn` True
        choose "Super Speed" isSuperSpeed
        isExhaustedNow p `shouldReturn` False

    it "is not used by a stunned attack" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        resolve [IdentityMessage p IdentityStunned]
        choose "attack" (isHeroAttack p)
        isStunned p `shouldReturn` False
        offers isSuperSpeed `shouldReturn` False
        isExhaustedNow p `shouldReturn` True
        villainDamageNow `shouldReturn` 0

    it "is not used by a confused thwart" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 3
        resolve [IdentityMessage p IdentityConfused]
        choose "thwart" (isHeroThwart p)
        isConfused p `shouldReturn` False
        offers isSuperSpeed `shouldReturn` False
        isExhaustedNow p `shouldReturn` True

  describe "Superpowered Siblings" do
    it "discards 2 cards to draw 2, once a round" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        clearHand p
        a <- putInHand p "01088"
        b <- putInHand p "01089"
        _ <- putInHand p "01090"
        choose "Superpowered Siblings" isSiblings
        choose "discard" (isTargetChoice a)
        choose "discard" (isTargetChoice b)
        pass
        length <$> handOf p `shouldReturn` 3
        discardOf p >>= (`shouldSatisfy` \d -> a `elem` d && b `elem` d)
        offers isSiblings `shouldReturn` False

    it "draws 3 instead with Wanda Maximoff in play" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "14002"
        a <- putInHand p "01088"
        b <- putInHand p "01089"
        _ <- putInHand p "01090"
        choose "Superpowered Siblings" isSiblings
        choose "discard" (isTargetChoice a)
        choose "discard" (isTargetChoice b)
        pass
        length <$> handOf p `shouldReturn` 4

    it "needs 2 cards to discard" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putInHand p "01088"
        offers isSiblings `shouldReturn` False

  describe "Scarlet Witch (14002)" do
    it "discards the top encounter card: +1 ATK for each boost icon, for this attack only" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "14002"
        witch <- allyIn "14002"
        _ <- stackEncounterDeck [Treacheries.explosion]
        vid <- villainIdNow
        choose "attack" (isAllyAttack witch)
        choose "discard the top card" isWitchDiscard
        choose "the villain" (isTargetChoice vid)
        pass
        villainDamageNow `shouldReturn` 3
        encounterDiscardCodes >>= (`shouldSatisfy` elem "01111")
        -- the next use gets nothing from the last
        resolve [AllyMessage (allyId witch) ReadiedAlly]
        _ <- stackEncounterDeck [Treacheries.imTough]
        choose "attack" (isAllyAttack witch)
        choose "discard the top card" isWitchDiscard
        choose "the villain" (isTargetChoice vid)
        pass
        villainDamageNow `shouldReturn` 4

    it "adds to her thwart; a star is not a boost icon" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "14002"
        witch <- allyIn "14002"
        placeMainThreat 5
        threat <- mainSchemeThreatNow
        _ <- stackEncounterDeck [Treacheries.stampede]
        choose "thwart" (isAllyThwart witch)
        choose "discard the top card" isWitchDiscard
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` (threat - 2)
        resolve [AllyMessage (allyId witch) ReadiedAlly]
        _ <- stackEncounterDeck [Treacheries.sonicBoom]
        choose "thwart" (isAllyThwart witch)
        choose "discard the top card" isWitchDiscard
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` (threat - 3)

    it "is optional" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "14002"
        witch <- allyIn "14002"
        [top] <- stackEncounterDeck [Treacheries.explosion]
        vid <- villainIdNow
        choose "attack" (isAllyAttack witch)
        choose "pass" (isLabel "$label.pass")
        choose "the villain" (isTargetChoice vid)
        pass
        villainDamageNow `shouldReturn` 1
        take 1 <$> scenarioEncounterDeckNow `shouldReturn` [top]

    it "is not used by a stunned attack" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "14002"
        witch <- allyIn "14002"
        resolve [GiveStatus (AllyRef $ allyId witch) StunnedStatus]
        choose "attack" (isAllyAttack witch)
        offers isWitchDiscard `shouldReturn` False
        (.allyStunned) <$> allyIn "14002" `shouldReturn` False
        villainDamageNow `shouldReturn` 0

  describe "Always Be Running (14003)" do
    it "readies Quicksilver" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        attackVillain p
        pass
        isExhaustedNow p `shouldReturn` True
        card <- putInHand p "14003"
        energy <- putInHand p "01088"
        playWith card [energy]
        pass
        isExhaustedNow p `shouldReturn` False

    it "cannot be played while he is ready" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        card <- putInHand p "14003"
        _ <- putInHand p "01088"
        offers (isPlay card) `shouldReturn` False

  describe "Double Time (14004)" do
    it "deals 2 damage to an enemy and removes 2 threat from a scheme" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 4
        threat <- mainSchemeThreatNow
        card <- putInHand p "14004"
        payment <- traverse (putInHand p) ["01088", "01089"]
        playWith card payment
        choose "deal 2 damage" (isLabel (countLabel "dealDamageToAnEnemy" 2))
        chooseVillain
        choose "remove 2 threat" (isLabel (countLabel "removeThreatFromAScheme" 2))
        chooseMainScheme
        pass
        villainDamageNow `shouldReturn` 2
        mainSchemeThreatNow `shouldReturn` (threat - 2)

    it "may choose the same option twice" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        card <- putInHand p "14004"
        payment <- traverse (putInHand p) ["01088", "01089"]
        playWith card payment
        choose "deal 2 damage" (isLabel (countLabel "dealDamageToAnEnemy" 2))
        chooseVillain
        choose "deal 2 damage again" (isLabel (countLabel "dealDamageToAnEnemy" 2))
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 4

  describe "Maximum Velocity (14005)" do
    it "gives +2 THW, ATK and DEF until the end of the round, max 1 a phase" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        card <- putInHand p "14005"
        payment <- traverse (putInHand p) ["01088", "01089"]
        playWith card payment
        pass
        heroStats p `shouldReturn` Stats 3 3 3
        second <- putInHand p "14005"
        _ <- traverse (putInHand p) ["01088", "01089"]
        offers (isPlay second) `shouldReturn` False
        toNextTurn
        heroStats p `shouldReturn` Stats 1 1 1
        offers (isPlay second) `shouldReturn` True

  describe "Speed Cyclone (14006)" do
    it "stuns X enemies, chosen" do
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        m1 <- addMinion p Minions.shocker
        m2 <- addMinion p Minions.hydraBomber
        pass
        -- Energy is 2 [energy] resources: X is 2, for 3 enemies
        card <- putInHand p "14006"
        energy <- putInHand p "01088"
        playWith card [energy]
        choose "stun a Shocker" (isTargetChoice m1)
        chooseVillain
        pass
        villainStunned `shouldReturn` True
        minionIsStunned m1 `shouldReturn` True
        minionIsStunned m2 `shouldReturn` False

    it "stuns every enemy when X is as many or more" do
      -- X is 2, for the villain alone
      tg <- quicksilver
      let p = player1 tg
      inGame tg do
        asHero p
        card <- putInHand p "14006"
        energy <- putInHand p "01088"
        playWith card [energy]
        pass
        villainStunned `shouldReturn` True
