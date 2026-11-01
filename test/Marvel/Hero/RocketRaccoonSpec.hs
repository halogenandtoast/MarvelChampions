module Marvel.Hero.RocketRaccoonSpec (spec) where

import Marvel.Hero.Runner (getModifiedAttack, getModifiedThwart)
import Marvel.Identity ( currentIdentity )
import Marvel.Matchers (IdentityMatcher (..))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query (selectAny)
import Marvel.Trait (Trait (Aerial))
import TestImport

rocketGame :: IO TestGame
rocketGame = testGame ["rocket-raccoon"]

-- | Rocket Raccoon in hero form, his hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

isMurderedYou, isTinkering :: Choice -> Bool
isMurderedYou = isNamedAbility "\"Murdered You!\""
isTinkering = isNamedAbility "Tinkering"

-- | Decline responses until a matching choice is offered.
declineUntil :: (HasCallStack) => (Choice -> Bool) -> GameAppT ()
declineUntil f = do
  o <- offers f
  unless o $ choose "decline" isNoResponse >> declineUntil f

-- | Decline every response, failing if a matching choice is ever offered.
neverOffered :: (HasCallStack) => (Choice -> Bool) -> GameAppT ()
neverOffered f = do
  offers f `shouldReturn` False
  whenM (offers isNoResponse) $ choose "decline" isNoResponse >> neverOffered f

heroAttack, heroThwart :: IdentityId -> GameAppT Natural
heroAttack p =
  getPlayer p >>= \i -> case currentIdentity i of
    HeroSide h -> getModifiedAttack (toAttrs h)
    AlterEgoSide _ -> error "Rocket Raccoon is in alter-ego form"
heroThwart p =
  getPlayer p >>= \i -> case currentIdentity i of
    HeroSide h -> getModifiedThwart (toAttrs h)
    AlterEgoSide _ -> error "Rocket Raccoon is in alter-ego form"

usesOf :: CardCode -> GameAppT [Natural]
usesOf code = map (.upgradeUses) <$> upgradesWith code

isAerial :: IdentityId -> GameAppT Bool
isAerial p = selectAny (IdentityWithId p <> IdentityWithTrait Aerial)

-- | Use an upgrade's ability on a target, when the target is asked for.
useOn :: (IsRef a) => PlayerCard -> a -> GameAppT ()
useOn card target = do
  choose "use the upgrade" (isAbilityOfCard card)
  chooseIf (isTargetChoice target)

codesOf :: [PlayerCard] -> [CardCode]
codesOf = map (toCardCode . getCardDef)

spec :: Spec
spec = describe "Rocket Raccoon" do
  it "deals the pack's own deck of 40 cards" do
    sum (map snd (starter "rocket-raccoon").cards) `shouldBe` (40 :: Int) :: IO ()

  describe "Rocket Raccoon (16029a)" do
    it "Murdered You!: draws 1 after his attack defeats a minion with excess damage" do
      tg <- rocketGame
      let p = player1 tg
      inGame tg do
        asHero p
        cannon <- putIntoPlay p "16036"
        mid <- addMinion p Minions.hydraMercenary
        useOn cannon mid
        declineUntil isMurderedYou
        choose "Murdered You!" isMurderedYou
        minionDamageOf mid `shouldReturn` Nothing
        length <$> handOf p `shouldReturn` 1

    it "Murdered You!: answers damage that is not an attack (Rocket Launcher)" do
      tg <- rocketGame
      let p = player1 tg
      inGame tg do
        asHero p
        pistol <- putIntoPlay p "16038"
        launcher <- putIntoPlay p "16037"
        mid <- addMinion p Minions.hydraMercenary
        useOn pistol mid
        neverOffered isMurderedYou
        minionDamageOf mid `shouldReturn` Just 2
        useOn launcher p
        declineUntil isMurderedYou
        choose "Murdered You!" isMurderedYou
        minionDamageOf mid `shouldReturn` Nothing
        length <$> handOf p `shouldReturn` 1

    it "Murdered You!: not when the damage exactly defeats the minion" do
      tg <- rocketGame
      let p = player1 tg
      inGame tg do
        asHero p
        pistol <- putIntoPlay p "16038"
        mid <- addMinion p Minions.hydraMercenary
        useOn pistol mid
        neverOffered isMurderedYou
        choose "attack" (isHeroAttack p)
        chooseIf (isTargetChoice mid)
        neverOffered isMurderedYou
        minionDamageOf mid `shouldReturn` Nothing
        length <$> handOf p `shouldReturn` 0

    it "Murdered You!: not for an ally's excess damage" do
      tg <- rocketGame
      let p = player1 tg
      inGame tg do
        asHero p
        pistol <- putIntoPlay p "16038"
        _ <- putIntoPlay p "04100"
        mid <- addMinion p Minions.hydraMercenary
        useOn pistol mid
        neverOffered isMurderedYou
        elektra <- allyIn "04100"
        choose "Elektra attacks" (isAllyAttack elektra)
        chooseIf (isTargetChoice mid)
        neverOffered isMurderedYou
        minionDamageOf mid `shouldReturn` Nothing
        length <$> handOf p `shouldReturn` 0

  describe "Rocket Raccoon (16029b)" do
    it "Tinkering: discards a Tech upgrade to draw 2, once a round" do
      tg <- rocketGame
      let p = player1 tg
      inGame tg do
        clearHand p
        pistol <- putIntoPlay p "16038"
        _ <- putIntoPlay p "16039"
        choose "Tinkering" isTinkering
        choose "discard Rocket's Pistol" (isTargetChoice (upgradeOf pistol))
        length <$> handOf p `shouldReturn` 2
        usesOf "16038" `shouldReturn` []
        codesOf <$> discardOf p >>= (`shouldSatisfy` elem "16038")
        offers isTinkering `shouldReturn` False

    it "Tinkering: needs a Tech upgrade, and is not used in hero form" do
      tg <- rocketGame
      let p = player1 tg
      inGame tg do
        offers isTinkering `shouldReturn` False
        _ <- putIntoPlay p "16038"
        offers isTinkering `shouldReturn` True
        asHero p
        offers isTinkering `shouldReturn` False

  describe "Battery Pack (16034)" do
    it "moves a charge counter to another Tech upgrade you control" do
      tg <- rocketGame
      let p = player1 tg
      inGame tg do
        battery <- putIntoPlay p "16034"
        offers (isAbilityOfCard battery) `shouldReturn` False
        pistol <- putIntoPlay p "16038"
        useOn battery (upgradeOf pistol)
        usesOf "16034" `shouldReturn` [1]
        usesOf "16038" `shouldReturn` [4]
        upgradeIsExhausted (upgradeOf battery) `shouldReturn` True

    it "stays in play with no counters, and cannot move one then" do
      tg <- rocketGame
      let p = player1 tg
      inGame tg do
        battery <- putIntoPlay p "16034"
        _ <- putIntoPlay p "16038"
        resolve [UpgradeMessage (upgradeOf battery) SpendUpgradeUse, UpgradeMessage (upgradeOf battery) SpendUpgradeUse]
        usesOf "16034" `shouldReturn` [0]
        offers (isAbilityOfCard battery) `shouldReturn` False

  describe "Cybernetic Skeleton (16035)" do
    it "gives +3 hit points, and +1 ATK in hero form" do
      tg <- rocketGame
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "16035"
        hitPointsOf p `shouldReturn` 12
        asHero p
        hitPointsOf p `shouldReturn` 12
        heroAttack p `shouldReturn` 2

  describe "Particle Cannon (16036)" do
    it "deals 4 damage with overkill, removing a charge counter" do
      tg <- rocketGame
      let p = player1 tg
      inGame tg do
        asHero p
        cannon <- putIntoPlay p "16036"
        mid <- addMinion p Minions.hydraMercenary
        damage <- villainDamageNow
        useOn cannon mid
        declineUntil isMurderedYou
        choose "Murdered You!" isMurderedYou
        villainDamageNow `shouldReturn` (damage + 1)
        usesOf "16036" `shouldReturn` [1]
        upgradeIsExhausted (upgradeOf cannon) `shouldReturn` True

  describe "Rocket Launcher (16037)" do
    it "deals 2 to the villain and each minion engaged with the chosen player" do
      tg <- rocketGame
      let p = player1 tg
      inGame tg do
        asHero p
        launcher <- putIntoPlay p "16037"
        mid <- addMinion p Minions.hydraMercenary
        damage <- villainDamageNow
        useOn launcher p
        villainDamageNow `shouldReturn` (damage + 2)
        minionDamageOf mid `shouldReturn` Just 2
        upgradeIsExhausted (upgradeOf launcher) `shouldReturn` True

    it "stays in play once its last counter is removed, and cannot be used then" do
      tg <- rocketGame
      let p = player1 tg
      inGame tg do
        asHero p
        launcher <- putIntoPlay p "16037"
        useOn launcher p
        runNow [UpgradeMessage (upgradeOf launcher) ReadiedUpgrade]
        useOn launcher p
        runNow [UpgradeMessage (upgradeOf launcher) ReadiedUpgrade]
        usesOf "16037" `shouldReturn` [0]
        offers (isAbilityOfCard launcher) `shouldReturn` False

  describe "Rocket's Pistol (16038)" do
    it "deals 2 damage to an enemy, removing a charge counter" do
      tg <- rocketGame
      let p = player1 tg
      inGame tg do
        asHero p
        pistol <- putIntoPlay p "16038"
        damage <- villainDamageNow
        vid <- villainIdNow
        useOn pistol vid
        villainDamageNow `shouldReturn` (damage + 2)
        usesOf "16038" `shouldReturn` [2]
        upgradeIsExhausted (upgradeOf pistol) `shouldReturn` True

  describe "Thruster Boots (16039)" do
    it "gives +1 THW and Aerial in hero form only" do
      tg <- rocketGame
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "16039"
        isAerial p `shouldReturn` False
        asHero p
        isAerial p `shouldReturn` True
        heroThwart p `shouldReturn` 3
