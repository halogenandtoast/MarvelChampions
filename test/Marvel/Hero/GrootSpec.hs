module Marvel.Hero.GrootSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Game.Public (publicGame)
import Marvel.Minion.Cards qualified as Minions
import TestImport

grootGame :: IO TestGame
grootGame = testGame ["groot"]

-- | Groot in hero form, his hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

countersOf :: IdentityId -> GameAppT Natural
countersOf p = (.playerIdentityCounters) <$> playerAttrs p

setCounters :: IdentityId -> Natural -> GameAppT ()
setCounters p n = withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
  PlayerIdentity a {playerIdentityCounters = n}

-- | The counters the client is shown on each player's identity.
shownCounters :: GameAppT [Natural]
shownCounters = do
  view <- liftIO . publicGame =<< getGame
  pure
    [ floor c
    | Object o <- [view]
    , Just (Array ps) <- [KeyMap.lookup "players" o]
    , Object pl <- toList ps
    , Just (Number c) <- [KeyMap.lookup "counters" pl]
    ]

isFloraColossus :: Choice -> Bool
isFloraColossus = isNamedAbility "Flora Colossus"

-- | Damage from a treachery, not an attack.
treacheryDamage :: IdentityId -> Natural -> GameAppT ()
treacheryDamage p n = start (damageIdentity p (toSource p) (toDamage n FromTreachery))

-- | Rhino attacks the player: 2 ATK and a Vulture's 2 boost icons.
villainAttacks :: IdentityId -> GameAppT ()
villainAttacks p = do
  _ <- stackEncounterDeck [Minions.vulture]
  vid <- villainIdNow
  start [VillainMessage vid (VillainAttacks p)]

pickVillain :: GameAppT ()
pickVillain = chooseIf . isTargetChoice =<< villainIdNow

pickMainScheme :: GameAppT ()
pickMainScheme = chooseIf . isTargetChoice =<< mainSchemeIdNow

spec :: Spec
spec = describe "Groot" do
  describe "growth counters" do
    it "stay on him when he changes form, and are shown on the board" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        setCounters p 4
        shownCounters `shouldReturn` [4]
        changeForm p
        pass
        countersOf p `shouldReturn` 4
        shownCounters `shouldReturn` [4]

  describe "Groot (16001b): Growth Spurt" do
    it "places 2 growth counters, once a round" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        choose "Growth Spurt" (isNamedAbility "Growth Spurt")
        countersOf p `shouldReturn` 2
        offers (isNamedAbility "Growth Spurt") `shouldReturn` False

    it "places them only to a maximum of 10" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        setCounters p 9
        choose "Growth Spurt" (isNamedAbility "Growth Spurt")
        countersOf p `shouldReturn` 10

  describe "Groot (16001a): Flora Colossus" do
    it "removes as many counters as it can, preventing that much: 3 against 5 damage" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        setCounters p 3
        treacheryDamage p 5
        choose "Flora Colossus" isFloraColossus
        resolve []
        countersOf p `shouldReturn` 0
        damageOf p `shouldReturn` 2

    it "removes only as many counters as the damage" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        setCounters p 5
        treacheryDamage p 2
        choose "Flora Colossus" isFloraColossus
        resolve []
        countersOf p `shouldReturn` 3
        damageOf p `shouldReturn` 0

    it "removes no counter when a tough status card prevents the damage" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        setCounters p 3
        resolve [IdentityMessage p IdentityBecomeTough]
        treacheryDamage p 2
        chooseIf isFloraColossus
        resolve []
        countersOf p `shouldReturn` 3
        damageOf p `shouldReturn` 0
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` False

    it "does not trigger without counters" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        treacheryDamage p 2
        offers isFloraColossus `shouldReturn` False
        resolve []
        damageOf p `shouldReturn` 2

    it "is not used in alter-ego form" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        clearHand p
        setCounters p 3
        treacheryDamage p 2
        offers isFloraColossus `shouldReturn` False
        resolve []
        countersOf p `shouldReturn` 3
        damageOf p `shouldReturn` 2

    it "answers an enemy's attack" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        setCounters p 3
        villainAttacks p
        choose "no defender" isNoDefender
        choose "Flora Colossus" isFloraColossus
        resolve []
        countersOf p `shouldReturn` 0
        damageOf p `shouldReturn` 1

  describe "Fertile Ground (16007)" do
    it "exhausts to place 1 growth counter and draw 1 card" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        clearHand p
        ground <- putIntoPlay p "16007"
        choose "Fertile Ground" (isAbilityOfCard ground)
        countersOf p `shouldReturn` 1
        length <$> handOf p `shouldReturn` 1
        map (.supportExhausted) <$> supportsWith "16007" `shouldReturn` [True]

    it "draws even when Groot already has 10 counters" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        clearHand p
        setCounters p 10
        ground <- putIntoPlay p "16007"
        choose "Fertile Ground" (isAbilityOfCard ground)
        countersOf p `shouldReturn` 10
        length <$> handOf p `shouldReturn` 1

    it "is an alter-ego action" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        ground <- putIntoPlay p "16007"
        offers (isAbilityOfCard ground) `shouldReturn` False

  describe "Entangling Vines (16008)" do
    it "removes a counter and exhausts for +2 THW on the basic thwart" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 5
        setCounters p 1
        vines <- putIntoPlay p "16008"
        before <- mainSchemeThreatNow
        choose "thwart" (isHeroThwart p)
        choose "Entangling Vines" (isAbilityOfCard vines)
        pickMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` before - 3
        countersOf p `shouldReturn` 0
        upgradeIsExhausted (upgradeIdOf vines) `shouldReturn` True

    it "needs a growth counter" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 3
        vines <- putIntoPlay p "16008"
        choose "thwart" (isHeroThwart p)
        offers (isAbilityOfCard vines) `shouldReturn` False

  describe "Vine Spikes (16011)" do
    it "removes a counter and exhausts for +2 ATK on the basic attack" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        setCounters p 2
        spikes <- putIntoPlay p "16011"
        choose "attack" (isHeroAttack p)
        choose "Vine Spikes" (isAbilityOfCard spikes)
        pickVillain
        pass
        villainDamageNow `shouldReturn` 4
        countersOf p `shouldReturn` 1
        upgradeIsExhausted (upgradeIdOf spikes) `shouldReturn` True

    it "needs a growth counter" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        spikes <- putIntoPlay p "16011"
        choose "attack" (isHeroAttack p)
        offers (isAbilityOfCard spikes) `shouldReturn` False

  describe "Vine Shield (16010)" do
    it "removes a counter and exhausts for +3 DEF against the attack" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        setCounters p 1
        shield <- putIntoPlay p "16010"
        -- 4 damage against 3 DEF and 3 more
        villainAttacks p
        choose "defend" isDefend
        choose "Vine Shield" (isAbilityOfCard shield)
        resolve []
        damageOf p `shouldReturn` 0
        countersOf p `shouldReturn` 0
        upgradeIsExhausted (upgradeIdOf shield) `shouldReturn` True

    it "needs a growth counter" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        shield <- putIntoPlay p "16010"
        villainAttacks p
        choose "defend" isDefend
        offers (isAbilityOfCard shield) `shouldReturn` False

  describe "Lashing Vines (16009)" do
    it "removes 2 counters and exhausts to ready Groot after his basic attack" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        setCounters p 2
        lashing <- putIntoPlay p "16009"
        choose "attack" (isHeroAttack p)
        pickVillain
        choose "Lashing Vines" (isAbilityOfCard lashing)
        exhaustedNow p `shouldReturn` False
        countersOf p `shouldReturn` 0
        upgradeIsExhausted (upgradeIdOf lashing) `shouldReturn` True

    it "readies him after his basic thwart" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 3
        setCounters p 2
        lashing <- putIntoPlay p "16009"
        choose "thwart" (isHeroThwart p)
        pickMainScheme
        choose "Lashing Vines" (isAbilityOfCard lashing)
        exhaustedNow p `shouldReturn` False

    it "needs 2 growth counters" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        setCounters p 1
        lashing <- putIntoPlay p "16009"
        choose "attack" (isHeroAttack p)
        pickVillain
        offers (isAbilityOfCard lashing) `shouldReturn` False
