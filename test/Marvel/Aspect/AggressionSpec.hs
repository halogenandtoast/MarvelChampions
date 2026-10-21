module Marvel.Aspect.AggressionSpec (spec) where

import Marvel.Minion.Cards qualified as Cards
import TestImport

spec :: Spec
spec = describe "Aggression" do
  describe "Hulk (01050)" do
    it "discards the top card of the deck after attacking; a physical card deals 2 damage to an enemy" do
      tg <- testGame ["she-hulk"]
      let p = player1 tg
      inGame tg do
        clearHand p
        hulk <- putInHand p "01050"
        energy <- putInHand p "01088"
        playWith hulk [energy]
        _ <- stackDeck p ["01090"]
        h <- allyIn "01050"
        villain <- getVillain
        choose "attack with Hulk" (isAllyAttack h)
        choose "attack Rhino" (isTargetChoice (toId villain))
        choose "Hulk's forced response" (isAbilityOf (allyId h))
        choose "deal 2 damage" (isLabel "$label.deal2DamageToAnEnemy")
        chooseIf (isTargetChoice (toId villain))
        villainDamageNow `shouldReturn` 5
        -- one consequential damage
        (.allyDamage) <$> allyIn "01050" `shouldReturn` 1
        map toCardCode <$> discardOf p `shouldReturn` ["01090", "01088"]

    it "is discarded when the card has a mental resource" do
      tg <- testGame ["she-hulk"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01050"
        _ <- stackDeck p ["01089"]
        h <- allyIn "01050"
        villain <- getVillain
        choose "attack with Hulk" (isAllyAttack h)
        choose "attack Rhino" (isTargetChoice (toId villain))
        choose "Hulk's forced response" (isAbilityOf (allyId h))
        chooseIf (isLabel "$label.discardHulk")
        alliesWith "01050" `shouldReturn` []

    it "deals 1 damage to each character when the card has an energy resource" do
      tg <- testGame ["she-hulk"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01050"
        _ <- stackDeck p ["01088"]
        h <- allyIn "01050"
        villain <- getVillain
        choose "attack with Hulk" (isAllyAttack h)
        choose "attack Rhino" (isTargetChoice (toId villain))
        choose "Hulk's forced response" (isAbilityOf (allyId h))
        chooseIf (isLabel "$label.deal1DamageToEachCharacter")
        villainDamageNow `shouldReturn` 4
        damageOf p `shouldReturn` 1
        -- 1 from the effect, 1 consequential
        (.allyDamage) <$> allyIn "01050" `shouldReturn` 2

  describe "Tigra (01051)" do
    it "heals 1 after attacking and defeating a minion" do
      tg <- testGame ["she-hulk"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01051"
        mercenary <- addMinion p Cards.hydraMercenary
        tigra <- allyIn "01051"
        resolve [AllyMessage (allyId tigra) (AllyDamaged (toSource p) (toDamage 1 FromAbility))]
        -- Hydra Mercenary has 3 hit points: 1 damage then a 2 ATK attack defeats it
        resolve [MinionMessage mercenary (MinionDamaged (toSource p) (toDamage 1 FromAbility))]
        choose "attack with Tigra" (isAllyAttack tigra)
        choose "attack the mercenary" (isTargetChoice mercenary)
        minionDamageOf mercenary `shouldReturn` Nothing
        choose "use Tigra" (isAbilityOf (allyId tigra))
        -- healed before the consequential damage is dealt, so she ends with 1
        (.allyDamage) <$> allyIn "01051" `shouldReturn` 1

    it "has nothing to heal when undamaged: her response comes before the consequential damage" do
      tg <- testGame ["she-hulk"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01051"
        mercenary <- addMinion p Cards.hydraMercenary
        tigra <- allyIn "01051"
        resolve [MinionMessage mercenary (MinionDamaged (toSource p) (toDamage 1 FromAbility))]
        choose "attack with Tigra" (isAllyAttack tigra)
        choose "attack the mercenary" (isTargetChoice mercenary)
        offers (isAbilityOf (allyId tigra)) `shouldReturn` False
        (.allyDamage) <$> allyIn "01051" `shouldReturn` 1

  describe "Chase Them Down (01052)" do
    it "removes 2 threat after the hero attacks and defeats an enemy" do
      tg <- testGame ["she-hulk"]
      let p = player1 tg
      inGame tg do
        clearHand p
        chase <- putInHand p "01052"
        placeMainThreat 3
        threat <- mainSchemeThreatNow
        changeForm p >> pass
        mercenary <- addMinion p Cards.hydraMercenary
        resolve [MinionMessage mercenary (MinionDamaged (toSource p) (toDamage 1 FromAbility))]
        choose "attack" (isHeroAttack p)
        chooseIf (isTargetChoice mercenary)
        choose "play Chase Them Down" (isPlay chase)
        mid <- mainSchemeId
        chooseIf (isTargetChoice mid)
        mainSchemeThreatNow `shouldReturn` (threat - 2)

    it "is not offered when an attack does not defeat the enemy" do
      tg <- testGame ["she-hulk"]
      let p = player1 tg
      inGame tg do
        clearHand p
        chase <- putInHand p "01052"
        _ <- putInHand p "01089"
        changeForm p >> pass
        villain <- getVillain
        choose "attack" (isHeroAttack p)
        choose "Rhino" (isTargetChoice (toId villain))
        offers (isPlay chase) `shouldReturn` False

  describe "Relentless Assault (01053)" do
    it "deals 5 damage to a minion, with overkill when paid with a physical resource" do
      tg <- testGame ["she-hulk"]
      let p = player1 tg
      inGame tg do
        clearHand p
        assault <- putInHand p "01053"
        strength <- putInHand p "01090"
        changeForm p >> pass
        mercenary <- addMinion p Cards.hydraMercenary
        playWith assault [strength]
        chooseIf (isTargetChoice mercenary)
        minionDamageOf mercenary `shouldReturn` Nothing
        -- 3 hit points, so 2 excess damage reaches Rhino
        villainDamageNow `shouldReturn` 2

    it "has no overkill otherwise and cannot target the villain" do
      tg <- testGame ["she-hulk"]
      let p = player1 tg
      inGame tg do
        clearHand p
        assault <- putInHand p "01053"
        energy <- putInHand p "01088"
        changeForm p >> pass
        offers (isPlay assault) `shouldReturn` False
        mercenary <- addMinion p Cards.hydraMercenary
        villain <- getVillain
        playWith assault [energy]
        offers (isTargetChoice (toId villain)) `shouldReturn` False
        chooseIf (isTargetChoice mercenary)
        villainDamageNow `shouldReturn` 0

  describe "Uppercut (01054)" do
    it "deals 5 damage to an enemy, only in hero form" do
      tg <- testGame ["she-hulk"]
      let p = player1 tg
      inGame tg do
        clearHand p
        uppercut <- putInHand p "01054"
        strength <- putInHand p "01090"
        energy <- putInHand p "01088"
        offers (isPlay uppercut) `shouldReturn` False
        changeForm p >> pass
        playWith uppercut [strength, energy]
        villain <- getVillain
        choose "Rhino" (isTargetChoice (toId villain))
        villainDamageNow `shouldReturn` 5

  describe "The Power of Aggression (01055)" do
    it "pays 2 for an Aggression card" do
      tg <- testGame ["she-hulk"]
      let p = player1 tg
      inGame tg do
        clearHand p
        tac <- putInHand p "01056"
        power <- putInHand p "01055"
        genius <- putInHand p "01089"
        playWith tac [power, genius]
        supportsWith "01056" >>= (`shouldSatisfy` ((== 1) . length))

    it "pays only 1 for a card of another aspect" do
      tg <- testGame ["she-hulk"]
      let p = player1 tg
      inGame tg do
        clearHand p
        haymaker <- putInHand p "01087"
        power <- putInHand p "01055"
        changeForm p >> pass
        -- Haymaker costs 2
        offers (isPlay haymaker) `shouldReturn` False
        _ <- putInHand p "01089"
        playWith haymaker [power]
        offers isFinishPayment `shouldReturn` False

  describe "Tac Team (01056)" do
    it "has 3 uses, each exhausting it to deal 2 damage, and is discarded after the last" do
      tg <- testGame ["she-hulk"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01056"
        [team] <- supportsWith "01056"
        team.supportUses `shouldBe` 3
        villain <- getVillain
        for_ [1 :: Int .. 3] \n -> do
          resolve [SupportMessage (supportId team) ReadiedSupport]
          choose "use Tac Team" (isAbilityOf (supportId team))
          chooseIf (isTargetChoice (toId villain))
          villainDamageNow `shouldReturn` fromIntegral (2 * n)
          when (n < 3) do
            -- exhausted: cannot be used again until readied
            offers (isAbilityOf (supportId team)) `shouldReturn` False
        supportsWith "01056" `shouldReturn` []
        map toCardCode <$> discardOf p `shouldReturn` ["01056"]

  describe "Combat Training (01057)" do
    it "gives the hero +1 ATK, max 1 per player" do
      tg <- testGame ["she-hulk"]
      let p = player1 tg
      inGame tg do
        clearHand p
        training <- putInHand p "01057"
        training2 <- putInHand p "01057"
        energy <- putInHand p "01088"
        playWith training [energy]
        offers (isPlay training2) `shouldReturn` False
        changeForm p >> pass
        villain <- getVillain
        before <- villainDamageNow
        choose "attack" (isHeroAttack p)
        choose "Rhino" (isTargetChoice (toId villain))
        -- She-Hulk has 3 ATK
        villainDamageNow `shouldReturn` (before + 4)
