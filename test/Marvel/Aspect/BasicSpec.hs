module Marvel.Aspect.BasicSpec (spec) where

import Marvel.Ability.Types (AbilityType (Action, HeroAction))
import Marvel.Villain.Types (villainStunned)
import TestImport

isAbilityOfType :: (IsRef a) => a -> AbilityType -> Choice -> Bool
isAbilityOfType a t = \case
  UseAbility ab -> abilitySource ab == toSource a && abilityType ab == t
  _ -> False

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

spec :: Spec
spec = describe "Basic" do
  describe "Mockingbird (01083)" do
    it "stuns an enemy after she enters play" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        mockingbird <- putInHand p "01083"
        strength <- putInHand p "01090"
        energy <- putInHand p "01088"
        playWith mockingbird [strength, energy]
        mb <- allyIn "01083"
        choose "use Mockingbird" (isAbilityOf (allyId mb))
        villain <- getVillain
        chooseIf (isTargetChoice (toId villain))
        villainStunned . toAttrs <$> getVillain `shouldReturn` True

  describe "Nick Fury (01084)" do
    it "draws 3 cards as he enters play, and is discarded at the end of the round" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        fury <- putInHand p "01084"
        strength <- putInHand p "01090"
        energy <- putInHand p "01088"
        playWith fury [strength, energy]
        nick <- allyIn "01084"
        choose "Nick Fury's forced response" (isAbilityOf (allyId nick))
        choose "draw 3 cards" (isLabel "$label.draw3Cards")
        length <$> handOf p `shouldReturn` 3
        toNextTurn
        alliesWith "01084" `shouldReturn` []
        map toCardCode <$> discardOf p >>= (`shouldSatisfy` elem "01084")

    it "can deal 4 damage to an enemy instead, and is unique" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        fury <- putInHand p "01084"
        strength <- putInHand p "01090"
        energy <- putInHand p "01088"
        playWith fury [strength, energy]
        nick <- allyIn "01084"
        choose "Nick Fury's forced response" (isAbilityOf (allyId nick))
        choose "deal 4 damage" (isLabel "$label.deal4DamageToAnEnemy")
        villain <- getVillain
        chooseIf (isTargetChoice (toId villain))
        villainDamageNow `shouldReturn` 4
        fury2 <- putInHand p "01084"
        _ <- putInHand p "01090"
        _ <- putInHand p "01088"
        -- a second Nick Fury cannot enter play
        offers (isPlay fury2) `shouldReturn` False

  describe "Emergency (01085)" do
    it "reduces the threat the villain places when scheming by 1" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        emergency <- putInHand p "01085"
        threat <- mainSchemeThreatNow
        villain <- getVillain
        offers (isPlay emergency) `shouldReturn` False
        -- Rhino schemes for 1, and Emergency may be played in alter-ego form
        runNow [VillainMessage (toId villain) VillainSchemed]
        choose "play Emergency" (isPlay emergency)
        pass
        mainSchemeThreatNow `shouldReturn` threat

  describe "First Aid (01086)" do
    it "heals 2 damage from any character" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        firstAid <- putInHand p "01086"
        genius <- putInHand p "01089"
        offers (isPlay firstAid) `shouldReturn` False
        putIntoPlay p "01083"
        mb <- allyIn "01083"
        resolve [AllyMessage (allyId mb) (AllyDamaged (toSource p) (toDamage 2 FromAbility))]
        playWith firstAid [genius]
        chooseIf (isTargetChoice (AllyCharacter (allyId mb)))
        (.allyDamage) <$> allyIn "01083" `shouldReturn` 0

  describe "Haymaker (01087)" do
    it "deals 3 damage to an enemy, in hero form" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        haymaker <- putInHand p "01087"
        energy <- putInHand p "01088"
        offers (isPlay haymaker) `shouldReturn` False
        changeForm p
        playWith haymaker [energy]
        villain <- getVillain
        chooseIf (isTargetChoice (toId villain))
        villainDamageNow `shouldReturn` 3

  describe "Energy, Genius and Strength (01088-01090)" do
    it "each generate 2 resources of their type" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01068"
        vision <- allyIn "01068"
        genius <- putInHand p "01089"
        strength <- putInHand p "01090"
        -- Vision's ability needs an energy resource
        offers (isAbilityOfType (allyId vision) Action) `shouldReturn` False
        energy <- putInHand p "01088"
        offers (isAbilityOfType (allyId vision) Action) `shouldReturn` True
        -- 2 mental resources pay for For Justice! and gain its bonus
        changeForm p
        placeMainThreat 6
        threat <- mainSchemeThreatNow
        justice <- putInHand p "01060"
        playWith justice [genius]
        mid <- mainSchemeId
        chooseIf (isTargetChoice mid)
        mainSchemeThreatNow `shouldReturn` (threat - 4)
        haymaker <- putInHand p "01087"
        playWith haymaker [strength]
        villain <- getVillain
        chooseIf (isTargetChoice (toId villain))
        villainDamageNow `shouldReturn` 3
        handOf p `shouldReturn` [energy]

  describe "Avengers Mansion (01091)" do
    it "exhausts to have a player draw 1 card, max 1 per player" do
      tg <- testGame ["spider-man", "captain-marvel"]
      let (p, q) = (player1 tg, player2 tg)
      inGame tg do
        clearHand p
        putIntoPlay p "01091"
        mansion2 <- putInHand p "01091"
        _ <- putInHand p "01088"
        _ <- putInHand p "01089"
        offers (isPlay mansion2) `shouldReturn` False
        [mansion] <- supportsWith "01091"
        others <- length <$> handOf q
        choose "use Avengers Mansion" (isAbilityOf (supportId mansion))
        choose "the other player" (isTargetChoice q)
        length <$> handOf q `shouldReturn` (others + 1)
        offers (isAbilityOf (supportId mansion)) `shouldReturn` False

  describe "Helicarrier (01092)" do
    it "exhausts to reduce the cost of the next card the player plays this phase by 1" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01092"
        [heli] <- supportsWith "01092"
        haymaker <- putInHand p "01087"
        power <- putInHand p "01079"
        _ <- putInHand p "01087"
        choose "use Helicarrier" (isAbilityOf (supportId heli))
        -- the only player is chosen
        changeForm p
        -- Haymaker costs 1 now, so a single resource pays for it
        playWith haymaker [power]
        villain <- getVillain
        chooseIf (isTargetChoice (toId villain))
        villainDamageNow `shouldReturn` 3
        -- only the next card
        [haymaker2] <- handOf p
        -- a single resource no longer pays for the other Haymaker
        _ <- putInHand p "01079"
        offers (isPlay haymaker2) `shouldReturn` False

  describe "Tenacity (01093)" do
    it "spends a physical resource and is discarded to ready the hero" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01093"
        [tenacity] <- upgradesWith "01093"
        changeForm p
        villain <- getVillain
        choose "attack" (isHeroAttack p)
        chooseIf (isTargetChoice (toId villain))
        exhaustedNow p `shouldReturn` True
        energy <- putInHand p "01088"
        offers (isAbilityOfType (upgradeId tenacity) HeroAction) `shouldReturn` False
        strength <- putInHand p "01090"
        choose "use Tenacity" (isAbilityOfType (upgradeId tenacity) HeroAction)
        choose "pay" (isPayWith strength)
        chooseIf isFinishPayment
        exhaustedNow p `shouldReturn` False
        upgradesWith "01093" `shouldReturn` []
        handOf p `shouldReturn` [energy]

  describe "uniqueness" do
    it "keeps a unique ally from entering play while a copy is in play under any player's control" do
      tg <- testGame ["spider-man", "captain-marvel"]
      let (p, q) = (player1 tg, player2 tg)
      inGame tg do
        putIntoPlay q "01067"
        clearHand p
        maria <- putInHand p "01067"
        _ <- putInHand p "01088"
        offers (isPlay maria) `shouldReturn` False
        _ <- putInDiscard p "01067"
        call <- putInHand p "01071"
        offers (isPlay call) `shouldReturn` False
