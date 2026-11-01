module Marvel.Aspect.GrootPackSpec (spec) where

import Control.Monad (replicateM)
import Marvel.Identity (getModifiedCost)
import Marvel.Keyword (Keyword (Retaliate))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier (Modifier (KeywordModifier))
import Marvel.Matchers (AllyMatcher (AllyNamedAs, AllyWithTitle))
import Marvel.Query (selectAny)
import TestImport

grootGame :: IO TestGame
grootGame = testGame ["groot"]

-- | The player in hero form, their hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

countersOf :: IdentityId -> GameAppT Natural
countersOf p = (.playerIdentityCounters) <$> playerAttrs p

setCounters :: IdentityId -> Natural -> GameAppT ()
setCounters p n = withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
  PlayerIdentity a {playerIdentityCounters = n}

-- | Play a card from hand paying with new Energy cards (2 resources each), declining an
-- interrupt to its being played.
playPaying :: IdentityId -> PlayerCard -> Int -> GameAppT ()
playPaying p card n = do
  payment <- replicateM n (putInHand p "01088")
  choose "play the card" (isPlay card)
  chooseIf isNoResponse
  payWith payment

-- | Rhino attacks the player: 2 ATK and a Vulture's 2 boost icons.
villainAttacks :: IdentityId -> GameAppT ()
villainAttacks p = do
  _ <- stackEncounterDeck [Minions.vulture]
  vid <- villainIdNow
  start [VillainMessage vid (VillainAttacks p)]

pickVillain :: GameAppT ()
pickVillain = chooseIf . isTargetChoice =<< villainIdNow

isFloraColossus :: Choice -> Bool
isFloraColossus = isNamedAbility "Flora Colossus"

retaliates :: IdentityId -> GameAppT Bool
retaliates p = elem (KeywordModifier (Retaliate 1)) <$> getModifiers p

costOf :: IdentityId -> PlayerCard -> GameAppT Int
costOf p card = do
  pl <- getPlayer p
  getModifiedCost pl card Nothing

codesInHand :: IdentityId -> GameAppT [CardCode]
codesInHand p = map toCardCode <$> handOf p

spec :: Spec
spec = describe "Groot pack" do
  describe "the groot starter deck" do
    it "deals 40 cards" do
      sum (map snd (starter "groot").cards) `shouldBe` (40 :: Int) :: IO ()

    it "resolves the pack's reprints as the cards they reprint" do
      let same :: CardCode -> CardCode -> IO ()
          same a b = cdName (lookupPlayerCard a) `shouldBe` cdName (lookupPlayerCard b)
      same "16013" "09015"
      same "16015" "01079"
      same "16018" "01082"
      same "16021" "01088"
      same "16022" "01089"
      same "16023" "01090"

  describe "Starhawk (16012)" do
    it "returns to your hand instead of taking damage equal to his remaining hit points" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putIntoPlay p "16012"
        start (damageAlly (allyIdOf card) (toSource p) (toDamage 3 FromAbility))
        choose "Starhawk" (isAbilityOfCard card)
        resolve []
        alliesWith "16012" `shouldReturn` []
        codesInHand p `shouldReturn` ["16012"]

    it "his attack's 2 consequential damage, as printed, at 2 remaining hit points returns him to your hand" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putIntoPlay p "16012"
        resolve (damageAlly (allyIdOf card) (toSource p) (toDamage 1 FromAbility))
        starhawk <- allyIn "16012"
        choose "Starhawk attacks" (isAllyAttack starhawk)
        chooseIf . isTargetChoice =<< villainIdNow
        choose "Starhawk" (isAbilityOfCard card)
        resolve []
        alliesWith "16012" `shouldReturn` []
        codesInHand p `shouldReturn` ["16012"]

    it "counts the damage already on him" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putIntoPlay p "16012"
        resolve (damageAlly (allyIdOf card) (toSource p) (toDamage 1 FromAbility))
        allyDamage <$> allyIn "16012" `shouldReturn` 1
        start (damageAlly (allyIdOf card) (toSource p) (toDamage 2 FromAbility))
        choose "Starhawk" (isAbilityOfCard card)
        resolve []
        alliesWith "16012" `shouldReturn` []
        codesInHand p `shouldReturn` ["16012"]

    it "does nothing against less or more damage than his remaining hit points" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putIntoPlay p "16012"
        start (damageAlly (allyIdOf card) (toSource p) (toDamage 2 FromAbility))
        chooseIf (isAbilityOfCard card)
        resolve []
        allyDamage <$> allyIn "16012" `shouldReturn` 2
        start (damageAlly (allyIdOf card) (toSource p) (toDamage 4 FromAbility))
        chooseIf (isAbilityOfCard card)
        resolve []
        alliesWith "16012" `shouldReturn` []
        codesInHand p `shouldReturn` []

    it "does nothing when a tough status card prevents the damage" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putIntoPlay p "16012"
        resolve [GiveStatus (AllyRef $ allyIdOf card) ToughStatus]
        start (damageAlly (allyIdOf card) (toSource p) (toDamage 3 FromAbility))
        chooseIf (isAbilityOfCard card)
        resolve []
        starhawk <- allyIn "16012"
        allyDamage starhawk `shouldBe` 0
        allyTough starhawk `shouldBe` False
        codesInHand p `shouldReturn` []

  describe "Fighting Fit (16014)" do
    it "deals 5 damage to the villain at full hit points" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        card <- putInHand p "16014"
        playPaying p card 1
        pickVillain
        pass
        villainDamageNow `shouldReturn` 5

    it "deals 2 damage below them" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        setDamage p 1
        card <- putInHand p "16014"
        playPaying p card 1
        pickVillain
        pass
        villainDamageNow `shouldReturn` 2

  describe "Dauntless (16016)" do
    it "gives your hero retaliate 1 while at full hit points, and not below them" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "16016"
        retaliates p `shouldReturn` True
        setDamage p 1
        retaliates p `shouldReturn` False

    it "retaliates when Flora Colossus prevents all of the attack's damage" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        setCounters p 4
        _ <- putIntoPlay p "16016"
        villainAttacks p
        choose "no defender" isNoDefender
        choose "Flora Colossus" isFloraColossus
        resolve []
        damageOf p `shouldReturn` 0
        villainDamageNow `shouldReturn` 1

    it "does not retaliate once the attack's damage is taken" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        _ <- putIntoPlay p "16016"
        villainAttacks p
        choose "no defender" isNoDefender
        resolve []
        damageOf p `shouldReturn` 4
        villainDamageNow `shouldReturn` 0

    it "is played under another player's control, for their hero" do
      tg <- testGame ["groot", "captain-marvel"]
      let (p, other) = (player1 tg, player2 tg)
      inGame tg do
        asHero p
        resolve [IdentityMessage other (ChangedToForm A)]
        card <- putInHand p "16016"
        energy <- putInHand p "01088"
        playOn card other [energy]
        retaliates other `shouldReturn` True
        retaliates p `shouldReturn` False

  describe "Hard to Ignore (16017)" do
    it "after defending and taking no damage, exhausts to remove 1 threat from the main scheme" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 2
        hard <- putIntoPlay p "16017"
        -- Desperate Defense's reprint: 3 DEF and 2 more against 4 damage
        _ <- putInHand p "16013"
        _ <- putInHand p "01088"
        villainAttacks p
        choose "defend" isDefend
        choose "Desperate Defense" \case TargetLabel _ cs -> any (\case PlayCard c _ -> toCardCode c == "16013"; _ -> False) cs; _ -> False
        chooseIf \case PayWithCard _ -> True; _ -> False
        chooseIf isFinishPayment
        damageOf p `shouldReturn` 0
        threat <- mainSchemeThreatNow
        choose "Hard to Ignore" (isAbilityOfCard hard)
        chooseIf . isTargetChoice =<< mainSchemeIdNow
        mainSchemeThreatNow `shouldReturn` (threat - 1)
        upgradeIsExhausted (upgradeIdOf hard) `shouldReturn` True

    it "is not offered when the defense takes damage" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 2
        hard <- putIntoPlay p "16017"
        villainAttacks p
        choose "defend" isDefend
        offers (isAbilityOfCard hard) `shouldReturn` False
        damageOf p `shouldReturn` 1

  describe "Rocket Raccoon (16019)" do
    it "is played only by a Guardian identity" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        clearHand p
        rocket <- putInHand p "16019"
        _ <- replicateM 2 (putInHand p "01088")
        -- Groot's alter-ego is an Outlaw, his hero a Guardian
        offers (isPlay rocket) `shouldReturn` False
        changeForm p
        pass
        offers (isPlay rocket) `shouldReturn` True

    it "gets +3 ATK and overkill attacking a minion" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putIntoPlay p "16019"
        mercenary <- addMinion p Minions.hydraMercenary
        rocket <- allyIn "16019"
        choose "attack with Rocket" (isAllyAttack rocket)
        choose "the minion" (isTargetChoice mercenary)
        choose "Rocket Raccoon" (isAbilityOfCard card)
        resolve []
        -- 4 damage against 3 hit points, 1 over to the villain
        minionDamageOf mercenary `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 1

    it "attacks with his printed ATK when it is declined" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putIntoPlay p "16019"
        mercenary <- addMinion p Minions.hydraMercenary
        rocket <- allyIn "16019"
        choose "attack with Rocket" (isAllyAttack rocket)
        choose "the minion" (isTargetChoice mercenary)
        offers (isAbilityOfCard card) `shouldReturn` True
        choose "decline" isNoResponse
        resolve []
        minionDamageOf mercenary `shouldReturn` Just 1

    it "is not offered attacking the villain" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putIntoPlay p "16019"
        rocket <- allyIn "16019"
        choose "attack with Rocket" (isAllyAttack rocket)
        pickVillain
        offers (isAbilityOfCard card) `shouldReturn` False
        resolve []
        villainDamageNow `shouldReturn` 1

  describe "Flora and Fauna (16020)" do
    it "is played only with Groot and Rocket Raccoon in play" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        card <- putInHand p "16020"
        _ <- putInHand p "01088"
        offers (isPlay card) `shouldReturn` False
        _ <- putIntoPlay p "16019"
        offers (isPlay card) `shouldReturn` True

    it "Team-Up counts an ally named by its subtitle as well as its title" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "16012" -- Starhawk — Stakar Ogord
        byTitle <- selectAny (AllyWithTitle "Stakar Ogord")
        bySubtitle <- selectAny (AllyNamedAs "Stakar Ogord")
        byName <- selectAny (AllyNamedAs "Starhawk")
        liftIO $ (byTitle, bySubtitle, byName) `shouldBe` (False, True, True)

    it "places 2 growth counters on Groot and readies him" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        setCounters p 3
        _ <- putIntoPlay p "16019"
        resolve [IdentityMessage p ExhaustedIdentity]
        card <- putInHand p "16020"
        playPaying p card 1
        chooseIf (isTargetChoice p)
        countersOf p `shouldReturn` 5
        exhaustedNow p `shouldReturn` False

    it "places them only to a maximum of 10" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        setCounters p 9
        _ <- putIntoPlay p "16019"
        card <- putInHand p "16020"
        playPaying p card 1
        chooseIf (isTargetChoice p)
        countersOf p `shouldReturn` 10

    -- ponytail: no Rocket Raccoon hero in this branch, so the upgrade option is only
    -- seen not to be offered for another player's upgrades
    it "teams up with another player's Rocket Raccoon ally, offering only Groot" do
      tg <- testGame ["groot", "star-lord"]
      let (p, other) = (player1 tg, player2 tg)
      inGame tg do
        asHero p
        _ <- putIntoPlay other "16019"
        dauntless <- putIntoPlay other "16016"
        card <- putInHand p "16020"
        playPaying p card 1
        -- Groot is the only choice: answered at once
        offers (isTargetChoice $ upgradeIdOf dauntless) `shouldReturn` False
        chooseIf (isTargetChoice p)
        countersOf p `shouldReturn` 2

  describe "Deft Focus (16024)" do
    it "exhausts to make the next Superpower card played this turn cost 1 less" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        focus <- putIntoPlay p "16024"
        -- Contaminant Immunity, a Superpower event costing 2; Fighting Fit is not one
        immunity <- putInHand p "04037"
        other <- putInHand p "04037"
        fit <- putInHand p "16014"
        costOf p immunity `shouldReturn` 2
        choose "Deft Focus" (isAbilityOfCard focus)
        upgradeIsExhausted (upgradeIdOf focus) `shouldReturn` True
        costOf p immunity `shouldReturn` 1
        costOf p fit `shouldReturn` 2
        -- Hard to Ignore pays a single resource
        hard <- putInHand p "16017"
        choose "play Contaminant Immunity" (isPlay immunity)
        chooseIf isNoResponse
        payWith [hard]
        costOf p other `shouldReturn` 2

    it "ends at the end of the turn" do
      tg <- grootGame
      let p = player1 tg
      inGame tg do
        asHero p
        focus <- putIntoPlay p "16024"
        immunity <- putInHand p "04037"
        choose "Deft Focus" (isAbilityOfCard focus)
        costOf p immunity `shouldReturn` 1
        endTurn
        costOf p immunity `shouldReturn` 2
