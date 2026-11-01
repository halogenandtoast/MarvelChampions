module Marvel.Hero.AngelSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability qualified as Ability
import Marvel.GameValue
import Marvel.Effect.Types (Effect (..), Attrs (..), IsEffect)
import Marvel.Matchers.Types (EntityMatcher (AllyEntity), AllyMatcher (AllyWithId))
import Marvel.Modifier (Modifier (..))
import Marvel.Phase
import Marvel.Stats
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (PlayerIdentitySide (..), currentIdentity)
import Marvel.AlterEgo.Runner qualified as AlterEgo
import Marvel.Window qualified as W
import TestImport

angel :: IO TestGame
angel = testGame ["angel"]

inForm :: IdentityId -> Side -> GameAppT ()
inForm p side = changeFormTo p side >> pass >> clearHand p

playFlight :: IdentityId -> GameAppT ()
playFlight p = do
  placeMainThreat 4
  flight <- putInHand p "42006"
  energy <- putInHand p "01088"
  playWith flight [energy]
  chooseMainScheme

allyDamageNow :: AllyId -> GameAppT Natural
allyDamageNow aid = getsGame (maybe 0 (allyDamage . toAttrs) . HashMap.lookup aid . gameAllies)

newtype AngelTestModifiers = AngelTestModifiers (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage AngelTestModifiers where
  runMessage msg (AngelTestModifiers attrs) = AngelTestModifiers <$> runMessage msg attrs

preventAllyHealing :: AllyId -> GameAppT ()
preventAllyHealing aid = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (AngelTestModifiers EffectAttrs
    {effectId = eid, effectCardCode = "01039", effectSource = GameRef,
     effectMatcher = AllyEntity (AllyWithId aid), effectModifiers = [CannotBeHealedByPlayerCards], effectEnds = Nothing})

spec :: Spec
spec = describe "Angel" do
  describe "identity forms" do
    it "starts as Warren, with three selectable faces and printed stats" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        handSizeOf p `shouldReturn` 6
        hitPointsOf p `shouldReturn` 12
        currentIdentity <$> getPlayer p >>= \case
          AlterEgoSide a -> AlterEgo.alterEgoBaseRecovery (toAttrs a) `shouldBe` Rec 3
          _ -> liftIO $ expectationFailure "Warren was not showing"
        choose "change form" \case
          UseAbility ab -> ChangeForm `elem` abilityChoices ab && abilitySource ab == toSource p
          _ -> False
        offers (isFormChoice $ Just A) `shouldReturn` True
        offers (isFormChoice $ Just C) `shouldReturn` True
        choose "Angel" (isFormChoice $ Just A)
        currentIdentity <$> getPlayer p >>= \case
          HeroSide h -> do
            Hero.heroBaseThwart (toAttrs h) `shouldBe` Thw 2
            Hero.heroBaseAttack (toAttrs h) `shouldBe` Atk 1
            Hero.heroBaseDefense (toAttrs h) `shouldBe` Def 2
          _ -> liftIO $ expectationFailure "Angel was not showing"

    it "Regrowth heals one damage and is limited once per round" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        setDamage p 4
        choose "Regrowth" (isNamedAbility "Regrowth")
        damageOf p `shouldReturn` 3
        offers (isNamedAbility "Regrowth") `shouldReturn` False

    it "Regrowth is unavailable when there is no damage to heal" do
      tg <- angel
      inGame tg $ offers (isNamedAbility "Regrowth") `shouldReturn` False

    it "Archangel has his printed stats and adds one acceleration only while showing" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        here <- areaId <$> getAreaOf p
        before <- getAccelerationCountIn here
        inForm p C
        getAccelerationCountIn here `shouldReturn` before + 1
        currentIdentity <$> getPlayer p >>= \case
          HeroSide h -> do
            Hero.heroBaseThwart (toAttrs h) `shouldBe` Thw 0
            Hero.heroBaseAttack (toAttrs h) `shouldBe` Atk 2
            Hero.heroBaseDefense (toAttrs h) `shouldBe` Def 3
          _ -> liftIO $ expectationFailure "Archangel was not showing"
        resolve [IdentityMessage p $ ChangedToForm B]
        getAccelerationCountIn here `shouldReturn` before

  describe "Angel of Life" do
    it "draws one card after an Aerial event, only once per phase" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p A
        playFlight p
        length <$> handOf p `shouldReturn` 0
        choose "Angel of Life" (isNamedAbility "Angel of Life")
        length <$> handOf p `shouldReturn` 1
        clearHand p
        playFlight p
        offers (isNamedAbility "Angel of Life") `shouldReturn` False
        length <$> handOf p `shouldReturn` 0

    it "can draw again in a new phase" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p A
        playFlight p
        choose "Angel of Life" (isNamedAbility "Angel of Life")
        resolve [BeginPhase PlayerPhase]
        clearHand p
        playFlight p
        offers (isNamedAbility "Angel of Life") `shouldReturn` True
        choose "Angel of Life" (isNamedAbility "Angel of Life")
        length <$> handOf p `shouldReturn` 1

    it "does not respond to a non-Aerial card" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p A
        card <- putInHand p "01087"
        energy <- putInHand p "01088"
        playWith card [energy]
        chooseVillain
        offers (isNamedAbility "Angel of Life") `shouldReturn` False

    it "does not reset its phase limit after changing away and back" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p A
        playFlight p
        choose "Angel of Life" (isNamedAbility "Angel of Life")
        resolve [IdentityMessage p $ ChangedToForm C, IdentityMessage p $ ChangedToForm A]
        clearHand p
        playFlight p
        offers (isNamedAbility "Angel of Life") `shouldReturn` False

  describe "Angel of Death" do
    it "deals the event's printed cost as ability damage, once per phase" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p C
        playFlight p
        choose "Angel of Death" (isNamedAbility "Angel of Death")
        chooseVillain
        villainDamageNow `shouldReturn` 2
        playFlight p
        offers (isNamedAbility "Angel of Death") `shouldReturn` False
        villainDamageNow `shouldReturn` 2

    it "uses printed cost even after Wings reduces the event's payment" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p C
        wings <- putIntoPlay p "42010"
        choose "Wings" (isAbilityOfCard wings)
        placeMainThreat 4
        flight <- putInHand p "42006"
        playWith flight []
        chooseMainScheme
        choose "Angel of Death" (isNamedAbility "Angel of Death")
        chooseVillain
        villainDamageNow `shouldReturn` 2

  describe "Psylocke" do
    it "heals one damage after attacking while Angel, before consequential damage" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p A
        card <- putIntoPlay p "42002"
        let aid = allyIdOf card
        resolve [AllyMessage aid $ AllySetDamage 1]
        a <- allyIn "42002"
        choose "Psylocke attacks" (isAllyAttack a)
        chooseVillain
        choose "Psylocke's response" (isAbilityOfCard card)
        pass
        allyDamageNow aid `shouldReturn` 1

    it "does not offer its healing response when player-card healing is forbidden" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p A
        card <- putIntoPlay p "42002"
        let aid = allyIdOf card
        resolve [AllyMessage aid $ AllySetDamage 1]
        preventAllyHealing aid
        a <- allyIn "42002"
        choose "Psylocke attacks" (isAllyAttack a)
        chooseVillain
        offers (isAbilityOfCard card) `shouldReturn` False
        allyDamageNow aid `shouldReturn` 2

    it "readies Archangel after attacking" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p C
        card <- putIntoPlay p "42002"
        resolve [IdentityMessage p ExhaustedIdentity]
        a <- allyIn "42002"
        choose "Psylocke attacks" (isAllyAttack a)
        chooseVillain
        choose "Psylocke's response" (isAbilityOfCard card)
        pass
        isExhaustedNow p `shouldReturn` False
        allyDamageNow (allyIdOf card) `shouldReturn` 1

    it "does not offer her hero response in alter-ego form" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        card <- putIntoPlay p "42002"
        resolve [IdentityMessage p ExhaustedIdentity]
        a <- allyIn "42002"
        choose "Psylocke attacks" (isAllyAttack a)
        chooseVillain
        offers (isAbilityOfCard card) `shouldReturn` False
        isExhaustedNow p `shouldReturn` True

  describe "Worthington Industries" do
    it "shuffles an Aerial card from discard and draws in alter-ego form" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        clearHand p
        industry <- putIntoPlay p "42009"
        aerial <- putInDiscard p "42006"
        other <- putInDiscard p "01087"
        choose "Worthington Industries" (isAbilityOfCard industry)
        offers (isTargetChoice other) `shouldReturn` False
        choose "shuffle the Aerial card" (isTargetChoice aerial)
        length <$> handOf p `shouldReturn` 1
        discardOf p >>= (`shouldSatisfy` notElem aerial)
        getsGame (maybe False (supportExhausted . toAttrs) . HashMap.lookup (supportIdOf industry) . gameSupports) `shouldReturn` True

    it "shuffles in hero form without drawing" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p A
        industry <- putIntoPlay p "42009"
        aerial <- putInDiscard p "42006"
        choose "Worthington Industries" (isAbilityOfCard industry)
        choose "shuffle the Aerial card" (isTargetChoice aerial)
        length <$> handOf p `shouldReturn` 0
        discardOf p >>= (`shouldSatisfy` notElem aerial)

    it "can draw in alter-ego with no eligible discard" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        clearHand p
        industry <- putIntoPlay p "42009"
        choose "Worthington Industries" (isAbilityOfCard industry)
        length <$> handOf p `shouldReturn` 1

  describe "Techno-Organic Wings" do
    it "exhausts to ready Angel" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p A
        wings <- putIntoPlay p "42010"
        resolve [IdentityMessage p ExhaustedIdentity]
        choose "Wings" (isAbilityOfCard wings)
        isExhaustedNow p `shouldReturn` False
        upgradeIsExhausted (upgradeIdOf wings) `shouldReturn` True

    it "reduces only the next Aerial event and expires after that play" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p C
        wings <- putIntoPlay p "42010"
        choose "Wings" (isAbilityOfCard wings)
        placeMainThreat 4
        first <- putInHand p "42006"
        playWith first []
        chooseMainScheme
        pass
        clearHand p
        second <- putInHand p "42006"
        offers (isPlay second) `shouldReturn` False

    it "does not discount a non-Aerial event or consume the pending reduction" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p C
        wings <- putIntoPlay p "42010"
        choose "Wings" (isAbilityOfCard wings)
        nonAerial <- putInHand p "01087"
        offers (isPlay nonAerial) `shouldReturn` False
        energy <- putInHand p "01088"
        playWith nonAerial [energy]
        chooseVillain
        placeMainThreat 4
        flight <- putInHand p "42006"
        playWith flight []
        chooseMainScheme
        pass
        villainDamageNow `shouldReturn` 3

    it "expires at the end of the phase without being spent" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p C
        wings <- putIntoPlay p "42010"
        choose "Wings" (isAbilityOfCard wings)
        resolve [EndPhase PlayerPhase]
        placeMainThreat 4
        flight <- putInHand p "42006"
        offers (isPlay flight) `shouldReturn` False

    it "does not discount or spend itself on an Aerial event held by George Stacy" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        inForm p C
        wings <- putIntoPlay p "42010"
        george <- putIntoPlay p "27007"
        placeMainThreat 4
        held <- putInHand p "42006"
        choose "store the event" (isAbilityOfCard george)
        choose "Natural Flight" (isTargetChoice held)
        choose "Wings" (isAbilityOfCard wings)
        offers (isPlay held) `shouldReturn` False
        energy <- putInHand p "01088"
        playWith held [energy]
        chooseMainScheme
        pass
        clearHand p
        placeMainThreat 4
        flight <- putInHand p "42006"
        playWith flight []
        chooseMainScheme
        pass

    it "cannot be used as an alter-ego action" do
      tg <- angel
      let p = player1 tg
      inGame tg do
        wings <- putIntoPlay p "42010"
        offers (isAbilityOfCard wings) `shouldReturn` False
