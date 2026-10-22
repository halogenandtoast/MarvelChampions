module Marvel.Aspect.ProtectionSpec (spec) where

import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Villain.Types (villainIsTough)
import TestImport hiding (isDefend)

isDefend :: Choice -> Bool
isDefend = \case
  Defend _ -> True
  _ -> False

-- | To the villain's attack on the player in the villain phase.
toVillainAttack :: (HasCallStack) => IdentityId -> GameAppT ()
toVillainAttack _ = do
  endTurn
  untilDefend (20 :: Int)
 where
  untilDefend 0 = error "the villain did not attack"
  untilDefend n = whenM (not <$> offers isDefend) do
    choose "decline" (isLabel "$label.noResponse")
    untilDefend (n - 1)

spec :: Spec
spec = describe "Protection" do
  describe "Black Widow (01075)" do
    it "exhausts and spends a mental resource to cancel and discard a revealed encounter card, then reveals another" do
      tg <- testGame ["black-panther"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01075"
        widow <- allyIn "01075"
        genius <- putInHand p "01089"
        card <- encounterCard Cards.imTough
        deckBefore <- length <$> scenarioEncounterDeckNow
        runNow [RevealEncounterCard p card]
        choose "use Black Widow" (isAbilityOf (allyId widow))
        choose "pay" (isPayWith genius)
        chooseIf isFinishPayment
        resolve []
        villainIsTough <$> getVillain `shouldReturn` False
        discards <- encounterDiscardNow
        discards `shouldSatisfy` elem card
        -- another card is revealed in its place
        length <$> scenarioEncounterDeckNow >>= (`shouldSatisfy` (< deckBefore))
        (.allyExhausted) <$> allyIn "01075" `shouldReturn` True

    it "does not answer a card revealed from the encounter discard pile" do
      tg <- testGame ["black-panther"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01075"
        _ <- putInHand p "01089"
        (_, [mercenary]) <- setEncounterDeck [Cards.advance] [Minions.hydraMercenary]
        runNow [SearchForAndRevealEncounterCard p (CardWithType MinionType)]
        -- not stopped to be offered Black Widow: the mercenary is in play
        (.allyExhausted) <$> allyIn "01075" `shouldReturn` False
        minionDamageOf (minionOf mercenary) `shouldReturn` Just 0

  describe "Luke Cage (01076)" do
    it "enters play with a tough status card" do
      tg <- testGame ["black-panther"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01076"
        luke <- allyIn "01076"
        luke.allyTough `shouldBe` True
        resolve [AllyMessage (allyId luke) (AllyDamaged (toSource p) (toDamage 3 FromAbility))]
        luke' <- allyIn "01076"
        luke'.allyDamage `shouldBe` 0
        luke'.allyTough `shouldBe` False

  describe "Counter-Punch (01077)" do
    it "deals damage equal to the hero's ATK after the hero defends" do
      tg <- testGame ["black-panther"]
      let p = player1 tg
      inGame tg do
        clearHand p
        counter <- putInHand p "01077"
        changeForm p
        toVillainAttack p
        start <- villainDamageNow
        choose "defend" isDefend
        -- retaliate is forced, so it comes before Counter-Punch is offered
        villainDamageNow `shouldReturn` start + 1
        choose "play Counter-Punch" (isPlay counter)
        -- Black Panther has 2 ATK
        villainDamageNow `shouldReturn` start + 1 + 2

  describe "Get Behind Me! (01078)" do
    it "cancels a treachery's when revealed effects and the villain attacks you instead" do
      tg <- testGame ["black-panther"]
      let p = player1 tg
      inGame tg do
        clearHand p
        changeForm p
        pass
        behind <- putInHand p "01078"
        genius <- putInHand p "01089"
        card <- encounterCard Cards.imTough
        runNow [RevealEncounterCard p card]
        choose "play Get Behind Me!" (isPlay behind)
        choose "pay" (isPayWith genius)
        chooseIf isFinishPayment
        -- Rhino attacks
        choose "take the attack" (isLabel "$label.noDefender")
        pass
        villainIsTough <$> getVillain `shouldReturn` False
        damageOf p >>= (`shouldSatisfy` (> 0))

  describe "The Power of Protection (01079)" do
    it "pays 2 for a Protection card" do
      tg <- testGame ["black-panther"]
      let p = player1 tg
      inGame tg do
        clearHand p
        team <- putInHand p "01080"
        power <- putInHand p "01079"
        energy <- putInHand p "01088"
        offers (isPlay team) `shouldReturn` True
        playWith team [power]
        supportsWith "01080" `shouldReturn` []
        choose "pay" (isPayWith energy)
        chooseIf isFinishPayment
        supportsWith "01080" >>= (`shouldSatisfy` ((== 1) . length))

  describe "Med Team (01080)" do
    it "has 3 uses, each exhausting it to heal 2 damage from a friendly character" do
      tg <- testGame ["black-panther"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01080"
        [team] <- supportsWith "01080"
        team.supportUses `shouldBe` 3
        offers (isAbilityOf (supportId team)) `shouldReturn` False
        setDamage p 7
        for_ [1 :: Natural .. 3] \n -> do
          resolve [SupportMessage (supportId team) ReadiedSupport]
          choose "use Med Team" (isAbilityOf (supportId team))
          chooseIf (isTargetChoice (IdentityCharacter p))
          damageOf p `shouldReturn` (7 - 2 * n)
        supportsWith "01080" `shouldReturn` []

  describe "Armored Vest (01081)" do
    it "gives the hero +1 DEF, max 1 per player" do
      let
        defended vested = do
          tg <- testGame ["black-panther"]
          let p = player1 tg
          inGame tg do
            clearHand p
            when vested do
              vest <- putInHand p "01081"
              vest2 <- putInHand p "01081"
              genius <- putInHand p "01089"
              playWith vest [genius]
              offers (isPlay vest2) `shouldReturn` False
            changeForm p
            toVillainAttack p
            choose "defend" isDefend
            pass
            damageOf p
      -- the same boost card is dealt to Rhino either way
      without <- defended False
      with <- defended True
      without `shouldSatisfy` (> 0)
      with `shouldBe` without - 1

  describe "Indomitable (01082)" do
    it "is discarded after the hero defends to ready the hero" do
      tg <- testGame ["black-panther"]
      let p = player1 tg
      inGame tg do
        clearHand p
        putIntoPlay p "01082"
        [indomitable] <- upgradesWith "01082"
        changeForm p
        toVillainAttack p
        choose "defend" isDefend
        choose "use Indomitable" (isAbilityOf (upgradeId indomitable))
        exhaustedNow p `shouldReturn` False
        upgradesWith "01082" `shouldReturn` []
        map toCardCode <$> discardOf p >>= (`shouldSatisfy` elem "01082")
