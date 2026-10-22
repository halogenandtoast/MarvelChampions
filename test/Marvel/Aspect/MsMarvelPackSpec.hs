module Marvel.Aspect.MsMarvelPackSpec (spec) where

import Marvel.Minion.Cards qualified as Minions
import Marvel.Villain.Types qualified as V
import Data.Text qualified as T
import TestImport

msMarvel :: IO TestGame
msMarvel = testGame ["ms-marvel"]

-- | Play a card from hand paying with new copies of the cards given.
playPaying :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
playPaying p card codes = do
  payment <- traverse (putInHand p) codes
  playWith card payment

-- | The villain attacks the player, who does not defend.
villainAttacks :: IdentityId -> GameAppT ()
villainAttacks p = do
  vid <- villainIdNow
  start [VillainMessage vid (VillainAttacks p)]

villainField :: Field Villain a -> GameAppT a
villainField f = field f <$> getVillain

spec :: Spec
spec = describe "Ms. Marvel pack" do
  describe "Nova (05012)" do
    it "spends an energy resource as an enemy attacks you to deal it 2 damage" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        nova <- putIntoPlay p "05012"
        energy <- putInHand p "01088"
        villainAttacks p
        choose "use Nova" (isAbilityOfCard nova)
        choose "pay" (isPayWith energy)
        chooseIf isFinishPayment
        villainDamageNow `shouldReturn` 2

    it "does not answer an attack against another player" do
      tg <- testGame ["ms-marvel", "black-panther"]
      inGame tg do
        [p1, p2] <- players
        clearHand p1
        nova <- putIntoPlay p1 "05012"
        _ <- putInHand p1 "01088"
        villainAttacks p2
        offers (isAbilityOfCard nova) `shouldReturn` False

  describe "Preemptive Strike (05014)" do
    it "cancels the boost icons of a boost card turned up for the villain's attack, 1 damage each" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        strike <- putInHand p "05014"
        energy <- putInHand p "01088"
        _ <- stackEncounterDeck [Minions.vulture]
        villainAttacks p
        choose "no defender" isNoDefender
        choose "play Preemptive Strike" (isPlay strike)
        choose "pay" (isPayWith energy)
        chooseIf isFinishPayment
        pass
        villainDamageNow `shouldReturn` 2
        -- Rhino's 2 ATK, without the 2 boost
        damageOf p `shouldReturn` 2
      -- the log tells the card turned up, but no boost added
      text <- T.concat . map entryText <$> readIORef tg.logged
      text `shouldSatisfy` T.isInfixOf "$log.boost"
      text `shouldSatisfy` (not . T.isInfixOf "$log.boostAdded")

    it "defeating the villain's stage mid-attack, the attack goes on at the next stage's ATK" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        strike <- putInHand p "05014"
        energy <- putInHand p "01088"
        vid <- villainIdNow
        resolve [VillainMessage vid (VillainDamaged (toSource p) (toDamage 13 FromAbility))]
        _ <- stackEncounterDeck [Minions.vulture]
        villainAttacks p
        choose "no defender" isNoDefender
        choose "play Preemptive Strike" (isPlay strike)
        choose "pay" (isPayWith energy)
        chooseIf isFinishPayment
        pass
        choose "Rhino II's When Revealed" (isAbilityOf (VillainRef vid))
        pass
        villainField V.VillainStage `shouldReturn` 2
        -- Rhino II's 3 ATK, without the 2 boost
        damageOf p `shouldReturn` 3

    it "defeating the villain mid-attack: the next stage resumes the attack with its own ATK" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        strike <- putInHand p "05014"
        energy <- putInHand p "01088"
        vid <- villainIdNow
        runNow [VillainMessage vid (VillainDamaged (toSource p) (toDamage 12 FromAbility))]
        _ <- stackEncounterDeck [Minions.vulture]
        villainAttacks p
        choose "no defender" isNoDefender
        choose "play Preemptive Strike" (isPlay strike)
        choose "pay" (isPayWith energy)
        chooseIf isFinishPayment
        pass
        choose "Rhino II's Breakin' & Takin'" isAnyAbility
        pass
        -- Rhino II's 3 ATK, the boost cancelled
        damageOf p `shouldReturn` 3
        villainDamageNow `shouldReturn` 0

    it "is not played as the villain schemes" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        strike <- putInHand p "05014"
        _ <- putInHand p "01088"
        vid <- villainIdNow
        start [VillainMessage vid VillainSchemes]
        offers (isPlay strike) `shouldReturn` False

  describe "Tackle (05015)" do
    it "stuns an enemy, and deals it 3 damage when paid with a physical resource" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        tackle <- putInHand p "05015"
        playPaying p tackle ["01090", "01089"]
        vid <- villainIdNow
        choose "tackle Rhino" (isTargetChoice (EnemyVillainId vid))
        villainField V.VillainStunned `shouldReturn` True
        villainDamageNow `shouldReturn` 3

    it "only stuns without a physical resource" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        tackle <- putInHand p "05015"
        playPaying p tackle ["01088", "01089"]
        vid <- villainIdNow
        choose "tackle Rhino" (isTargetChoice (EnemyVillainId vid))
        villainField V.VillainStunned `shouldReturn` True
        villainDamageNow `shouldReturn` 0

  describe "Energy Barrier (05017)" do
    it "removes a counter as you would take damage to prevent 1 and deal 1 to an enemy" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        barrier <- putIntoPlay p "05017"
        vid <- villainIdNow
        start (damageIdentity p (toSource vid) (toDamage 3 FromAbility))
        choose "use Energy Barrier" (isAbilityOfCard barrier)
        pass
        damageOf p `shouldReturn` 2
        villainDamageNow `shouldReturn` 1
        map (.upgradeUses) <$> upgradesWith "05017" `shouldReturn` [2]

    it "is discarded with its last counter" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        barrier <- putIntoPlay p "05017"
        vid <- villainIdNow
        for_ [1 :: Int .. 3] \_ -> do
          start (damageIdentity p (toSource vid) (toDamage 1 FromAbility))
          choose "use Energy Barrier" (isAbilityOfCard barrier)
          pass
        upgradesWith "05017" `shouldReturn` []
        damageOf p `shouldReturn` 0
        filter (== "05017") . map toCardCode <$> discardOf p `shouldReturn` ["05017"]

  describe "Lockjaw (05018)" do
    it "may be played from the discard pile" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        clearHand p
        [lockjaw] <- stackDiscard p ["05018"]
        playPaying p lockjaw ["01088", "01089"]
        alliesWith "05018" >>= (`shouldSatisfy` ((== 1) . length))
        discardOf p >>= (`shouldSatisfy` notElem lockjaw)

    it "is left in the discard pile when its cost cannot be paid after all" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        clearHand p
        [lockjaw] <- stackDiscard p ["05018"]
        resolve [IdentityMessage p (PayForCard lockjaw Nothing Nothing)]
        discardOf p >>= (`shouldSatisfy` elem lockjaw)
        handOf p `shouldReturn` []
        alliesWith "05018" `shouldReturn` []

  describe "Endurance (05023)" do
    it "gives +3 hit points, max 1 per player" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        clearHand p
        before <- hitPointsOf p
        endurance <- putInHand p "05023"
        second <- putInHand p "05023"
        playPaying p endurance ["01088"]
        hitPointsOf p `shouldReturn` (before + 3)
        _ <- putInHand p "01089"
        offers (isPlay second) `shouldReturn` False

  describe "Enhanced Reflexes (05024)" do
    it "exhausts and spends an energy counter for an energy resource, as a hero" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        clearHand p
        reflexes <- putIntoPlay p "05024"
        aamir <- putInHand p "05006"
        offers (isPlay aamir) `shouldReturn` False
        changeForm p
        boost <- putInHand p "05032"
        choose "play Morale Boost" (isPlay boost)
        choose "pay with reflexes" (isAbilityOfCard reflexes)
        chooseIf isFinishPayment
        upgradeIsExhausted (upgradeIdOf reflexes) `shouldReturn` True
        map (.upgradeUses) <$> upgradesWith "05024" `shouldReturn` [2]

  describe "Melee (05030)" do
    it "deals 3 damage to an enemy and 3 to another, never the same one twice" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        melee <- putInHand p "05030"
        playPaying p melee ["01089", "01090"]
        vid <- villainIdNow
        choose "hit Rhino" (isTargetChoice (EnemyVillainId vid))
        -- no other enemy: nothing more is asked
        villainDamageNow `shouldReturn` 3

  describe "Concussive Blow (05031)" do
    it "confuses an enemy, and deals it 3 damage when paid with a physical resource" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        blow <- putInHand p "05031"
        playPaying p blow ["01090", "01089"]
        vid <- villainIdNow
        choose "blow to Rhino" (isTargetChoice (EnemyVillainId vid))
        villainField V.VillainConfused `shouldReturn` True
        villainDamageNow `shouldReturn` 3

  describe "Morale Boost (05032)" do
    it "gives a hero +1 THW, ATK and DEF until the end of the round" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        boost <- putInHand p "05032"
        playPaying p boost ["01088"]
        choose "attack" (isHeroAttack p)
        vid <- villainIdNow
        choose "hit Rhino" (isTargetChoice vid)
        villainDamageNow `shouldReturn` 2
        getsGame (length . gameEffects) `shouldReturn` 1
        resolve [EndRound]
        getsGame (length . gameEffects) `shouldReturn` 0

  describe "Down Time (05033)" do
    it "gives your alter-ego +2 REC" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "05033"
        setDamage p 9
        choose "recover" (\case UseAbility ab -> Recover `elem` abilityChoices ab; _ -> False)
        damageOf p `shouldReturn` 2
