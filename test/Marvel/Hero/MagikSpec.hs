module Marvel.Hero.MagikSpec (spec) where

import Marvel.Hp (HP (..))
import Marvel.Keyword
import Marvel.Modifier
import Marvel.Phase
import Marvel.Message.Lifted qualified as L
import TestImport

perform :: L.QueueT GameAppT () -> GameAppT ()
perform action = L.runQueueT (L.capture action) >>= runNow

spec :: Spec
spec = describe "Magik" do
  it "starts as Illyana with ten hit points and six cards, changing to a five-card hero" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      hitPointsOf p `shouldReturn` 10
      handSizeOf p `shouldReturn` 6
      changeForm p
      handSizeOf p `shouldReturn` 5

  it "retrieves a Spell before changing to hero form" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      [spell, other] <- stackDiscard p ["45039", "45037"]
      changeForm p
      choose "Illyana's spell interrupt" (isAbilityOf p)
      offers (isTargetChoice other) `shouldReturn` False
      choose "put the Spell on top" (isTargetChoice spell)
      pass
      headMay . unDeck . playerIdentityDeck <$> playerAttrs p `shouldReturn` Just spell
      discardOf p >>= (`shouldSatisfy` notElem spell)

  it "does not offer Illyana's interrupt when no Spell is in the discard pile" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      _ <- stackDiscard p ["45037"]
      changeForm p
      pass
      handSizeOf p `shouldReturn` 5

  it "Limbo swaps actual hand and top cards and exhausts" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      limbo <- putIntoPlay p "45032"
      clearHand p
      handCard <- putInHand p "01086"
      [top] <- stackDeck p ["01087"]
      choose "use Limbo" (isAbilityOfCard limbo)
      choose "swap the hand card" (isTargetChoice handCard)
      handOf p `shouldReturn` [top]
      headMay . unDeck . playerIdentityDeck <$> playerAttrs p `shouldReturn` Just handCard
      offers (isAbilityOfCard limbo) `shouldReturn` False

  it "Limbo also offers its swap after the villain phase begins" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      limbo <- putIntoPlay p "45032"
      clearHand p
      handCard <- putInHand p "01086"
      [top] <- stackDeck p ["01087"]
      runNow [BeginPhase PlayerPhase]
      choose "Limbo's phase response" (isAbilityOfCard limbo)
      choose "swap at phase start" (isTargetChoice handCard)
      pass
      handOf p `shouldReturn` [top]

  for_ [("45033", Mental, ThwartModifier 1, KeywordModifier Steady), ("45034", Physical, AttackModifier 1, BasicAttackKeyword Piercing), ("45035", Energy, DefenseModifier 1, KeywordModifier (Retaliate 1))] \(code, resource, stat, keyword) -> do
    it ("Magik equipment " <> show code <> " grants its constant and matching top-card bonus") do
      tg <- testGame ["magik"]
      inGame tg do
        let p = player1 tg
        changeForm p
        _ <- putIntoPlay p code
        let resourceCode = case resource of Mental -> "01089"; Physical -> "01090"; _ -> "01088"
        _ <- stackDeck p [resourceCode]
        getModifiers p >>= (`shouldSatisfy` (\ms -> stat `elem` ms && keyword `elem` ms))
        _ <- stackDeck p ["45031"]
        getModifiers p >>= (`shouldSatisfy` elem stat)
        let mismatch = if resource == Mental then "01088" else "01089"
        _ <- stackDeck p [mismatch]
        getModifiers p >>= (`shouldSatisfy` (\ms -> stat `notElem` ms && keyword `elem` ms))
        perform (L.changeToAlterEgo p)
        pass
        getModifiers p >>= (`shouldSatisfy` (\ms -> stat `notElem` ms && keyword `notElem` ms))

  it "Colossus enters play with tough and printed two-point basic powers" do
    tg <- testGame ["magik"]
    inGame tg do
      colossus <- putIntoPlay (player1 tg) "45031"
      attrs <- allyIn "45031"
      allyTough attrs `shouldBe` True
      allyExhausted attrs `shouldBe` False
      allyHitPoints attrs `shouldBe` HP 3
      toCardCode colossus `shouldBe` "45031"

  it "pays to play Colossus during an attack and declares him without exhausting" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      changeForm p
      clearHand p
      colossus <- putInHand p "45031"
      payments <- traverse (putInHand p) ["01088", "01086"]
      v <- villainIdNow
      runNow [VillainMessage v (VillainAttacks p)]
      choose "Colossus's paid interrupt" (isPlay colossus)
      payWith payments
      pass
      damageOf p `shouldReturn` 0
      attrs <- allyIn "45031"
      allyExhausted attrs `shouldBe` False
      allyDamage attrs `shouldBe` 0
      allyTough attrs `shouldBe` False

  it "borrowed equipment still benefits Magik and reads its controller's deck" do
    tg <- testGame ["magik", "spider-man"]
    inGame tg do
      let magik = player1 tg
          borrower = player2 tg
      changeForm magik
      _ <- putIntoPlay borrower "45033"
      _ <- stackDeck magik ["01088"]
      _ <- stackDeck borrower ["01089"]
      getModifiers magik >>= (`shouldSatisfy` (\ms -> KeywordModifier Steady `elem` ms && ThwartModifier 1 `elem` ms))
      getModifiers borrower >>= (`shouldSatisfy` notElem (KeywordModifier Steady))

  it "Illyana's Spell interrupt is once per phase and renews in the next phase" do
    tg <- testGame ["magik"]
    inGame tg do
      let p = player1 tg
      [first, second] <- stackDiscard p ["45039", "45038"]
      changeForm p
      choose "first Spell interrupt" (isAbilityOf p)
      choose "first Spell" (isTargetChoice first)
      pass
      perform (L.changeToAlterEgo p)
      pass
      perform (L.changeToHeroForm p)
      pass
      discardOf p >>= (`shouldSatisfy` elem second)
      headMay . unDeck . playerIdentityDeck <$> playerAttrs p `shouldReturn` Just first
      perform (L.changeToAlterEgo p)
      pass
      runNow [BeginPhase PlayerPhase]
      pass
      perform (L.changeToHeroForm p)
      choose "new phase's Spell interrupt" (isAbilityOf p)
      choose "second Spell" (isTargetChoice second)
      pass
      headMay . unDeck . playerIdentityDeck <$> playerAttrs p `shouldReturn` Just second
