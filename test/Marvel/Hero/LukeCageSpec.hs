module Marvel.Hero.LukeCageSpec (spec) where

import Marvel.Ally.Types qualified as Ally
import Marvel.Game.Source qualified as Engine
import Marvel.JessicaJones.Helpers qualified as JL
import Marvel.Keyword qualified as K
import Marvel.Modifier
import Marvel.Status
import TestImport

asLuke :: IdentityId -> GameAppT ()
asLuke p = changeForm p >> pass >> clearHand p

pay :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
pay p card codes = traverse (putInHand p) codes >>= playWith card

spec :: Spec
spec = describe "Luke Cage" do
  it "starts in alter ego with ten hit points and a six-card hand-size stat" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      hitPointsOf (player1 tg) `shouldReturn` 10
      handSizeOf (player1 tg) `shouldReturn` 6

  it "holds more than two tough status cards in alter ego" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      resolve $ replicate 4 (GiveStatus (IdentityRef p) ToughStatus)
      JL.toughCount p `shouldReturn` 4

  it "holds more than two tough status cards in hero form" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asLuke p
      resolve $ replicate 4 (GiveStatus (IdentityRef p) ToughStatus)
      JL.toughCount p `shouldReturn` 4

  it "takes exactly one unpreventable damage when one tough is discarded in hero form" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asLuke p
      resolve $ replicate 3 (GiveStatus (IdentityRef p) ToughStatus)
      resolve [RemoveStatus (IdentityRef p) ToughStatus]
      JL.toughCount p `shouldReturn` 2
      damageOf p `shouldReturn` 1

  it "does not take the hero's tough-discard damage in alter ego" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      resolve [GiveStatus (IdentityRef p) ToughStatus, RemoveStatus (IdentityRef p) ToughStatus]
      damageOf p `shouldReturn` 0

  it "unpreventable damage preserves every tough card" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      resolve $ replicate 3 (GiveStatus (IdentityRef p) ToughStatus)
      resolve (damageIdentity p (toSource p) $ withUnpreventable $ toDamage 2 FromAbility)
      damageOf p `shouldReturn` 2
      JL.toughCount p `shouldReturn` 3

  it "finds Burstein Process from the deck without paying for it" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      clearHand p
      found <- stackDeck p ["62008", "01088"]
      choose "Luke's search" (\case UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == 1; _ -> False)
      choose "Burstein Process" (isTargetChoice $ testHead found)
      pass
      map toCardCode <$> handOf p `shouldReturn` ["62008"]

  it "Burstein Process gives two tough cards when Luke has none" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      process <- putIntoPlay p "62008"
      choose "Burstein Process" (isAbilityOfCard process)
      JL.toughCount p `shouldReturn` 2
      upgradeIsExhausted (upgradeOf process) `shouldReturn` True

  it "Burstein Process gives only one tough card when Luke already has one" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      process <- putIntoPlay p "62008"
      resolve [GiveStatus (IdentityRef p) ToughStatus]
      choose "Burstein Process" (isAbilityOfCard process)
      JL.toughCount p `shouldReturn` 2

  it "Jessica Jones gains one THW per tough card, capped at four" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      _ <- putIntoPlay p "62002"
      resolve $ replicate 6 (GiveStatus (IdentityRef p) ToughStatus)
      a <- allyIn "62002"
      Ally.getModifiedThwart a `shouldReturn` 5

  it "Jessica Jones loses her bonus when Luke loses tough cards" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      _ <- putIntoPlay p "62002"
      resolve [GiveStatus (IdentityRef p) ToughStatus, RemoveStatus (IdentityRef p) ToughStatus]
      a <- allyIn "62002"
      Ally.getModifiedThwart a `shouldReturn` 1

  it "Knuckle Sandwich deals three damage without discarding tough" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asLuke p
      card <- putInHand p "62004"
      pay p card ["01088"]
      chooseVillain
      villainDamageNow `shouldReturn` 3
      discarded <- map toCardCode <$> discardOf p
      discarded `shouldSatisfy` elem "62004"

  it "Stand with Me gives tough before counting its threat removal" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asLuke p
      placeMainThreat 5
      before <- mainSchemeThreatNow
      card <- putInHand p "62005"
      pay p card ["01088"]
      chooseMainScheme
      JL.toughCount p `shouldReturn` 1
      mainSchemeThreatNow `shouldReturn` (before - 1)

  it "Stand with Me caps threat removal at five" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asLuke p
      resolve $ replicate 6 (GiveStatus (IdentityRef p) ToughStatus)
      current <- mainSchemeThreatNow
      scheme <- mainSchemeId
      resolve [MainSchemeMessage scheme $ MainSchemeThreatRemoved (toSource p) current]
      placeMainThreat 6
      before <- mainSchemeThreatNow
      card <- putInHand p "62005"
      pay p card ["01088"]
      chooseMainScheme
      mainSchemeThreatNow `shouldReturn` (before - 5)

  it "Sweet Christmas costs zero with five tough cards and hits the villain for five" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asLuke p
      resolve $ replicate 5 (GiveStatus (IdentityRef p) ToughStatus)
      card <- putInHand p "62006"
      playWith card []
      villainDamageNow `shouldReturn` 5

  it "Metal Bracer adds retaliate one" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asLuke p
      _ <- putIntoPlay p "62010"
      modifiers <- Engine.getModifiers p
      modifiers `shouldSatisfy` elem (KeywordModifier $ K.Retaliate 1)

  it "Metal Bracer reduces ordinary damage and exhausts" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asLuke p
      bracer <- putIntoPlay p "62010"
      start (damageIdentity p (toSource p) $ toDamage 3 FromAbility)
      choose "Metal Bracer" (isAbilityOfCard bracer)
      damageOf p `shouldReturn` 2
      upgradeIsExhausted (upgradeOf bracer) `shouldReturn` True

  it "Power Man draws a card after a tough discard and exhausts" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asLuke p
      power <- putIntoPlay p "62011"
      resolve [GiveStatus (IdentityRef p) ToughStatus]
      start [RemoveStatus (IdentityRef p) ToughStatus]
      choose "Unbreakable Skin" (\case UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == 1; _ -> False)
      choose "Power Man" (isAbilityOfCard power)
      length <$> handOf p `shouldReturn` 1
      upgradeIsExhausted (upgradeOf power) `shouldReturn` True

  it "Harlem's Hero gives tough before readying an exhausted character" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asLuke p
      resolve [IdentityMessage p ExhaustedIdentity]
      card <- putInHand p "62003"
      pay p card ["01088"]
      choose "ready Luke" (isTargetChoice $ IdentityCharacter p)
      JL.toughCount p `shouldReturn` 1
      isExhaustedPlayer p `shouldReturn` False

  it "Knuckle Sandwich returns from discard after voluntarily discarding tough" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asLuke p
      resolve [GiveStatus (IdentityRef p) ToughStatus]
      card <- putInHand p "62004"
      pay p card ["01088"]
      chooseVillain
      choose "return event" (isLabel "$label.knuckleSandwichReturn")
      choose "Unbreakable Skin" (\case UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == 1; _ -> False)
      JL.toughCount p `shouldReturn` 0
      damageOf p `shouldReturn` 1
      cards <- map toCardCode <$> handOf p
      cards `shouldSatisfy` elem "62004"

  it "Metal Bracer cannot prevent Luke's unpreventable tough-discard damage" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asLuke p
      _ <- putIntoPlay p "62010"
      resolve [GiveStatus (IdentityRef p) ToughStatus, RemoveStatus (IdentityRef p) ToughStatus]
      damageOf p `shouldReturn` 1

  it "Fogwell's Gym readies Luke after a tough card is discarded" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asLuke p
      gym <- putIntoPlay p "62007"
      resolve [IdentityMessage p ExhaustedIdentity, GiveStatus (IdentityRef p) ToughStatus]
      start [RemoveStatus (IdentityRef p) ToughStatus]
      choose "Unbreakable Skin" (\case UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == 1; _ -> False)
      choose "Fogwell's Gym" (isAbilityOfCard gym)
      isExhaustedPlayer p `shouldReturn` False
      support <- testHead <$> supportsWith "62007"
      supportExhausted support `shouldBe` True

  it "does not offer Fogwell's Gym when Luke is already ready" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      asLuke p
      gym <- putIntoPlay p "62007"
      resolve [GiveStatus (IdentityRef p) ToughStatus]
      start [RemoveStatus (IdentityRef p) ToughStatus]
      choose "Unbreakable Skin" (\case UseAbility ab -> abilitySource ab == IdentityRef p && abilityIndex ab == 1; _ -> False)
      offers (isAbilityOfCard gym) `shouldReturn` False

  it "does not offer Fogwell's Gym for a ready Defender even when another Defender is exhausted" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      gym <- putIntoPlay p "62007"
      ally <- putIntoPlay p "62013"
      resolve [IdentityMessage p ExhaustedIdentity]
      start [RemoveStatus (AllyRef $ allyIdOf ally) ToughStatus]
      offers (isAbilityOfCard gym) `shouldReturn` False

  it "Fogwell's Gym also readies a Defender ally that loses tough" do
    tg <- testGame ["luke-cage"]
    inGame tg do
      let p = player1 tg
      gym <- putIntoPlay p "62007"
      ally <- putIntoPlay p "62013"
      resolve [AllyMessage (allyIdOf ally) ExhaustedAlly]
      start [RemoveStatus (AllyRef $ allyIdOf ally) ToughStatus]
      choose "Fogwell's Gym" (isAbilityOfCard gym)
      a <- allyIn "62013"
      allyExhausted a `shouldBe` False
