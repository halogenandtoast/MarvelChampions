module Marvel.Hero.CaptainAmericaSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as V
import TestImport

-- | A card from hand paid for with the cards given, put into play.
inPlay :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
inPlay p code payment = do
  card <- putInHand p code
  cards <- traverse (putInHand p) payment
  playWith card cards
  pure card

-- | Turn the identity to a form, as changing form would, however often it has changed
-- this turn.
flipTo :: IdentityId -> Side -> GameAppT ()
flipTo p side = resolve [IdentityMessage p (ChangedToForm side)]

-- | The hero's basic attack on the villain.
attackVillain :: IdentityId -> GameAppT ()
attackVillain p = do
  vid <- villainIdNow
  choose "attack" (isHeroAttack p)
  choose "attack the villain" (isTargetChoice vid)
  pass

-- | Rhino attacks the player, his boost card giving him no more ATK.
rhinoAttacks :: IdentityId -> GameAppT ()
rhinoAttacks p = do
  _ <- stackEncounterDeck [Treacheries.imTough]
  vid <- villainIdNow
  runNow [VillainMessage vid (VillainAttacks p)]

spec :: Spec
spec = describe "Captain America" do
  it "Steve Rogers's setup adds Captain America's Shield to the starting hand" do
    tg <- testGame ["captain-america"]
    let p = player1 tg
    inGame tg do
      hand <- handOf p
      length hand `shouldBe` 7
      map toCardCode hand `shouldSatisfy` elem "03009"

  it "Steve Rogers's setup finds the shield in the discard pile" do
    tg <- testGame ["captain-america"]
    let p = player1 tg
    inGame tg do
      clearHand p
      withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) -> PlayerIdentity a {playerIdentityDeck = Deck []}
      _ <- stackDeck p ["03003", "03003"]
      [shield] <- stackDiscard p ["03009"]
      resolve [RanAbility p (IdentityRef p) 1 [] NoPayment]
      handOf p >>= (`shouldSatisfy` elem shield)
      discardOf p `shouldReturn` []

  describe "\"I Can Do This All Day!\"" do
    it "discards a card to ready Captain America, once a round" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        changeForm p
        card <- putInHand p "03003"
        _ <- putInHand p "03003"
        attackVillain p
        isExhaustedNow p `shouldReturn` True
        choose "ready" (isLabeledAbility "\"I Can Do This All Day!\"")
        choose "discard" (isTargetChoice card)
        isExhaustedNow p `shouldReturn` False
        discardOf p `shouldReturn` [card]
        attackVillain p
        offers (isLabeledAbility "\"I Can Do This All Day!\"") `shouldReturn` False

    it "needs Captain America exhausted and a card to discard, and is not Steve Rogers's" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        flipTo p A
        runNow [IdentityMessage p ExhaustedIdentity]
        offers (isLabeledAbility "\"I Can Do This All Day!\"") `shouldReturn` False
        _ <- putInHand p "03003"
        offers (isLabeledAbility "\"I Can Do This All Day!\"") `shouldReturn` True
        runNow [IdentityMessage p ReadiedIdentity]
        offers (isLabeledAbility "\"I Can Do This All Day!\"") `shouldReturn` False
        flipTo p B
        offers (isLabeledAbility "\"I Can Do This All Day!\"") `shouldReturn` False

  describe "Living Legend" do
    it "makes the first ally of the round cost 1 less, and only the first" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        -- Agent 13 costs 3: paid with 2 cards
        agent13 <- putInHand p "03002"
        payment <- traverse (putInHand p) ["03003", "03003"]
        playWith agent13 payment
        pass
        alliesWith "03002" >>= (`shouldSatisfy` ((== 1) . length))
        handOf p `shouldReturn` []
        -- Squirrel Girl costs 2: 1 card does not pay for her now
        squirrel <- putInHand p "03013"
        _ <- putInHand p "03003"
        offers (isPlay squirrel) `shouldReturn` False

    it "reduces another player's ally when theirs is the round's first" do
      tg <- testGame ["spider-man", "captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        -- Spider-Man, with no resource of Peter Parker's to pay with
        changeForm p
        -- Squirrel Girl costs 2: 1 card pays for her as the round's first ally
        squirrel <- putInHand p "03013"
        payment <- putInHand p "03003"
        playWith squirrel [payment]
        pass
        alliesWith "03013" >>= (`shouldSatisfy` ((== 1) . length))
        -- the discount is spent: a second ally costs full
        wonder <- putInHand p "03014"
        _ <- putInHand p "03003"
        offers (isPlay wonder) `shouldReturn` False

    it "works again next round" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        squirrel <- putInHand p "03013"
        payment <- traverse (putInHand p) ["03003", "03003"]
        changeForm p
        playWith squirrel payment
        pass
        flipTo p B
        runNow [EndRound]
        wonder <- putInHand p "03014"
        _ <- putInHand p "03003"
        offers (isPlay wonder) `shouldReturn` True

    it "does not apply once an ally has been played as Captain America" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        changeForm p
        squirrel <- putInHand p "03013"
        payment <- traverse (putInHand p) ["03003", "03003"]
        playWith squirrel payment
        pass
        flipTo p B
        wonder <- putInHand p "03014"
        _ <- putInHand p "03003"
        offers (isPlay wonder) `shouldReturn` False

    it "is Steve Rogers's, not Captain America's" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        changeForm p
        squirrel <- putInHand p "03013"
        _ <- putInHand p "03003"
        offers (isPlay squirrel) `shouldReturn` False

  it "Agent 13 removes 2 threat from a scheme as she enters play" do
    tg <- testGame ["captain-america"]
    let p = player1 tg
    inGame tg do
      clearHand p
      placeMainThreat 3
      before <- mainSchemeThreat
      _ <- inPlay p "03002" ["03003", "03003"]
      choose "use Agent 13" isAnyAbility
      pass
      mainSchemeThreat `shouldReturn` (before - 2)

  describe "Fearless Determination" do
    it "gives Captain America +1 THW for the phase and draws a card" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        changeForm p
        placeMainThreat 5
        before <- mainSchemeThreat
        _ <- inPlay p "03003" []
        length <$> handOf p `shouldReturn` 1
        ms <- mainSchemeId
        choose "thwart" (isHeroThwart p)
        choose "thwart the main scheme" (isTargetChoice ms)
        mainSchemeThreat `shouldReturn` (before - 3)

    it "is a hero action" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putInHand p "03003"
        offers (isPlay card) `shouldReturn` False

  describe "Heroic Strike" do
    it "deals 6 damage, and stuns when paid with a physical resource" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        changeForm p
        _ <- inPlay p "03004" ["01090", "03010"]
        vid <- villainIdNow
        choose "strike Rhino" (isTargetChoice (EnemyVillainId vid))
        villainDamageNow `shouldReturn` 6
        field V.VillainStunned <$> getVillain `shouldReturn` True

    it "is answered as an attack once all of it has resolved: the stun comes first" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        changeForm p
        jarnbjorn <- putIntoPlay p "06019"
        _ <- putInHand p "01090"
        _ <- inPlay p "03004" ["01090", "03010"]
        vid <- villainIdNow
        choose "strike Rhino" (isTargetChoice (EnemyVillainId vid))
        offers (isAbilityOfCard jarnbjorn) `shouldReturn` True
        field V.VillainStunned <$> getVillain `shouldReturn` True

    it "does not stun when paid otherwise" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        changeForm p
        _ <- inPlay p "03004" ["01088", "03003"]
        vid <- villainIdNow
        choose "strike Rhino" (isTargetChoice (EnemyVillainId vid))
        villainDamageNow `shouldReturn` 6
        field V.VillainStunned <$> getVillain `shouldReturn` False

  describe "Shield Block" do
    it "exhausts the shield to prevent all of the damage" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        changeForm p
        shield <- putIntoPlay p "03009"
        block <- putInHand p "03005"
        runNow (damageIdentity p GameRef (toDamage 5 FromAbility))
        choose "block" (isPlay block)
        pass
        damageOf p `shouldReturn` 0
        upgradeIsExhausted (upgradeIdOf shield) `shouldReturn` True

    it "is not played without a ready shield" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        changeForm p
        shield <- putIntoPlay p "03009"
        resolve [UpgradeMessage (upgradeIdOf shield) ExhaustedUpgrade]
        block <- putInHand p "03005"
        runNow (damageIdentity p GameRef (toDamage 5 FromAbility))
        offers (isPlay block) `shouldReturn` False
        pass
        damageOf p `shouldReturn` 5

  describe "Shield Toss" do
    it "discards X cards and returns the shield to hand to deal 4 damage to X enemies" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        changeForm p
        shield <- putIntoPlay p "03009"
        minion <- addMinion p Minions.madameHydra
        pass
        toss <- putInHand p "03006"
        [a, b] <- traverse (putInHand p) ["03003", "03003"]
        playWith toss []
        choose "discard" (isTargetChoice a)
        choose "discard" (isTargetChoice b)
        handOf p >>= (`shouldSatisfy` elem shield)
        upgradesOf p `shouldReturn` []
        vid <- villainIdNow
        choose "toss at Rhino" (isTargetChoice (EnemyVillainId vid))
        choose "toss at the minion" (isTargetChoice (EnemyMinionId minion))
        villainDamageNow `shouldReturn` 4
        minionDamageOf minion `shouldReturn` Just 4
        discardOf p >>= (`shouldSatisfy` \d -> a `elem` d && b `elem` d)

    it "a stunned hero still discards the cards and returns the shield: the stun replaces the attack" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        changeForm p
        shield <- putIntoPlay p "03009"
        resolve [IdentityMessage p IdentityStunned]
        toss <- putInHand p "03006"
        a <- putInHand p "03003"
        playWith toss []
        choose "discard" (isTargetChoice a)
        isStunned p `shouldReturn` False
        handOf p >>= (`shouldSatisfy` elem shield)
        discardOf p >>= (`shouldSatisfy` elem a)
        villainDamageNow `shouldReturn` 0

    it "needs the shield in play" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        changeForm p
        toss <- putInHand p "03006"
        _ <- putInHand p "01088"
        offers (isPlay toss) `shouldReturn` False

  describe "Steve's Apartment" do
    it "draws a card and heals 1 damage from Steve Rogers" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        setDamage p 3
        apartment <- putIntoPlay p "03007"
        choose "use the apartment" (isAbilityOfCard apartment)
        damageOf p `shouldReturn` 2
        length <$> handOf p `shouldReturn` 1

    it "is an alter-ego action" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        apartment <- putIntoPlay p "03007"
        changeForm p
        offers (isAbilityOfCard apartment) `shouldReturn` False

  describe "Captain America's Helmet" do
    it "sets the hit point dial to 1 instead of Captain America being defeated, then is discarded" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        changeForm p
        helmet <- putIntoPlay p "03008"
        setDamage p 9
        runNow (damageIdentity p GameRef (toDamage 5 FromAbility))
        choose "use the helmet" (isAbilityOfCard helmet)
        damageOf p `shouldReturn` 10
        hitPointsOf p `shouldReturn` 11
        upgradesOf p `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` elem helmet)
        getsGame (HashMap.member p . gamePlayers) `shouldReturn` True

    it "does not save Steve Rogers" do
      tg <- testGame ["captain-america", "spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        helmet <- putIntoPlay p "03008"
        setDamage p 9
        runNow (damageIdentity p GameRef (toDamage 5 FromAbility))
        offers (isAbilityOfCard helmet) `shouldReturn` False

  describe "Captain America's Shield" do
    it "gives Captain America +1 DEF and retaliate 1" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        changeForm p
        _ <- putIntoPlay p "03009"
        rhinoAttacks p
        choose "defend" isDefend
        pass
        -- Rhino's 2 ATK against 3 DEF
        damageOf p `shouldReturn` 0
        villainDamageNow `shouldReturn` 1

    it "gives Steve Rogers nothing" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "03009"
        rhinoAttacks p
        pass
        damageOf p `shouldReturn` 2
        villainDamageNow `shouldReturn` 0

    it "is restricted: with a third restricted card in play, one is discarded" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        [a, _] <- traverse (putIntoPlay p) ["03009", "03009"]
        length <$> upgradesOf p `shouldReturn` 2
        c <- putInHand p "03009"
        runNow [PutCardIntoPlay p c NoPayment Nothing]
        choose "discard a restricted card" (isTargetChoice (upgradeIdOf a))
        length <$> upgradesOf p `shouldReturn` 2
        discardOf p >>= (`shouldSatisfy` elem a)

  it "Super-Soldier Serum exhausts to pay a physical resource" do
    tg <- testGame ["captain-america"]
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      serum <- putIntoPlay p "03010"
      helmet <- putInHand p "03008"
      choose "play the helmet" (isPlay helmet)
      choose "pay with the serum" (isAbilityOfCard serum)
      chooseIf isFinishPayment
      upgradeIsExhausted (upgradeIdOf serum) `shouldReturn` True
      upgradeCodes >>= (`shouldSatisfy` elem "03008")

  describe "Man Out of Time" do
    it "exhausting Steve Rogers removes it from the game" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Obligations.manOutOfTime
        choose "exhaust Steve Rogers" (isLabel "$label.exhaustSteveRogersRemoveManOutOfTime")
        isExhaustedNow p `shouldReturn` True
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "03026")
        obligationsInPlay `shouldReturn` 0

    it "Captain America may flip to Steve Rogers, then exhaust him" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        flipTo p A
        _ <- revealNow p Obligations.manOutOfTime
        choose "flip to Steve Rogers" (isLabel "$label.changeToB")
        choose "exhaust Steve Rogers" (isLabel "$label.exhaustSteveRogersRemoveManOutOfTime")
        isExhaustedNow p `shouldReturn` True
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "03026")
        -- the flip was the card's: the player may still change form this turn
        offers (\case UseAbility ab -> ChangeForm `elem` abilityChoices ab; _ -> False) `shouldReturn` True

    it "otherwise discards half the hand, rounded down" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        changeForm p
        [a, b, _, _, _] <- traverse (putInHand p) ["03003", "03003", "03004", "03004", "03004"]
        _ <- revealNow p Obligations.manOutOfTime
        choose "stay Captain America" (isLabel "$label.doNotFlipToAlterEgo")
        -- a hero cannot exhaust Steve Rogers: the only choice is made for them
        choose "discard" (isTargetChoice a)
        choose "discard" (isTargetChoice b)
        length <$> handOf p `shouldReturn` 3
        encounterDiscardCodes >>= (`shouldSatisfy` elem "03026")

  it "the starter deck is 40 cards" do
    sum (map snd (starter "captain-america").cards) `shouldBe` (40 :: Int) :: IO ()
