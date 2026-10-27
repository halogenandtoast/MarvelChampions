module Marvel.Hero.StarLordSpec (spec) where

import Marvel.Ally.Types qualified as Ally
import Marvel.Hero.Runner (getModifiedThwart)
import Marvel.Identity (PlayerIdentitySide (..), currentIdentity)
import Marvel.Matchers (AllyMatcher (..), IdentityMatcher (..))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Payment.Types (Payment (NoPayment))
import Marvel.Query (selectAny)
import Marvel.Trait (Trait (Aerial, Guardian))
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as V
import TestImport

starLordGame :: IO TestGame
starLordGame = testGame ["star-lord"]

-- | Star-Lord in hero form, his hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

isWhatCouldGoWrong :: Choice -> Bool
isWhatCouldGoWrong = isNamedAbility "\"What could go wrong?\""

isSmoothTalker :: Choice -> Bool
isSmoothTalker = isNamedAbility "Smooth Talker"

codesOf :: [PlayerCard] -> [CardCode]
codesOf = map (toCardCode . getCardDef)

-- | Decline responses until a matching choice is offered.
declineUntil :: (HasCallStack) => (Choice -> Bool) -> GameAppT ()
declineUntil f = do
  o <- offers f
  unless o $ choose "decline" isNoResponse >> declineUntil f

-- | Decline every response, failing if a matching choice is ever offered.
neverOffered :: (HasCallStack) => (Choice -> Bool) -> GameAppT ()
neverOffered f = do
  offers f `shouldReturn` False
  whenM (offers isNoResponse) $ choose "decline" isNoResponse >> neverOffered f

-- | Rhino attacks the player, who does not defend; Stampede (1 boost icon) is the boost.
rhinoAttacks :: IdentityId -> GameAppT ()
rhinoAttacks p = do
  _ <- stackEncounterDeck [Treacheries.stampede]
  vid <- villainIdNow
  runNow [VillainMessage vid (VillainAttacks p)]
  choose "no defender" isNoDefender

heroThwart :: IdentityId -> GameAppT Natural
heroThwart p =
  getPlayer p >>= \i -> case currentIdentity i of
    HeroSide h -> getModifiedThwart (toAttrs h)
    AlterEgoSide _ -> error "Star-Lord is in alter-ego form"

isGuardianAlly :: PlayerCard -> GameAppT Bool
isGuardianAlly c = selectAny (AllyWithId (allyIdOf c) <> AllyWithTrait Guardian)

spec :: Spec
spec = describe "Star-Lord" do
  it "deals the pack's own deck of 40 cards" do
    sum (map snd (starter "star-lord").cards) `shouldBe` (40 :: Int) :: IO ()

  describe "Star-Lord (17001a): allies gain Guardian" do
    it "gives each ally he controls the Guardian trait in hero form only" do
      tg <- starLordGame
      let p = player1 tg
      inGame tg do
        bill <- putIntoPlay p "17012"
        isGuardianAlly bill `shouldReturn` False
        asHero p
        isGuardianAlly bill `shouldReturn` True

  describe "Star-Lord (17001a): \"What could go wrong?\"" do
    it "plays a 4-cost card for 1 resource, dealing him a facedown encounter card" do
      tg <- starLordGame
      let p = player1 tg
      inGame tg do
        asHero p
        mansion <- putInHand p "01091"
        strength <- putInHand p "01090"
        dealt <- length <$> dealtTo p
        -- affordable only through the interrupt
        offers (isPlay mansion) `shouldReturn` True
        choose "play Avengers Mansion" (isPlay mansion)
        choose "What could go wrong?" isWhatCouldGoWrong
        payWith [strength]
        pass
        supportsWith "01091" >>= (`shouldSatisfy` (not . null))
        length <$> dealtTo p `shouldReturn` dealt + 1
        -- the reduction was for that card only
        null <$> getsGame gameEffects `shouldReturn` True

    it "is not offered for a card that costs nothing" do
      tg <- starLordGame
      let p = player1 tg
      inGame tg do
        asHero p
        escape <- putInHand p "17003"
        choose "play Daring Escape" (isPlay escape)
        offers isWhatCouldGoWrong `shouldReturn` False

    it "is limited to once a round" do
      tg <- starLordGame
      let p = player1 tg
      inGame tg do
        asHero p
        helmet <- putInHand p "17010"
        choose "play the Helmet" (isPlay helmet)
        choose "What could go wrong?" isWhatCouldGoWrong
        payWith []
        pass
        -- spent: a card he cannot pay for is no longer playable, nor is it offered again
        mansion <- putInHand p "01091"
        strength <- putInHand p "01090"
        offers (isPlay mansion) `shouldReturn` False
        boots <- putInHand p "17008"
        strength' <- putInHand p "01090"
        choose "play Jet Boots" (isPlay boots)
        offers isWhatCouldGoWrong `shouldReturn` False
        payWith [strength, strength']

    it "is not offered in alter-ego form" do
      tg <- starLordGame
      let p = player1 tg
      inGame tg do
        clearHand p
        mansion <- putInHand p "01091"
        _ <- putInHand p "01090"
        offers (isPlay mansion) `shouldReturn` False
        helmet <- putInHand p "17010"
        choose "play the Helmet" (isPlay helmet)
        offers isWhatCouldGoWrong `shouldReturn` False

  describe "Peter Quill (17001b)" do
    it "begins the game with an Element Gun in hand" do
      tg <- starLordGame
      let p = player1 tg
      inGame tg do
        codesOf <$> handOf p >>= (`shouldSatisfy` elem "17007")

    it "searches the discard pile for an Element Gun too" do
      tg <- starLordGame
      let p = player1 tg
      inGame tg do
        clearHand p
        withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
          PlayerIdentity a {playerIdentityDeck = Deck (filter ((/= "17007") . toCardCode . getCardDef) (unDeck a.playerIdentityDeck))}
        gun <- putInDiscard p "17007"
        resolve [RanAbility p (IdentityRef p) 1 [] NoPayment]
        handOf p `shouldReturn` [gun]
        discardOf p `shouldReturn` []

    it "Smooth Talker swaps a card in hand with the top card of his deck, once a round" do
      tg <- starLordGame
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putInHand p "01090"
        tops <- stackDeck p ["01089"]
        choose "Smooth Talker" isSmoothTalker
        choose "the card" (isTargetChoice card)
        handOf p `shouldReturn` tops
        headMay . unDeck . (.playerIdentityDeck) <$> playerAttrs p `shouldReturn` Just card
        offers isSmoothTalker `shouldReturn` False

    it "Smooth Talker is not offered with an empty hand" do
      tg <- starLordGame
      let p = player1 tg
      inGame tg do
        clearHand p
        offers isSmoothTalker `shouldReturn` False
        _ <- putInHand p "01090"
        offers isSmoothTalker `shouldReturn` True

  describe "Bad Boy (17006)" do
    it "prevents all damage of the villain's attack, changes to alter-ego and draws 2" do
      tg <- starLordGame
      let p = player1 tg
      inGame tg do
        asHero p
        badBoy <- putIntoPlay p "17006"
        rhinoAttacks p
        declineUntil (isAbilityOfCard badBoy)
        choose "Bad Boy" (isAbilityOfCard badBoy)
        pass
        damageOf p `shouldReturn` 0
        (.playerIdentitySide) <$> playerAttrs p `shouldReturn` B
        length <$> handOf p `shouldReturn` 2
        supportsWith "17006" `shouldReturn` []
        codesOf <$> discardOf p >>= (`shouldSatisfy` elem "17006")

    it "does not answer a minion's attack" do
      tg <- starLordGame
      let p = player1 tg
      inGame tg do
        asHero p
        badBoy <- putIntoPlay p "17006"
        mid <- addMinion p Minions.hydraMercenary
        pass
        runNow [MinionMessage mid (MinionAttacks p)]
        choose "no defender" isNoDefender
        neverOffered (isAbilityOfCard badBoy)
        damageOf p >>= (`shouldSatisfy` (> 0))
        length <$> supportsWith "17006" `shouldReturn` 1

  describe "Element Gun (17007)" do
    it "exhausts and spends a resource to deal 3 piercing damage" do
      tg <- starLordGame
      let p = player1 tg
      inGame tg do
        asHero p
        gun <- putIntoPlay p "17007"
        strength <- putInHand p "01090"
        vid <- villainIdNow
        resolve [VillainMessage vid VillainBecomeTough]
        choose "Element Gun" (isAbilityOfCard gun)
        payWith [strength]
        chooseVillain
        pass
        V.villainIsTough <$> getVillain `shouldReturn` False
        villainDamageNow `shouldReturn` 3
        upgradeIsExhausted (upgradeOf gun) `shouldReturn` True

  describe "Jet Boots (17008)" do
    it "makes the hero Aerial, not Peter Quill" do
      tg <- starLordGame
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "17008"
        selectAny (IdentityWithId p <> IdentityWithTrait Aerial) `shouldReturn` False
        asHero p
        selectAny (IdentityWithId p <> IdentityWithTrait Aerial) `shouldReturn` True

    it "prevents 1 damage for each facedown encounter card in front of him" do
      tg <- starLordGame
      let p = player1 tg
      inGame tg do
        asHero p
        boots <- putIntoPlay p "17008"
        resolve [DealEncounterCard p, DealEncounterCard p]
        rhinoAttacks p
        declineUntil (isAbilityOfCard boots)
        choose "Jet Boots" (isAbilityOfCard boots)
        pass
        -- Rhino's 2 ATK and Stampede's 1, 2 prevented
        damageOf p `shouldReturn` 1
        upgradeIsExhausted (upgradeOf boots) `shouldReturn` True

    it "is not offered in alter-ego form" do
      tg <- starLordGame
      let p = player1 tg
      inGame tg do
        boots <- putIntoPlay p "17008"
        resolve [DealEncounterCard p]
        runNow (damageIdentity p (IdentityRef p) (toDamage 2 FromAbility))
        neverOffered (isAbilityOfCard boots)
        damageOf p `shouldReturn` 2

  describe "Leader of the Guardians (17009)" do
    it "gives +1 THW to Star-Lord and to his Guardian allies" do
      tg <- starLordGame
      let p = player1 tg
      inGame tg do
        asHero p
        bill <- putIntoPlay p "17012"
        _ <- putIntoPlay p "17009"
        heroThwart p `shouldReturn` 3
        -- Beta Ray Bill is a Guardian through Star-Lord: printed THW 1
        (Ally.getModifiedThwart =<< allyIn "17012") `shouldReturn` 2
        isGuardianAlly bill `shouldReturn` True

    it "gives nothing to Peter Quill's allies, who are no Guardians" do
      tg <- starLordGame
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "17012"
        _ <- putIntoPlay p "17009"
        (Ally.getModifiedThwart =<< allyIn "17012") `shouldReturn` 1

  describe "Star-Lord's Helmet (17010)" do
    it "gives +1 hand size per facedown encounter card in hero form, at most +3" do
      tg <- starLordGame
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "17010"
        dealtTo p `shouldReturn` []
        resolve [DealEncounterCard p]
        -- Peter Quill's 6: not in hero form
        handSizeOf p `shouldReturn` 6
        asHero p
        handSizeOf p `shouldReturn` 6
        resolve (replicate 3 (DealEncounterCard p))
        handSizeOf p `shouldReturn` 8
