module Marvel.Hero.GamoraSignatureSpec (spec) where

import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Obligation.Cards qualified as Obligations
import TestImport

gamora :: IO TestGame
gamora = testGame ["gamora"]

-- | Gamora in hero form, her hand empty.
asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

sideOf :: IdentityId -> GameAppT Side
sideOf p = (.playerIdentitySide) <$> playerAttrs p

-- | Play the card paying with new copies of the cards given, damage on the villain if it
-- is asked for, threat off the main scheme if asked for, then decline Gamora's Finesse and
-- Precision.
playEvent :: IdentityId -> CardCode -> [CardCode] -> GameAppT ()
playEvent p code payment = do
  card <- putInHand p code
  cards <- traverse (putInHand p) payment
  playWith card cards
  vid <- villainIdNow
  chooseIf (isTargetChoice vid)
  m <- mainSchemeIdNow
  chooseIf (isTargetChoice m)
  pass

-- | End the turn and decline everything until the player's next turn. The encounter
-- deck is only Advance, so the villain phase stuns, confuses, toughens and guards no one.
toNextTurn :: (HasCallStack) => GameAppT ()
toNextTurn = do
  _ <- setEncounterDeck (replicate 6 Treacheries.advance) []
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

exhaustAlterEgo, discardEvents :: Choice -> Bool
exhaustAlterEgo = isLabel "$label.exhaustGamoraRemoveUnfulfilledDestiny"
discardEvents = isLabelStarting "$label.discardEventsFromHand"

spec :: Spec
spec = describe "Gamora signature cards" do
  describe "Acrobatic Move (18003)" do
    it "deals 2 damage to an enemy" do
      tg <- gamora
      let p = player1 tg
      inGame tg do
        asHero p
        playEvent p "18003" []
        villainDamageNow `shouldReturn` 2

    it "is a hero action" do
      tg <- gamora
      let p = player1 tg
      inGame tg do
        clearHand p
        move <- putInHand p "18003"
        offers (isPlay move) `shouldReturn` False

  describe "Crosscounter (18004)" do
    it "prevents 3 damage, deals 1 damage to an enemy and removes 1 threat" do
      tg <- gamora
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 2
        before <- mainSchemeThreatNow
        cc <- putInHand p "18004"
        energy <- putInHand p "01088"
        vid <- villainIdNow
        start (damageIdentity p (toSource vid) (toDamage 5 FromAbility))
        playWith cc [energy]
        chooseIf (isTargetChoice vid)
        mainSchemeIdNow >>= chooseIf . isTargetChoice
        pass
        damageOf p `shouldReturn` 2
        villainDamageNow `shouldReturn` 1
        mainSchemeThreatNow `shouldReturn` (before - 1)

    it "is not offered in alter-ego form" do
      tg <- gamora
      let p = player1 tg
      inGame tg do
        clearHand p
        cc <- putInHand p "18004"
        _ <- putInHand p "01088"
        vid <- villainIdNow
        start (damageIdentity p (toSource vid) (toDamage 5 FromAbility))
        offers (isPlay cc) `shouldReturn` False

    -- RR Labeled Ability: a stun or confusion cancels the whole event, each removed
    it "stunned and confused, all of it is canceled and both are removed" do
      tg <- gamora
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 2
        before <- mainSchemeThreatNow
        resolve [IdentityMessage p IdentityStunned, IdentityMessage p IdentityConfused]
        cc <- putInHand p "18004"
        energy <- putInHand p "01088"
        vid <- villainIdNow
        start (damageIdentity p (toSource vid) (toDamage 5 FromAbility))
        playWith cc [energy]
        pass
        damageOf p `shouldReturn` 5
        villainDamageNow `shouldReturn` 0
        mainSchemeThreatNow `shouldReturn` before
        isStunned p `shouldReturn` False
        isConfused p `shouldReturn` False

    it "confused only, all of it is canceled, the prevention too" do
      tg <- gamora
      let p = player1 tg
      inGame tg do
        asHero p
        resolve [IdentityMessage p IdentityConfused]
        cc <- putInHand p "18004"
        energy <- putInHand p "01088"
        vid <- villainIdNow
        start (damageIdentity p (toSource vid) (toDamage 5 FromAbility))
        playWith cc [energy]
        pass
        damageOf p `shouldReturn` 5
        villainDamageNow `shouldReturn` 0
        isConfused p `shouldReturn` False

  describe "Set the Pace (18005)" do
    it "removes 1 threat from a scheme" do
      tg <- gamora
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 2
        before <- mainSchemeThreatNow
        playEvent p "18005" []
        mainSchemeThreatNow `shouldReturn` (before - 1)

  describe "Decisive Blow (18006)" do
    it "deals 4 damage to an enemy" do
      tg <- gamora
      let p = player1 tg
      inGame tg do
        asHero p
        playEvent p "18006" ["01090"]
        villainDamageNow `shouldReturn` 4

    it "deals 7 damage after you played a thwart event this turn" do
      tg <- gamora
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 2
        playEvent p "18005" []
        playEvent p "18006" ["01090"]
        villainDamageNow `shouldReturn` 7

    it "an attack event this turn does not count" do
      tg <- gamora
      let p = player1 tg
      inGame tg do
        asHero p
        playEvent p "18003" []
        playEvent p "18006" ["01090"]
        villainDamageNow `shouldReturn` 6

    it "a thwart event of an earlier turn does not count" do
      tg <- gamora
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 2
        playEvent p "18005" []
        toNextTurn
        before <- villainDamageNow
        playEvent p "18006" ["01090"]
        villainDamageNow `shouldReturn` (before + 4)

  describe "Forward Momentum (18007)" do
    it "removes 3 threat from a scheme" do
      tg <- gamora
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 5
        before <- mainSchemeThreatNow
        playEvent p "18007" ["01089"]
        mainSchemeThreatNow `shouldReturn` (before - 3)

    it "removes 5 threat after you played an attack event this turn" do
      tg <- gamora
      let p = player1 tg
      inGame tg do
        asHero p
        placeMainThreat 5
        before <- mainSchemeThreatNow
        playEvent p "18003" []
        playEvent p "18007" ["01089"]
        mainSchemeThreatNow `shouldReturn` (before - 5)

    it "an attack event of an earlier turn does not count" do
      tg <- gamora
      let p = player1 tg
      inGame tg do
        asHero p
        playEvent p "18003" []
        toNextTurn
        now <- mainSchemeThreatNow
        when (now < 3) $ placeMainThreat (3 - now)
        before <- mainSchemeThreatNow
        playEvent p "18007" ["01089"]
        mainSchemeThreatNow `shouldReturn` (before - 3)

  describe "Unfulfilled Destiny (18024)" do
    it "is given to the Gamora player" do
      tg <- testGame ["she-hulk", "gamora"]
      let p1 = player1 tg
          p2 = player2 tg
      inGame tg do
        _ <- revealNow p1 Obligations.unfulfilledDestiny
        fmap fst <$> asked `shouldReturn` Just p2

    it "exhausting the alter-ego removes it from the game" do
      tg <- gamora
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Obligations.unfulfilledDestiny
        offers discardEvents `shouldReturn` True
        choose "exhaust Gamora" exhaustAlterEgo
        isExhaustedNow p `shouldReturn` True
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "18024")

    it "a hero may flip to alter-ego to exhaust it" do
      tg <- gamora
      let p = player1 tg
      inGame tg do
        changeForm p >> pass
        _ <- revealNow p Obligations.unfulfilledDestiny
        choose "flip to alter-ego" (isLabel "$label.changeToB")
        sideOf p `shouldReturn` B
        choose "exhaust Gamora" exhaustAlterEgo
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` notElem "18024")

    it "an exhausted alter-ego cannot be exhausted for it" do
      tg <- gamora
      let p = player1 tg
      inGame tg do
        clearHand p
        resolve [IdentityMessage p ExhaustedIdentity]
        _ <- revealNow p Obligations.unfulfilledDestiny
        offers exhaustAlterEgo `shouldReturn` False

    it "otherwise discards 2 chosen events from hand, and is discarded" do
      tg <- gamora
      let p = player1 tg
      inGame tg do
        asHero p
        e1 <- putInHand p "18003"
        e2 <- putInHand p "18005"
        e3 <- putInHand p "18006"
        energy <- putInHand p "01088"
        _ <- revealNow p Obligations.unfulfilledDestiny
        chooseIf (isLabel "$label.doNotFlipToAlterEgo")
        chooseIf discardEvents
        choose "the first event" (isTargetChoice e1)
        offers (isTargetChoice e1) `shouldReturn` False
        offers (isTargetChoice energy) `shouldReturn` False
        choose "the second event" (isTargetChoice e3)
        pass
        handOf p >>= (`shouldMatchList` [e2, energy])
        discardOf p >>= (`shouldSatisfy` \d -> e1 `elem` d && e3 `elem` d)
        isExhaustedNow p `shouldReturn` False
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` elem "18024")

    -- ruling: the second option resolves as far as it can
    it "with fewer than 2 events, discards those there are, and is discarded" do
      tg <- gamora
      let p = player1 tg
      inGame tg do
        asHero p
        e1 <- putInHand p "18003"
        energy <- putInHand p "01088"
        _ <- revealNow p Obligations.unfulfilledDestiny
        chooseIf (isLabel "$label.doNotFlipToAlterEgo")
        chooseIf discardEvents
        pass
        handOf p `shouldReturn` [energy]
        discardOf p >>= (`shouldSatisfy` elem e1)
        obligationsInPlay `shouldReturn` 0
        encounterDiscardCodes >>= (`shouldSatisfy` elem "18024")
