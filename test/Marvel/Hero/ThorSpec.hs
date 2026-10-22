module Marvel.Hero.ThorSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

thor :: IO TestGame
thor = testGame ["thor"]

-- | Play a card from hand paying with new copies of the cards given.
playPaying :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
playPaying p card codes = do
  payment <- traverse (putInHand p) codes
  playWith card payment

-- | A card put into play from hand, paid for with new resource cards.
inPlay :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
inPlay p code codes = do
  card <- putInHand p code
  playPaying p card codes
  pure card

haveAtThee :: Choice -> Bool
haveAtThee = isNamedAbility "\"Have at thee!\""

codes :: [PlayerCard] -> [CardCode]
codes = map toCardCode

-- | Thor, with Mjolnir in play.
withMjolnir :: IdentityId -> GameAppT PlayerCard
withMjolnir p = do
  changeForm p
  inPlay p "06009" ["01088"]

spec :: Spec
spec = describe "Thor" do
  describe "Thor (06001a)" do
    it "Have at thee!: after he engages a minion, draw 2 cards, once a phase" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        _ <- revealNow p Minions.hydraMercenary
        choose "have at thee" haveAtThee
        pass
        length <$> handOf p `shouldReturn` 2
        _ <- revealNow p Minions.hydraMercenary
        offers haveAtThee `shouldReturn` False

    it "Have at thee! is Thor's, not Odinson's" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Minions.hydraMercenary
        offers haveAtThee `shouldReturn` False

  describe "Odinson (06001b)" do
    it "Worthy: finds Mjolnir in the discard pile, once a round" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        clearHand p
        [mjolnir] <- stackDiscard p ["06009"]
        choose "worthy" (isNamedAbility "Worthy")
        choose "take Mjolnir" (isTargetChoice mjolnir)
        codes <$> handOf p `shouldReturn` ["06009"]
        codes <$> discardOf p >>= (`shouldSatisfy` notElem "06009")
        offers (isNamedAbility "Worthy") `shouldReturn` False

  describe "Lady Sif (06002)" do
    it "readies Thor as she enters play" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        resolve [IdentityMessage p ExhaustedIdentity]
        sif <- putInHand p "06002"
        playPaying p sif ["01088", "01089", "01090"]
        choose "ready Thor" (isAbilityOfCard sif)
        exhaustedNow p `shouldReturn` False

  describe "Defender of the Nine Realms (06003)" do
    it "puts the first minion of the encounter deck into play engaged with him, then removes 3 threat" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        placeMainThreat 5
        before <- mainSchemeThreat
        [_, mercenary] <- stackEncounterDeck [Treacheries.advance, Minions.hydraMercenary]
        defender <- putInHand p "06003"
        choose "play" (isPlay defender)
        pass
        pass
        mainSchemeThreat `shouldReturn` (before - 3)
        engaged <- (.playerIdentityMinions) <$> playerAttrs p
        toList engaged `shouldBe` [minionOf mercenary]
        map toCardCode <$> encounterDiscardNow >>= (`shouldSatisfy` elem (toCardCode Treacheries.advance))

  describe "For Asgard! (06004)" do
    it "is an alter-ego action that finds an Asgard card in the deck or discard pile" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        clearHand p
        [asgard] <- stackDiscard p ["06007"]
        forAsgard <- putInHand p "06004"
        playPaying p forAsgard ["01088"]
        choose "take Asgard" (isTargetChoice asgard)
        codes <$> handOf p >>= (`shouldSatisfy` elem "06007")
        changeForm p
        another <- putInHand p "06004"
        _ <- putInHand p "01088"
        offers (isPlay another) `shouldReturn` False

  describe "Hammer Throw (06005)" do
    it "exhausts Mjolnir, deals 8 with overkill, and returns Mjolnir to hand" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        clearHand p
        mjolnir <- withMjolnir p
        mercenary <- addMinion p Minions.hydraMercenary
        throw <- putInHand p "06005"
        playPaying p throw ["01088", "01089", "01090"]
        choose "throw at the mercenary" (isTargetChoice mercenary)
        pass
        minionDamageOf mercenary `shouldReturn` Nothing
        villainDamageNow `shouldReturn` 5
        map pcCardId <$> handOf p >>= (`shouldSatisfy` elem (pcCardId mjolnir))
        upgradeCodes `shouldReturn` []

    it "exhausts Mjolnir even as a stun replaces the attack, Mjolnir staying in play" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        clearHand p
        mjolnir <- withMjolnir p
        resolve [IdentityMessage p IdentityStunned]
        throw <- putInHand p "06005"
        playPaying p throw ["01088", "01089", "01090"]
        pass
        isStunned p `shouldReturn` False
        upgradeIsExhausted (upgradeOf mjolnir) `shouldReturn` True
        villainDamageNow `shouldReturn` 0
        upgradeCodes `shouldReturn` ["06009"]

    it "needs Mjolnir in play and ready" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        throw <- putInHand p "06005"
        _ <- traverse (putInHand p) ["01088", "01089", "01090"]
        offers (isPlay throw) `shouldReturn` False

  describe "Lightning Strike (06006)" do
    it "spends X energy for X damage to the villain and each minion engaged with him, past tough with Mjolnir" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- withMjolnir p
        sandman <- addMinion p Minions.sandman
        pass
        strike <- putInHand p "06006"
        genius <- putInHand p "01089"
        energy <- putInHand p "01088"
        -- X is paid with the card's cost: the mental resource pays for the card, the
        -- energy resources are X
        choose "play" (isPlay strike)
        choose "pay with a mental card" (isPayWith genius)
        offers isFinishPayment `shouldReturn` False
        choose "spend energy" (isPayWith energy)
        pass
        villainDamageNow `shouldReturn` 2
        minionDamageOf sandman `shouldReturn` Just 2
        tough <- getsGame (fmap (.minionTough) . fmap toAttrs . HashMap.lookup sandman . gameMinions)
        tough `shouldBe` Just True

    it "without the Aerial trait, a tough status card prevents it" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        sandman <- addMinion p Minions.sandman
        pass
        strike <- putInHand p "06006"
        energy <- putInHand p "01088"
        playPaying p strike ["01089"]
        choose "spend energy" (isPayWith energy)
        whenM (offers isFinishPayment) $ choose "done" isFinishPayment
        pass
        minionDamageOf sandman `shouldReturn` Just 0

    it "takes what pays for its own cost out of X" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        strike <- putInHand p "06006"
        energy <- traverse (putInHand p) ["01088", "01088"]
        playWith strike energy
        pass
        villainDamageNow `shouldReturn` 3

  describe "Asgard (06007)" do
    it "gives +1 hand size" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        clearHand p
        before <- handSizeOf p
        _ <- inPlay p "06007" ["01088", "01089", "01090"]
        handSizeOf p `shouldReturn` (before + 1)

  describe "God of Thunder (06008)" do
    it "exhausts to pay an energy resource, in hero form" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        god <- inPlay p "06008" ["01088"]
        mjolnir <- putInHand p "06009"
        choose "play Mjolnir" (isPlay mjolnir)
        choose "pay with God of Thunder" (isAbilityOfCard god)
        upgradeIsExhausted (upgradeOf god) `shouldReturn` True
        upgradeCodes >>= (`shouldSatisfy` elem "06009")

  describe "Mjolnir (06009)" do
    it "gives Thor +1 ATK" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- withMjolnir p
        clearHand p
        choose "attack" (isHeroAttack p)
        vid <- villainIdNow
        choose "attack Rhino" (isTargetChoice vid)
        pass
        villainDamageNow `shouldReturn` 3

    it "is restricted: a third restricted upgrade discards one" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        _ <- inPlay p "06009" ["01088"]
        _ <- inPlay p "06019" ["01090"]
        shield <- putInHand p "03009"
        playPaying p shield ["01089"]
        choose "discard the shield" (isTargetChoice (upgradeOf shield))
        upgradeCodes >>= (`shouldSatisfy` \cs -> "06009" `elem` cs && "06019" `elem` cs && "03009" `notElem` cs)

  describe "Thor's Helmet (06010)" do
    it "gives +5 hit points" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        clearHand p
        before <- hitPointsOf p
        _ <- inPlay p "06010" ["01088", "01089"]
        hitPointsOf p `shouldReturn` (before + 5)

  describe "Odin's Anger (06026)" do
    it "exhausting Odinson removes it from the game" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Obligations.odinsAnger
        choose "exhaust Odinson" (isLabel "$label.exhaustOdinsonRemoveOdinsAnger")
        exhaustedNow p `shouldReturn` True
        getsGame (HashMap.null . gameObligations) `shouldReturn` True

    it "otherwise Mjolnir is discarded from play and he is stunned" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        _ <- withMjolnir p
        anger <- revealNow p Obligations.odinsAnger
        -- Thor is not Odinson, so this is all there is
        choose "stay a hero" (isLabel "$label.doNotFlipToAlterEgo")
        isStunned p `shouldReturn` True
        upgradeCodes `shouldReturn` []
        codes <$> discardOf p >>= (`shouldSatisfy` elem "06009")
        (map ecCardId <$> encounterDiscard) >>= (`shouldSatisfy` elem (ecCardId anger))

    it "discards Mjolnir from hand" do
      tg <- thor
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putInHand p "06009"
        resolve [IdentityMessage p ExhaustedIdentity]
        -- Odinson is exhausted, so this is all there is
        _ <- revealNow p Obligations.odinsAnger
        handOf p `shouldReturn` []
        codes <$> discardOf p >>= (`shouldSatisfy` elem "06009")
        isStunned p `shouldReturn` True
