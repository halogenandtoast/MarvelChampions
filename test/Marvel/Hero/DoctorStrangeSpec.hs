module Marvel.Hero.DoctorStrangeSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.List (elemIndex, sortOn)
import Data.Typeable (cast)
import Marvel.Hero.Heroes.DoctorStrange (DoctorStrange (..), InvocationDeck (..), invocationDeckOf)
import Marvel.Hero.Types (Hero (..))
import Marvel.Invocation
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Hero.Runner (getModifiedThwart)
import Marvel.Identity (currentIdentity)
import Marvel.Villain.Types qualified as V
import TestImport

doctorStrange :: IO TestGame
doctorStrange = testGame ["doctor-strange"]

invocationOf :: IdentityId -> GameAppT InvocationDeck
invocationOf p = fromMaybe (error "no Invocation deck") . invocationDeckOf <$> getPlayer p

topInvocation :: IdentityId -> GameAppT CardCode
topInvocation p = maybe (error "empty Invocation deck") toCardCode . headMay . invocationDeck <$> invocationOf p

-- | Put these Invocation cards on top of the Invocation deck, the first on top.
stackInvocation :: IdentityId -> [CardCode] -> GameAppT ()
stackInvocation p codes = withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
  PlayerIdentity a {playerIdentitySides = fmap restack (playerIdentitySides a)}
 where
  restack = \case
    HeroSide (Hero h) | Just (DoctorStrange (attrs `With` d)) <- cast h ->
      HeroSide . Hero . DoctorStrange $ attrs `With` d {invocationDeck = sortOn rank (invocationDeck d)}
    side -> side
  rank c = fromMaybe (length codes) (elemIndex (toCardCode c) codes)

-- | Play a card from hand paying with new copies of the cards given.
playPaying :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
playPaying p card codes = do
  payment <- traverse (putInHand p) codes
  playWith card payment

isVillainChoice :: Choice -> Bool
isVillainChoice = \case
  TargetLabel (VillainRef _) _ -> True
  TargetLabel (EnemyRef (EnemyVillainId _)) _ -> True
  _ -> False

spellMastery :: Choice -> Bool
spellMastery = isNamedAbility "Spell Mastery"

spec :: Spec
spec = describe "Doctor Strange" do
  describe "the Invocation deck" do
    it "begins the game with the five Invocation cards, none of them in his player deck" do
      tg <- doctorStrange
      let p = player1 tg
      inGame tg do
        d <- invocationOf p
        sortOn id (map toCardCode d.invocationDeck) `shouldBe` ["09032", "09033", "09034", "09035", "09036"]
        d.invocationDiscard `shouldBe` []
        a <- playerAttrs p
        let codes = map toCardCode (unDeck a.playerIdentityDeck <> unHand a.playerIdentityHand)
        filter (`elem` ["09032", "09033", "09034", "09035", "09036"]) codes `shouldBe` []

    it "is made again from its discard pile once it is empty" do
      tg <- doctorStrange
      let p = player1 tg
      inGame tg do
        resolve $ replicate 4 (invocation p DiscardTopInvocation)
        d <- invocationOf p
        length d.invocationDeck `shouldBe` 1
        length d.invocationDiscard `shouldBe` 4
        resolve [invocation p DiscardTopInvocation]
        d' <- invocationOf p
        length d'.invocationDeck `shouldBe` 5
        d'.invocationDiscard `shouldBe` []

  it "Natural Talent discards the top card of the Invocation deck, once a turn, as Stephen Strange" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      top <- topInvocation p
      choose "Natural Talent" (isNamedAbility "Natural Talent")
      d <- invocationOf p
      map toCardCode d.invocationDiscard `shouldBe` [top]
      offers (isNamedAbility "Natural Talent") `shouldReturn` False
      changeForm p
      offers (isNamedAbility "Natural Talent") `shouldReturn` False

  it "Spell Mastery exhausts him and pays the top card's cost to resolve its Special, then discards it" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      stackInvocation p ["09032"]
      offers spellMastery `shouldReturn` False
      energy <- putInHand p "01088"
      choose "Spell Mastery" spellMastery
      choose "pay" (isPayWith energy)
      chooseIf isFinishPayment
      choose "Rhino" isVillainChoice
      villainDamageNow `shouldReturn` 7
      (V.villainStunned . toAttrs <$> getVillain) `shouldReturn` True
      isExhaustedNow p `shouldReturn` True
      d <- invocationOf p
      map toCardCode d.invocationDiscard `shouldBe` ["09032"]
      -- in the Invocation deck's discard pile, not his own
      map toCardCode <$> discardOf p >>= (`shouldSatisfy` notElem "09032")

  it "Spell Mastery is Doctor Strange's alone: Stephen Strange cannot use it" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      stackInvocation p ["09036"]
      offers spellMastery `shouldReturn` False

  it "Winds of Watoomb costs nothing and draws 3 cards" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      stackInvocation p ["09036"]
      choose "Spell Mastery" spellMastery
      length <$> handOf p `shouldReturn` 3

  it "Images of Ikonn confuses the villain and removes 4 threat from a scheme" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      placeMainThreat 6
      threat <- mainSchemeThreatNow
      stackInvocation p ["09033"]
      genius <- putInHand p "01089"
      choose "Spell Mastery" spellMastery
      choose "pay" (isPayWith genius)
      chooseIf isFinishPayment
      chooseIf \case TargetLabel {} -> True; _ -> False
      mainSchemeThreatNow `shouldReturn` (threat - 4)
      (V.villainConfused . toAttrs <$> getVillain) `shouldReturn` True

  it "Seven Rings of Raggadorr gives up to 3 characters a tough status card" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      stackInvocation p ["09034"]
      genius <- putInHand p "01089"
      choose "Spell Mastery" spellMastery
      choose "pay" (isPayWith genius)
      chooseIf isFinishPayment
      choose "Doctor Strange" (isTargetChoice p)
      -- he has one now, so is not offered again
      offers (isTargetChoice p) `shouldReturn` False
      choose "done" (isLabel "$label.done")
      (.playerIdentityTough) <$> playerAttrs p `shouldReturn` True
      -- a tough hero takes no damage, and loses the status instead
      resolve (damageIdentity p (toSource p) (toDamage 3 FromAbility))
      damageOf p `shouldReturn` 0
      (.playerIdentityTough) <$> playerAttrs p `shouldReturn` False

  it "Vapors of Valtorr replaces a status card with one of another type" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      vid <- villainIdNow
      resolve [GiveStatus (VillainRef vid) StunnedStatus]
      stackInvocation p ["09035"]
      choose "Spell Mastery" spellMastery
      choose "Rhino" isVillainChoice
      chooseIf (isLabel "$label.stunnedStatus")
      choose "confused" (isLabel "$label.confusedStatus")
      v <- toAttrs <$> getVillain
      (v.villainStunned, v.villainConfused) `shouldBe` (False, True)

  it "Master of the Mystic Arts pays the top card's printed cost too, resolves it, and puts it back on top" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      stackInvocation p ["09032"]
      master <- putInHand p "09005"
      -- 1 for the card and 2 for Crimson Bands
      _ <- putInHand p "01089"
      offers (isPlay master) `shouldReturn` False
      _ <- putInHand p "01088"
      playPaying p master []
      choose "pay genius" \case PayWithCard c -> toCardCode c == "01089"; _ -> False
      choose "pay energy" \case PayWithCard c -> toCardCode c == "01088"; _ -> False
      chooseIf isFinishPayment
      choose "Rhino" isVillainChoice
      villainDamageNow `shouldReturn` 7
      topInvocation p `shouldReturn` "09032"
      (.invocationDiscard) <$> invocationOf p `shouldReturn` []
      -- he was not exhausted: that is Spell Mastery's cost
      isExhaustedNow p `shouldReturn` False

  it "Wong heals 1 damage from your identity or discards the top Invocation card" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      _ <- putIntoPlay p "09002"
      wong <- allyIn "09002"
      top <- topInvocation p
      choose "Wong" (isAbilityOf (allyId wong))
      choose "discard" (isLabel "$label.discardTopInvocation")
      map toCardCode . (.invocationDiscard) <$> invocationOf p `shouldReturn` [top]

  it "Magic Blast deals 5 damage, and more as the card it discards says" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      -- Strength is physical: Rhino is stunned
      _ <- stackDeck p ["09024"]
      blast <- putInHand p "09004"
      playPaying p blast ["01088", "01089"]
      choose "Rhino" isVillainChoice
      villainDamageNow `shouldReturn` 5
      (V.villainStunned . toAttrs <$> getVillain) `shouldReturn` True
      (V.villainConfused . toAttrs <$> getVillain) `shouldReturn` False

  it "Magic Blast discarding a wild card does all three" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      -- Wong's printed resource is wild
      _ <- stackDeck p ["09002"]
      blast <- putInHand p "09004"
      playPaying p blast ["01088", "01089"]
      choose "Rhino" isVillainChoice
      villainDamageNow `shouldReturn` 7
      v <- toAttrs <$> getVillain
      (v.villainStunned, v.villainConfused) `shouldBe` (True, True)

  it "Astral Projection removes 3 threat and 1 more for each boost icon on the top encounter card" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      placeMainThreat 5
      threat <- mainSchemeThreatNow
      -- Hydra Mercenary has a boost icon
      _ <- stackEncounterDeck [Minions.hydraMercenary]
      projection <- putInHand p "09003"
      playPaying p projection ["01088"]
      chooseIf \case TargetLabel {} -> True; _ -> False
      mainSchemeThreatNow `shouldReturn` (threat - 4)

  it "Mystical Studies adds a Doctor Strange card from the discard pile to hand, as Stephen Strange" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      [cloak] <- stackDiscard p ["09009"]
      studies <- putInHand p "09006"
      playPaying p studies ["01090"]
      choose "the cloak" (isTargetChoice cloak)
      map toCardCode <$> handOf p `shouldReturn` ["09009"]

  it "Protective Ward cancels a revealed treachery and discards it" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      ward <- putInHand p "09007"
      _ <- putInHand p "01090"
      card <- encounterCard Treacheries.imTough
      runNow [RevealEncounterCard p card]
      choose "play Protective Ward" (isPlay ward)
      chooseIf (isPayWith' "01090")
      chooseIf isFinishPayment
      resolve []
      (V.villainIsTough <$> getVillain) `shouldReturn` False
      encounterDiscardNow >>= (`shouldSatisfy` elem card)

  it "Sanctum Sanctorum shuffles a Spell card from the discard pile into the deck and draws a card" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      [ward, _] <- stackDiscard p ["09007", "09024"]
      _ <- putIntoPlay p "09008"
      [sanctum] <- supportsWith "09008"
      choose "Sanctum Sanctorum" (isAbilityOf (supportId sanctum))
      choose "the ward" (isTargetChoice ward)
      length <$> handOf p `shouldReturn` 1
      map toCardCode <$> discardOf p `shouldReturn` ["09024"]

  it "Cloak of Levitation gives the Aerial trait and readies Doctor Strange" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      cloak <- putIntoPlay p "09009"
      resolve [IdentityMessage p ExhaustedIdentity]
      choose "the cloak" (isAbilityOfCard cloak)
      isExhaustedNow p `shouldReturn` False

  it "Magical Enhancements gives your hero +1 THW, ATK and DEF until the round ends, under any player's control" do
    tg <- testGame ["doctor-strange", "spider-man"]
    let (p, other) = (player1 tg, player2 tg)
    inGame tg do
      clearHand p
      changeForm p
      enhancements <- putInHand p "09010"
      playPaying p enhancements ["01088"]
      choose "Spider-Man" (isTargetChoice other)
      map (.upgradeController) <$> upgradesWith "09010" `shouldReturn` [other]
      resolve [IdentityMessage other (ChangedToForm A)]
      other' <- getPlayer other
      -- Spider-Man's printed THW is 1
      thw <- case currentIdentity other' of
        HeroSide h -> getModifiedThwart (toAttrs h)
        AlterEgoSide _ -> error "Spider-Man is a hero"
      thw `shouldBe` 2
      resolve [EndRound]
      upgradesWith "09010" `shouldReturn` []

  it "The Eye of Agamotto generates a wild resource in hero form" do
    tg <- doctorStrange
    let p = player1 tg
    inGame tg do
      clearHand p
      changeForm p
      eye <- putIntoPlay p "09011"
      stackInvocation p ["09033"]
      choose "Spell Mastery" spellMastery
      choose "the eye" (isAbilityOfCard eye)
      chooseIf isFinishPayment
      chooseIf \case TargetLabel {} -> True; _ -> False
      (V.villainConfused . toAttrs <$> getVillain) `shouldReturn` True

  describe "Physical Toll" do
    it "exhausting Stephen Strange removes it from the game" do
      tg <- doctorStrange
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Obligations.physicalToll
        choose "exhaust" (isLabel "$label.exhaustStephenStrangeRemovePhysicalToll")
        isExhaustedNow p `shouldReturn` True
        getsGame (HashMap.null . gameObligations) `shouldReturn` True

    it "otherwise the next event costs 3 more, and it is discarded once one is played" do
      tg <- doctorStrange
      let p = player1 tg
      inGame tg do
        clearHand p
        toll <- revealNow p Obligations.physicalToll
        choose "the next event" (isLabel "$label.nextEventCosts3More")
        obligationsInPlay `shouldReturn` 1
        studies <- putInHand p "09006"
        -- Warning generates a single resource: 1 and 3 more need four
        payment <- traverse (putInHand p) ["09021", "09021", "09021"]
        offers (isPlay studies) `shouldReturn` False
        fourth <- putInHand p "09021"
        playWith studies (fourth : payment)
        chooseIf \case TargetLabel {} -> True; Label "$label.noMatchingCardsFound" _ -> True; _ -> False
        obligationsInPlay `shouldReturn` 0
        encounterDiscardNow >>= (`shouldSatisfy` elem toll)
 where
  isPayWith' code = \case
    PayWithCard c -> toCardCode c == code
    _ -> False
