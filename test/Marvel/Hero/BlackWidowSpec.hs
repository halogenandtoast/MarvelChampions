module Marvel.Hero.BlackWidowSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as V
import TestImport

blackWidow :: IO TestGame
blackWidow = testGame ["black-widow"]

-- | A card put into play from hand, paid for with new resource cards.
inPlay :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
inPlay p code codes = do
  card <- putInHand p code
  payment <- traverse (putInHand p) codes
  playWith card payment
  pure card

isWidowmaker :: Choice -> Bool
isWidowmaker = isNamedAbility "Widowmaker"

isMissionPrep :: Choice -> Bool
isMissionPrep = isNamedAbility "Mission Prep"

isMinionStunned :: MinionId -> GameAppT Bool
isMinionStunned m = getsGame (maybe False (minionStunned . toAttrs) . HashMap.lookup m . gameMinions)

spec :: Spec
spec = describe "Black Widow" do
  it "deals the pack's own deck of 40 cards" do
    sum (map snd (starter "black-widow").cards) `shouldBe` (40 :: Int) :: IO ()

  it "Widowmaker: after a Preparation card's ability resolves, deal 1 damage to an enemy" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      bite <- inPlay p "08010" ["01088"]
      mercenary <- encounterCard Minions.hydraMercenary
      runNow [RevealEncounterCard p mercenary]
      choose "use Widow's Bite" (isAbilityOfCard bite)
      minionDamageOf (minionOf mercenary) `shouldReturn` Just 2
      choose "Widowmaker" isWidowmaker
      chooseIf (isTargetChoice (EnemyMinionId $ minionOf mercenary))
      minionDamageOf (minionOf mercenary) `shouldReturn` Nothing

  it "Widowmaker is Black Widow's: Natasha Romanoff has it not" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      clearHand p
      spycraft <- inPlay p "08018" ["01088"]
      chooseIf isMissionPrep
      _ <- stackEncounterDeck [Treacheries.theVulturesPlans]
      mercenary <- encounterCard Minions.hydraMercenary
      runNow [RevealEncounterCard p mercenary]
      choose "use Spycraft" (isAbilityOfCard spycraft)
      offers isWidowmaker `shouldReturn` False

  it "Widowmaker does not answer a card that is not a Preparation card" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      coulson <- inPlay p "08011" ["01088", "01089"]
      choose "use Agent Coulson" (isAbilityOfCard coulson)
      chooseIf \case
        TargetLabel {} -> True
        Label "$label.noMatchingCardsFound" _ -> True
        _ -> False
      offers isWidowmaker `shouldReturn` False

  it "Mission Prep: after playing a Preparation card, draw 1 card, once a phase" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      clearHand p
      hand <- length <$> handOf p
      _ <- inPlay p "08017" ["01088"]
      choose "Mission Prep" isMissionPrep
      length <$> handOf p `shouldReturn` (hand + 1)
      clearHand p
      _ <- inPlay p "08024" ["01089"]
      offers isMissionPrep `shouldReturn` False

  it "Mission Prep does not answer a card that is not a Preparation card, nor a card put into play" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      clearHand p
      _ <- inPlay p "08007" ["01088"]
      offers isMissionPrep `shouldReturn` False
      _ <- putIntoPlay p "08024"
      offers isMissionPrep `shouldReturn` False

  it "Winter Soldier costs 1 less for each Preparation card you control" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      clearHand p
      soldier <- putInHand p "08002"
      -- two cards of one resource each
      _ <- putInHand p "08003"
      _ <- putInHand p "08004"
      offers (isPlay soldier) `shouldReturn` False
      _ <- putIntoPlay p "08017"
      _ <- putIntoPlay p "08024"
      offers (isPlay soldier) `shouldReturn` True
      choose "play Winter Soldier" (isPlay soldier)
      chooseIf isAnyPay
      chooseIf isAnyPay
      chooseIf isFinishPayment
      alliesWith "08002" >>= (`shouldSatisfy` ((== 1) . length))
      handOf p `shouldReturn` []

  it "Covert Ops removes 4 threat from a scheme and confuses the villain" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      clearHand p
      placeMainThreat 6
      threat <- mainSchemeThreatNow
      ops <- putInHand p "08003"
      [a, b, c] <- traverse (putInHand p) ["01088", "01089", "01090"]
      playWith ops [a, b, c]
      mid <- mainSchemeId
      chooseIf (isTargetChoice mid)
      mainSchemeThreatNow `shouldReturn` (threat - 4)
      (V.villainConfused . toAttrs <$> getVillain) `shouldReturn` True

  it "Dance of Death makes 3 attacks, of 1, 2 and 3 damage" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      dance <- putInHand p "08004"
      [a, b, c] <- traverse (putInHand p) ["01088", "01089", "01090"]
      playWith dance [a, b, c]
      villainDamageNow `shouldReturn` 6

  it "Dance of Death: a stun prevents only its first attack" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      runNow [IdentityMessage p IdentityStunned]
      dance <- putInHand p "08004"
      [a, b, c] <- traverse (putInHand p) ["01088", "01089", "01090"]
      playWith dance [a, b, c]
      villainDamageNow `shouldReturn` 5
      isStunned p `shouldReturn` False

  it "Safe House #29: as Natasha, exhaust it to take a Preparation card from the discard pile" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      clearHand p
      house <- putIntoPlay p "08005"
      [bite, _] <- stackDiscard p ["08010", "01088"]
      choose "use Safe House #29" (isAbilityOfCard house)
      chooseIf (isTargetChoice bite)
      handOf p `shouldReturn` [bite]
      offers (isAbilityOfCard house) `shouldReturn` False

  it "Safe House #29 is an alter-ego action" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      clearHand p
      house <- putIntoPlay p "08005"
      _ <- stackDiscard p ["08010"]
      changeForm p
      offers (isAbilityOfCard house) `shouldReturn` False

  it "Attacrobatics cancels a boost card's icons and deals as much damage to the villain" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      acro <- inPlay p "08006" ["01088"]
      _ <- stackEncounterDeck [Treacheries.soundManipulation]
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks p)]
      choose "no defender" isNoDefender
      choose "use Attacrobatics" (isAbilityOfCard acro)
      pass
      villainDamageNow `shouldReturn` 2
      -- Rhino's 2 ATK, without the 2 boost icons
      damageOf p `shouldReturn` 2

  it "Attacrobatics cannot be used on a boost card without boost icons" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      acro <- inPlay p "08006" ["01088"]
      _ <- stackEncounterDeck [Minions.taskmaster]
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks p)]
      choose "no defender" isNoDefender
      offers (isAbilityOfCard acro) `shouldReturn` False

  it "Black Widow's Gauntlet pays only for a Preparation card" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      clearHand p
      _ <- putIntoPlay p "08007"
      bite <- putInHand p "08010"
      coulson <- putInHand p "08011"
      offers (isPlay coulson) `shouldReturn` False
      offers (isPlay bite) `shouldReturn` True
      choose "play Widow's Bite" (isPlay bite)
      choose "use the gauntlet" isAnyAbility
      chooseIf isFinishPayment
      upgradeCodes >>= (`shouldSatisfy` elem "08010")

  it "Grappling Hook cancels a treachery you reveal and discards it" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      hook <- inPlay p "08008" ["01088", "01089"]
      shot <- encounterCard Treacheries.deadlyShot
      runNow [RevealEncounterCard p shot]
      choose "use Grappling Hook" (isAbilityOfCard hook)
      pass
      damageOf p `shouldReturn` 0
      (map ecCardId <$> encounterDiscard) >>= (`shouldSatisfy` elem (ecCardId shot))
      upgradeCodes `shouldReturn` []
      length . filter (== hook) <$> discardOf p `shouldReturn` 1

  it "Grappling Hook is a hero interrupt" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      clearHand p
      hook <- putIntoPlay p "08008"
      shot <- encounterCard Treacheries.deadlyShot
      runNow [RevealEncounterCard p shot]
      offers (isAbilityOfCard hook) `shouldReturn` False

  it "Synth-Suit gives +1 DEF, and readies Black Widow after a Preparation card's ability" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      suit <- inPlay p "08009" ["01088", "01089", "01090"]
      bite <- putIntoPlay p "08010"
      runNow [IdentityMessage p ExhaustedIdentity]
      mercenary <- encounterCard Minions.hydraMercenary
      runNow [RevealEncounterCard p mercenary]
      choose "use Widow's Bite" (isAbilityOfCard bite)
      choose "use Synth-Suit" (isAbilityOfCard suit)
      isExhaustedNow p `shouldReturn` False
      pass
      -- Rhino's 2 ATK and 2 boost against 3 DEF
      _ <- stackEncounterDeck [Treacheries.theVulturesPlans]
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks p)]
      choose "defend" isDefend
      pass
      damageOf p `shouldReturn` 1

  it "Widow's Bite deals 2 damage to a minion entering play and stuns it" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      bite <- inPlay p "08010" ["01088"]
      mercenary <- encounterCard Minions.taskmaster
      runNow [RevealEncounterCard p mercenary]
      choose "use Widow's Bite" (isAbilityOfCard bite)
      pass
      minionDamageOf (minionOf mercenary) `shouldReturn` Just 2
      isMinionStunned (minionOf mercenary) `shouldReturn` True
      discardOf p >>= (`shouldSatisfy` elem bite)

  it "Widow's Bite is an attack: a stunned Black Widow only loses the stun" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      bite <- inPlay p "08010" ["01088"]
      runNow [IdentityMessage p IdentityStunned]
      mercenary <- encounterCard Minions.taskmaster
      runNow [RevealEncounterCard p mercenary]
      choose "use Widow's Bite" (isAbilityOfCard bite)
      pass
      minionDamageOf (minionOf mercenary) `shouldReturn` Just 0
      isMinionStunned (minionOf mercenary) `shouldReturn` False
      isStunned p `shouldReturn` False

  it "Burn Notice: flipping to Natasha Romanoff and exhausting her removes it from the game" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      changeForm p
      _ <- revealNow p Obligations.burnNotice
      choose "flip to alter-ego" (isLabel "$label.changeToB")
      choose "exhaust Natasha" (isLabel "$label.exhaustNatashaRomanoffRemoveBurnNotice")
      isExhaustedNow p `shouldReturn` True
      obligationsInPlay `shouldReturn` 0
      encounterDiscardCodes >>= (`shouldSatisfy` notElem "08025")

  it "Burn Notice: otherwise the Preparation card with the highest cost is discarded" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      changeForm p
      _ <- putIntoPlay p "08010"
      hook <- putIntoPlay p "08008"
      _ <- putIntoPlay p "08009"
      _ <- revealNow p Obligations.burnNotice
      choose "stay a hero" (isLabel "$label.doNotFlipToAlterEgo")
      chooseIf (isTargetChoice (upgradeOf hook))
      upgradeCodes >>= (`shouldSatisfy` \us -> "08008" `notElem` us && "08010" `elem` us && "08009" `elem` us)
      encounterDiscardCodes >>= (`shouldSatisfy` elem "08025")

  it "Burn Notice: with no Preparation card it surges" do
    tg <- blackWidow
    let p = player1 tg
    inGame tg do
      changeForm p
      [next] <- stackEncounterDeck [Minions.hydraMercenary]
      _ <- revealNow p Obligations.burnNotice
      choose "stay a hero" (isLabel "$label.doNotFlipToAlterEgo")
      chooseIf (isLabel "$label.discardHighestCostPreparationDiscardObligation")
      pass
      getsGame (HashMap.member (minionOf next) . gameMinions) `shouldReturn` True
 where
  isAnyPay = \case
    PayWithCard _ -> True
    _ -> False
