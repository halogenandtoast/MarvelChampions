-- | The engine mechanics the Venom pack builds on: more restricted upgrades (any, or
-- Weapons only), the patrol keyword, a damage cost a tough status card keeps from being
-- paid, an attack cancelled as a defense, and counters on an ally.
module Marvel.Rules.VenomRulesSpec (spec) where

import Data.HashSet qualified as HashSet
import Marvel.Matchers (SchemeMatcher (ThwartableScheme))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Payment.Types (Payment (NoPayment))
import Marvel.Query (select)
import Marvel.Villain.Types qualified as V
import TestImport

-- | Put a new copy of a player card into play, as if played for free, past the responses
-- to it: what else it asks, the test answers.
putIntoPlayNow :: IdentityId -> CardCode -> GameAppT PlayerCard
putIntoPlayNow ident code = do
  cardId <- getRandom
  let card = MkPlayerCard cardId (lookupPlayerCard code) (Just ident) (Just ident)
  runNow [PutCardIntoPlay ident card NoPayment Nothing True]
  pass
  pure card

askedToDiscardRestricted :: GameAppT Bool
askedToDiscardRestricted =
  openQuestion <&> \case
    Just (_, QuestionLabel "$label.discardARestrictedCard" _ _) -> True
    _ -> False

-- | Decline responses until the choice is offered.
untilOffered :: (Choice -> Bool) -> GameAppT ()
untilOffered p = go (20 :: Int)
 where
  go 0 = pure ()
  go n =
    offers p >>= \case
      True -> pure ()
      False -> whenM (offers isNoResponse) (choose "no response" isNoResponse >> go (n - 1))

venomsPistol, plasmaPistol, sideHolster, captainAmericasShield :: CardCode
venomsPistol = "20010"
plasmaPistol = "20022"
sideHolster = "20021"
captainAmericasShield = "03009"

spec :: Spec
spec = describe "the Venom pack's mechanics" do
  describe "the restricted limit" do
    it "is two: a third restricted upgrade asks for one to be discarded" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p venomsPistol
        _ <- putIntoPlay p venomsPistol
        askedToDiscardRestricted `shouldReturn` False
        third <- putIntoPlayNow p plasmaPistol
        askedToDiscardRestricted `shouldReturn` True
        choose "discard the third" (isTargetChoice (upgradeIdOf third))
        askedToDiscardRestricted `shouldReturn` False
        length <$> upgradesOf p `shouldReturn` 2

    it "is one more for Venom, in either form" do
      tg <- testGame ["venom"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p venomsPistol
        _ <- putIntoPlay p venomsPistol
        _ <- putIntoPlayNow p plasmaPistol
        askedToDiscardRestricted `shouldReturn` False
        changeForm p >> pass
        askedToDiscardRestricted `shouldReturn` False
        fourth <- putIntoPlayNow p plasmaPistol
        askedToDiscardRestricted `shouldReturn` True
        choose "discard the fourth" (isTargetChoice (upgradeIdOf fourth))
        length <$> upgradesOf p `shouldReturn` 3

    it "is one more Weapon with a Side Holster, discarding until what remains fits" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p sideHolster
        _ <- putIntoPlay p captainAmericasShield
        _ <- putIntoPlay p venomsPistol
        _ <- putIntoPlayNow p plasmaPistol
        askedToDiscardRestricted `shouldReturn` False
        fourth <- putIntoPlayNow p plasmaPistol
        askedToDiscardRestricted `shouldReturn` True
        choose "discard the fourth" (isTargetChoice (upgradeIdOf fourth))
        askedToDiscardRestricted `shouldReturn` False

    it "drops as the Side Holster leaves play, and the player discards down to it" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        holster <- putIntoPlay p sideHolster
        _ <- putIntoPlay p captainAmericasShield
        pistol <- putIntoPlay p venomsPistol
        _ <- putIntoPlay p plasmaPistol
        askedToDiscardRestricted `shouldReturn` False
        runNow [RemoveFromPlay (toTarget (upgradeIdOf holster))]
        pass
        askedToDiscardRestricted `shouldReturn` True
        choose "discard the pistol" (isTargetChoice (upgradeIdOf pistol))
        askedToDiscardRestricted `shouldReturn` False
        length <$> upgradesOf p `shouldReturn` 2

  it "a patrol minion keeps the player it is engaged with, and only them, from thwarting the main scheme" do
    tg <- testGame ["venom", "spider-man"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      ms <- mainSchemeId
      resolve [MainSchemeMessage ms (MainSchemePlaceThreat 2)]
      _ <- addMinion other Minions.enragedSymbiote
      mine <- asYou p (select ThwartableScheme)
      liftIO $ mine `shouldSatisfy` HashSet.member (SchemeMainSchemeId ms)
      theirs <- asYou other (select ThwartableScheme)
      liftIO $ theirs `shouldNotSatisfy` HashSet.member (SchemeMainSchemeId ms)

  describe "patrol, on the main scheme" do
    it "keeps the patrolled player from playing Impede on it" do
      tg <- testGame ["venom"]
      let p = player1 tg
      inGame tg do
        changeForm p >> pass >> clearHand p
        ms <- mainSchemeId
        resolve [MainSchemeMessage ms (MainSchemePlaceThreat 4)]
        impede <- putInHand p "18016"
        _ <- putInHand p "01089"
        offers (isPlay impede) `shouldReturn` True
        _ <- addMinion p Minions.enragedSymbiote
        offers (isPlay impede) `shouldReturn` False

    it "keeps threat on it that a card the patrolled player controls would remove, and only theirs" do
      tg <- testGame ["venom", "spider-man"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        ms <- mainSchemeId
        resolve [MainSchemeMessage ms (MainSchemePlaceThreat 4)]
        _ <- addMinion p Minions.enragedSymbiote
        before <- mainSchemeThreatNow
        resolve [MainSchemeMessage ms (MainSchemeThreatRemoved (IdentityRef p) 2)]
        mainSchemeThreatNow `shouldReturn` before
        resolve [MainSchemeMessage ms (MainSchemeThwarted (IdentityRef p) 2)]
        mainSchemeThreatNow `shouldReturn` before
        resolve [MainSchemeMessage ms (MainSchemeThreatRemoved (IdentityRef other) 2)]
        mainSchemeThreatNow `shouldReturn` before - 2

  it "each set-aside Enraged Symbiote is Venom's nemesis minion (rulesheet FAQ); a one-minion set has one" do
    tg <- testGame ["venom", "spider-man"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      map toCardCode <$> getNemesisMinionsOutOfPlay p `shouldReturn` replicate 4 (toCardCode Minions.enragedSymbiote)
      length <$> getNemesisMinionsOutOfPlay other `shouldReturn` 1

  it "a damage cost cannot be paid while the identity has a tough status card (RR FAQ, Focused Rage)" do
    tg <- testGame ["she-hulk"]
    let p = player1 tg
    inGame tg do
      changeForm p >> pass
      rage <- putIntoPlay p "01027"
      offers (isAbilityOf (upgradeIdOf rage)) `shouldReturn` True
      resolve [GiveStatus (IdentityRef p) ToughStatus]
      offers (isAbilityOf (upgradeIdOf rage)) `shouldReturn` False

  it "Grasping Tendrils cancels the villain's attack as a defense: no damage, and the hero defended it" do
    tg <- testGame ["venom"]
    let p = player1 tg
    inGame tg do
      changeForm p >> pass >> clearHand p
      tendrils <- putInHand p "20003"
      strength <- putInHand p "01090"
      counterPunch <- putInHand p "01077"
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks p)]
      playWith tendrils [strength]
      -- "after you defend" (Counter-Punch)
      untilOffered (isPlay counterPunch)
      offers (isPlay counterPunch) `shouldReturn` True
      pass
      damageOf p `shouldReturn` 0
      -- paid using only [physical]: the villain is stunned
      V.villainStunned . toAttrs <$> getVillain `shouldReturn` True

  it "counters can be placed on and removed from an ally" do
    tg <- testGame ["venom"]
    let p = player1 tg
    inGame tg do
      jack <- putIntoPlay p "20011"
      resolve [PlaceCounters (toTarget (allyIdOf jack)) 2]
      (.allyCounters) <$> allyIn "20011" `shouldReturn` 2
      resolve [RemoveCounters (toTarget (allyIdOf jack)) 1]
      (.allyCounters) <$> allyIn "20011" `shouldReturn` 1
