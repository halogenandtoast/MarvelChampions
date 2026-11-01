module Marvel.Hero.SpiderHamSpec (spec) where

import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity ( currentIdentity )
import Marvel.Minion.Cards qualified as Minions
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Trait
import Marvel.Phase
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

countersOf :: IdentityId -> GameAppT Natural
countersOf p = (.playerIdentityCounters) <$> playerAttrs p

spec :: Spec
spec = describe "Spider-Ham" do
  it "starts as Peter Porker with REC 5, 12 hit points and six cards" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      hitPointsOf (player1 tg) `shouldReturn` 12
      handSizeOf (player1 tg) `shouldReturn` 6

  it "responds to non-attack damage once per instance, regardless of amount" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      asHero p
      start (damageIdentity p (toSource p) (toDamage 3 FromAbility))
      choose "Spider-Nonsense" (isNamedAbility "$label.spiderNonsense")
      countersOf p `shouldReturn` 1
      damageOf p `shouldReturn` 3
      start (damageIdentity p (toSource p) (toDamage 1 FromTreachery))
      choose "Spider-Nonsense again" (isNamedAbility "$label.spiderNonsense")
      countersOf p `shouldReturn` 2

  it "does not respond in alter-ego form or to damage prevented by tough" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      start (damageIdentity p (toSource p) (toDamage 2 FromAbility))
      offers (isNamedAbility "$label.spiderNonsense") `shouldReturn` False
      pass
      asHero p
      runNow [IdentityMessage p IdentityBecomeTough]
      start (damageIdentity p (toSource p) (toDamage 2 FromAbility))
      offers (isNamedAbility "$label.spiderNonsense") `shouldReturn` False
      countersOf p `shouldReturn` 0

  it "Cartoon Power gains a counter after a basic recovery, preserving it on a flip" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      setDamage p 7
      choose "recover" \case UseAbility ab -> Recover `elem` abilityChoices ab; _ -> False
      choose "Cartoon Power" (isNamedAbility "$label.cartoonPower")
      countersOf p `shouldReturn` 1
      damageOf p `shouldReturn` 2
      changeForm p
      pass
      countersOf p `shouldReturn` 1

  it "The Daily Beagle exhausts for one counter only in alter-ego" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      clearHand p
      beagle <- putIntoPlay p "30008"
      choose "Daily Beagle" (isAbilityOfCard beagle)
      countersOf p `shouldReturn` 1
      offers (isAbilityOfCard beagle) `shouldReturn` False
      asHero p
      offers (isAbilityOfCard beagle) `shouldReturn` False

  it "Huge Wooden Hammer adds one ATK and spends a toon counter for two more and overkill" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      asHero p
      hammer <- putIntoPlay p "30010"
      currentIdentity <$> getPlayer p >>= \case
        HeroSide h -> Hero.getModifiedAttack (toAttrs h) `shouldReturn` 2
        _ -> error "expected hero"
      runNow [PlaceCounters (toTarget p) 1]
      before <- villainDamageNow
      choose "attack" (isHeroAttack p)
      choose "hammer" (isAbilityOfCard hammer)
      chooseVillain
      countersOf p `shouldReturn` 0
      villainDamageNow `shouldReturn` (before + 4)

  it "Organic Webbing grants THW, readies for a counter, and grants Aerial for the phase" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      asHero p
      webbing <- putIntoPlay p "30011"
      currentIdentity <$> getPlayer p >>= \case
        HeroSide h -> Hero.getModifiedThwart (toAttrs h) `shouldReturn` 3
        _ -> error "expected hero"
      runNow [PlaceCounters (toTarget p) 1, IdentityMessage p ExhaustedIdentity]
      choose "webbing" (isAbilityOfCard webbing)
      exhaustedNow p `shouldReturn` False
      countersOf p `shouldReturn` 0
      identityMatches (IdentityWithTrait Aerial) p `shouldReturn` True
      runNow [UpgradeMessage (upgradeOf webbing) ReadiedUpgrade, PlaceCounters (toTarget p) 1]
      offers (isAbilityOfCard webbing) `shouldReturn` False
      runNow [EndPhase PlayerPhase]
      identityMatches (IdentityWithTrait Aerial) p `shouldReturn` False

  it "spends several toon counters as resources without spending the counter owed by Hogwashed twice" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      asHero p
      runNow [PlaceCounters (toTarget p) 2]
      m <- addMinion p Minions.hydraMercenary
      c <- putInHand p "30004"
      choose "play Hogwashed" (isPlay c)
      choose "spend toon resource" (isNamedAbility "$label.spendIdentityCounter")
      payWith []
      choose "target minion" (isTargetChoice m)
      countersOf p `shouldReturn` 0
      minionDamageOf m `shouldReturn` Nothing

  it "does not offer a toon-cost card with only enough counters to pay its resource cost" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      asHero p
      runNow [PlaceCounters (toTarget p) 1]
      _ <- addMinion p Minions.hydraMercenary
      c <- putInHand p "30004"
      offers (isPlay c) `shouldReturn` False

  it "cannot spend toon counters as resources in alter-ego" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      clearHand p
      runNow [PlaceCounters (toTarget p) 4]
      c <- putInHand p "30008"
      offers (isPlay c) `shouldReturn` False

  it "cannot play Ham It Up for zero threat with no toon counters, but can clear confusion" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      asHero p
      placeMainThreat 2
      c <- putInHand p "30003"
      offers (isPlay c) `shouldReturn` False
      runNow [IdentityMessage p IdentityConfused]
      playWith c []
      isConfused p `shouldReturn` False
      mainSchemeThreatNow `shouldReturn` 2
