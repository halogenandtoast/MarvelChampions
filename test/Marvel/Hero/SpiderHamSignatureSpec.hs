module Marvel.Hero.SpiderHamSignatureSpec (spec) where

import Marvel.Villain.Types (villainConfused)
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Window qualified as W
import TestImport
import Marvel.Minion.Cards qualified as Minions

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

spec :: Spec
spec = describe "Spider-Ham signature cards" do
  it "Ham It Up removes threat equal to unspent toon counters" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      asHero p
      placeMainThreat 5
      runNow [PlaceCounters (toTarget p) 3]
      c <- putInHand p "30003"
      playWith c []
      chooseMainScheme
      mainSchemeThreatNow `shouldReturn` 2
      (.playerIdentityCounters) <$> playerAttrs p `shouldReturn` 3

  it "Hogwashed deals non-attack damage to a minion and spends one toon counter" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      asHero p
      m <- addMinion p Minions.hydraMercenary
      runNow [PlaceCounters (toTarget p) 1]
      c <- putInHand p "30004"
      pay <- putInHand p "01090"
      playWith c [pay]
      choose "minion" (isTargetChoice m)
      minionDamageOf m `shouldReturn` Nothing
      (.playerIdentityCounters) <$> playerAttrs p `shouldReturn` 0

  it "Petulant Pig draws three after the villain's attack resolves" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      asHero p
      c <- putInHand p "30006"
      playWith c []
      choose "no defender" isNoDefender
      pass
      length <$> handOf p `shouldReturn` 3

  it "Swinging Web Pig deals six damage and confuses its target" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      asHero p
      c <- putInHand p "30007"
      a <- putInHand p "01088"
      b <- putInHand p "01089"
      before <- villainDamageNow
      playWith c [a,b]
      chooseVillain
      villainDamageNow `shouldReturn` (before + 6)
      (.villainConfused) . toAttrs <$> getVillain `shouldReturn` True

  it "Cartoon Physics discards to prevent all but one damage, which can trigger Spider-Nonsense" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      asHero p
      physics <- putIntoPlay p "30009"
      start (damageIdentity p (toSource p) (toDamage 5 FromAbility))
      choose "Cartoon Physics" (isAbilityOfCard physics)
      choose "Spider-Nonsense" (isNamedAbility "$label.spiderNonsense")
      damageOf p `shouldReturn` 1
      (.playerIdentityCounters) <$> playerAttrs p `shouldReturn` 1

  it "Captain Americat places a counter and shuffles a signature card from discard" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      asHero p
      sig <- putInHand p "30003"
      runNow [DiscardedCard $ PlayerCard sig]
      c <- putInHand p "30002"
      a <- putInHand p "01088"
      b <- putInHand p "01089"
      playWith c [a,b]
      choose "Captain Americat response" (isAbilityOfCard c)
      choose "shuffle signature" (isTargetChoice sig)
      (.playerIdentityCounters) <$> playerAttrs p `shouldReturn` 1
      discardOf p >>= \cards -> cards `shouldSatisfy` all ((/= pcCardId sig) . pcCardId)

  it "I Don't Think So cancels a revealed minion's entire card, including entering play" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      asHero p
      runNow [PlaceCounters (toTarget p) 1]
      c <- putInHand p "30005"
      pay <- putInHand p "01090"
      card <- revealNow p Minions.hydraMercenary
      playWith c [pay]
      pass
      minionDamageOf (minionOf card) `shouldReturn` Nothing
      (.playerIdentityCounters) <$> playerAttrs p `shouldReturn` 0

  it "Hogwashed removes five threat from a side scheme without triggering Overwatch" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      asHero p
      encounter <- encounterCard Schemes.crowdControl
      resolve [RevealEncounterCard p encounter]
      let sid = sideSchemeOf encounter
      resolve [SideSchemeMessage sid $ SideSchemePlaceThreat 6]
      before <- sideSchemeThreat sid
      overwatch <- putInHand p "30019"
      playOn overwatch sid []
      runNow [PlaceCounters (toTarget p) 1]
      card <- putInHand p "30004"
      payment <- putInHand p "01090"
      playWith card [payment]
      choose "side scheme" (isTargetChoice sid)
      offers (isAbilityOfCard overwatch) `shouldReturn` False
      sideSchemeThreat sid `shouldReturn` (before - 5)
      length <$> upgradesWith "30019" `shouldReturn` 1
      (.playerIdentityCounters) <$> playerAttrs p `shouldReturn` 0

  it "Hogwashed cannot be played without its toon-counter cost" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- addMinion p Minions.hydraMercenary
      card <- putInHand p "30004"
      _ <- putInHand p "01090"
      offers (isPlay card) `shouldReturn` False

  it "I Don't Think So cannot cancel a card revealed by another player" do
    tg <- testGame ["spider-ham", "spider-man"]
    inGame tg do
      let p = player1 tg
      asHero p
      runNow [PlaceCounters (toTarget p) 1]
      card <- putInHand p "30005"
      _ <- putInHand p "01090"
      encounter <- revealNow (player2 tg) Minions.hydraMercenary
      offers (isPlay card) `shouldReturn` False
      pass
      minionDamageOf (minionOf encounter) `shouldReturn` Just 0
      (.playerIdentityCounters) <$> playerAttrs p `shouldReturn` 1

  it "I Don't Think So cannot cancel a reveal from the discard pile" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      asHero p
      runNow [PlaceCounters (toTarget p) 1]
      card <- putInHand p "30005"
      _ <- putInHand p "01090"
      encounter <- encounterCard Minions.hydraMercenary
      runNow [RevealEncounterCardFrom W.RevealedFromDiscardPile p encounter]
      offers (isPlay card) `shouldReturn` False
      pass
      minionDamageOf (minionOf encounter) `shouldReturn` Just 0
      (.playerIdentityCounters) <$> playerAttrs p `shouldReturn` 1

  it "Huge Wooden Hammer spills excess damage, then loses overkill after that attack" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      asHero p
      hammer <- putIntoPlay p "30010"
      runNow [PlaceCounters (toTarget p) 1]
      first <- addMinion p Minions.hydraMercenary
      choose "basic attack" (isHeroAttack p)
      choose "use the hammer" (isAbilityOfCard hammer)
      choose "attack the first minion" (isTargetChoice first)
      pass
      minionDamageOf first `shouldReturn` Nothing
      villainDamageNow `shouldReturn` 1
      (.playerIdentityCounters) <$> playerAttrs p `shouldReturn` 0
      resolve [IdentityMessage p ReadiedIdentity, UpgradeMessage (upgradeOf hammer) ReadiedUpgrade]
      second <- addMinion p Minions.hydraMercenary
      resolve [MinionMessage second $ MinionDamaged (toSource p) (toDamage 2 FromAbility)]
      choose "basic attack without a counter" (isHeroAttack p)
      offers (isAbilityOfCard hammer) `shouldReturn` False
      choose "attack the second minion" (isTargetChoice second)
      pass
      minionDamageOf second `shouldReturn` Nothing
      villainDamageNow `shouldReturn` 1

  it "Cartoon Physics works in alter-ego form and is discarded as its cost" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      clearHand p
      physics <- putIntoPlay p "30009"
      start (damageIdentity p (toSource p) (toDamage 4 FromAbility))
      choose "prevent all but one" (isAbilityOfCard physics)
      pass
      damageOf p `shouldReturn` 1
      upgradesWith "30009" `shouldReturn` []
      map toCardCode <$> discardOf p >>= (`shouldSatisfy` elem "30009")
      (.playerIdentityCounters) <$> playerAttrs p `shouldReturn` 0

  it "Cartoon Physics is not offered against only one damage" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      clearHand p
      physics <- putIntoPlay p "30009"
      start (damageIdentity p (toSource p) (toDamage 1 FromAbility))
      offers (isAbilityOfCard physics) `shouldReturn` False
      pass
      damageOf p `shouldReturn` 1
      length <$> upgradesWith "30009" `shouldReturn` 1

  it "Captain Americat still places a counter without a signature card in discard" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      clearHand p
      card <- putInHand p "30002"
      a <- putInHand p "01088"
      b <- putInHand p "01089"
      playWith card [a,b]
      choose "Captain Americat response" (isAbilityOfCard card)
      (.playerIdentityCounters) <$> playerAttrs p `shouldReturn` 1
      discardOf p >>= \cards -> cards `shouldSatisfy` all ((/= "30002") . toCardCode)

  it "Petulant Pig draws three even when stun replaces the villain's attack" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      asHero p
      vid <- villainIdNow
      resolve [VillainMessage vid $ VillainStunned GameRef]
      card <- putInHand p "30006"
      playWith card []
      pass
      damageOf p `shouldReturn` 0
      length <$> handOf p `shouldReturn` 3

  it "a second Cartoon Physics cannot prevent damage already reduced to one" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      clearHand p
      first <- putIntoPlay p "30009"
      second <- putIntoPlay p "30009"
      start (damageIdentity p (toSource p) (toDamage 5 FromAbility))
      choose "use the first Cartoon Physics" (isAbilityOfCard first)
      offers (isAbilityOfCard second) `shouldReturn` False
      pass
      damageOf p `shouldReturn` 1
      length <$> upgradesWith "30009" `shouldReturn` 1

  it "Hogwashed does not offer a side scheme whose threat cannot be removed" do
    tg <- testGame ["spider-ham"]
    inGame tg do
      let p = player1 tg
      asHero p
      minion <- addMinion p Minions.abomination
      scheme <- encounterCard Schemes.totalDestruction
      resolve [RevealEncounterCard p scheme]
      runNow [PlaceCounters (toTarget p) 1]
      card <- putInHand p "30004"
      payment <- putInHand p "01090"
      playWith card [payment]
      offers (isTargetChoice $ sideSchemeOf scheme) `shouldReturn` False
      choose "damage Abomination" (isTargetChoice minion)
