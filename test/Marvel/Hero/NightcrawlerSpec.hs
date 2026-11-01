module Marvel.Hero.NightcrawlerSpec (spec) where

import Data.HashMap.Strict ()
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (currentIdentity)
import Marvel.Keyword
import Marvel.Matchers ()
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Window ()
import TestImport

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

paying :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
paying p code codes = do
  card <- putInHand p code
  payment <- traverse (putInHand p) codes
  playWith card payment
  pure card

heroStats :: IdentityId -> GameAppT (Natural,Natural,Natural)
heroStats p = currentIdentity <$> getPlayer p >>= \case
  HeroSide h -> (,,) <$> Hero.getModifiedThwart (toAttrs h) <*> Hero.getModifiedAttack (toAttrs h) <*> Hero.getModifiedDefense (toAttrs h)
  _ -> pure (0,0,0)

bamfOn :: IdentityId -> EnemyId -> GameAppT PlayerCard
bamfOn p enemy = do
  card <- putInHand p "48006"
  playOn card (enemyRef enemy) []
  pass
  pure card

spec :: Spec
spec = describe "Nightcrawler" do
  it "starts as Kurt Wagner with nine hit points and six cards; hero has 2/1/3 and five cards" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      hitPointsOf p `shouldReturn` 9
      handSizeOf p `shouldReturn` 6
      asHero p
      heroStats p `shouldReturn` (2,1,3)
      handSizeOf p `shouldReturn` 5

  it "Kurt searches for the exact Bamf card and can only search once per round" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      clearHand p
      [bamf] <- stackDeck p ["48006"]
      choose "Kurt's Bamf search" (\case UseAbility a -> abilitySource a == IdentityRef p && abilityIndex a == 1; _ -> False)
      choose "take Bamf" (isTargetChoice bamf)
      pass
      handOf p >>= (`shouldSatisfy` elem bamf)
      offers (\case UseAbility a -> abilitySource a == IdentityRef p && abilityIndex a == 1; _ -> False) `shouldReturn` False

  it "Rapid Teleportation spends one resource to retrieve Bamf, once per phase" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      [bamf] <- stackDiscard p ["48006"]
      resource <- putInHand p "01086"
      choose "Rapid Teleportation" (\case UseAbility a -> abilitySource a == IdentityRef p && abilityIndex a == 1; _ -> False)
      payWith [resource]
      choose "retrieve Bamf" (isTargetChoice bamf)
      pass
      handOf p >>= (`shouldSatisfy` elem bamf)
      offers (\case UseAbility a -> abilitySource a == IdentityRef p && abilityIndex a == 1; _ -> False) `shouldReturn` False

  it "Rapid Teleportation is unavailable without a Bamf in the discard pile" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      _ <- stackDiscard p []
      _ <- putInHand p "01088"
      offers (\case UseAbility a -> abilitySource a == IdentityRef p && abilityIndex a == 1; _ -> False) `shouldReturn` False

  it "Kurt's Chapel adds a point of REC and exhausts to let a chosen player draw after recovery" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      clearHand p
      chapel <- paying p "48003" ["01086"]
      pass
      getModifiers p >>= (`shouldSatisfy` elem (RecoveryModifier 1))
      resolve [IdentityMessage p (IdentityDamaged GameRef $ toDamage 5 FromAbility)]
      choose "recover" (\case UseAbility a -> Recover `elem` abilityChoices a; _ -> False)
      choose "Chapel's recovery response" (isAbilityOfCard chapel)
      choose "draw for Kurt" (isTargetChoice p)
      pass
      damageOf p `shouldReturn` 1
      length <$> handOf p `shouldReturn` 1
      offers (isAbilityOfCard chapel) `shouldReturn` False

  it "Kurt's Cutlasses boosts Nightcrawler and occupies two restricted slots" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      swords <- paying p "48004" ["01088"]
      pass
      heroStats p `shouldReturn` (2,2,4)
      getModifiers p >>= (`shouldSatisfy` elem (KeywordModifier $ Retaliate 1))
      getModifiers (upgradeIdOf swords) >>= (`shouldSatisfy` elem (RestrictedSlots 2))

  it "Cutlasses do not add ATK DEF or retaliate to Kurt Wagner" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      _ <- putIntoPlay p "48004"
      getModifiers p >>= (`shouldSatisfy` notElem (KeywordModifier $ Retaliate 1))

  it "Prehensile Tail adds a restricted slot and generates an event-only wild" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      tailCard <- putIntoPlay p "48005"
      getModifiers p >>= (`shouldSatisfy` elem (RestrictedLimitModifier 1))
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main (MainSchemePlaceThreat 3)]
      scout <- putInHand p "48009"
      choose "play Scout Ahead" (isPlay scout)
      choose "Tail pays for event" (isAbilityOfCard tailCard)
      payWith []
      chooseMainScheme
      pass
      upgradeIsExhausted (upgradeIdOf tailCard) `shouldReturn` True

  it "Bamf attaches to enemies and a second copy cannot attach to the same enemy" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      v <- villainIdNow
      _ <- bamfOn p (EnemyVillainId v)
      second <- putInHand p "48006"
      offers (isPlay second) `shouldReturn` False

  it "Bamf declares basic defense while exhausted and applies DEF without readying" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      m <- addMinion p Minions.hydraMercenary
      bamf <- bamfOn p (EnemyMinionId m)
      resolve [IdentityMessage p ExhaustedIdentity]
      runNow [MinionMessage m (MinionAttacks p)]
      choose "Bamf defense" (isAbilityOfCard bamf)
      pass
      damageOf p `shouldReturn` 0
      isExhaustedPlayer p `shouldReturn` True
      discardOf p >>= (`shouldSatisfy` elem bamf)

  it "Bamf defends a villain attack with a ready hero without offering another defense or applying DEF twice" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      v <- villainIdNow
      bamf <- bamfOn p (EnemyVillainId v)
      isExhaustedPlayer p `shouldReturn` False
      -- Rhino's two ATK plus Shocker's two boost icons exceeds Nightcrawler's
      -- three DEF, so applying DEF twice would incorrectly prevent all damage.
      _ <- stackEncounterDeck [Minions.shocker]
      runNow [VillainMessage v (VillainAttacks p)]
      choose "Bamf defense" (isAbilityOfCard bamf)
      offers isDefend `shouldReturn` False
      isExhaustedPlayer p `shouldReturn` False
      pass
      damageOf p `shouldReturn` 1
      isExhaustedPlayer p `shouldReturn` False
      discardOf p >>= (`shouldSatisfy` elem bamf)

  it "Bamf defends a minion attack with a ready hero without offering another defense or applying DEF twice" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      m <- addMinion p Minions.hydraMercenary
      bamf <- bamfOn p (EnemyMinionId m)
      isExhaustedPlayer p `shouldReturn` False
      -- Five ATK against three DEF deals two, rather than zero from applying
      -- Nightcrawler's DEF a second time through an ordinary defense.
      runNow [MinionMessage m (MinionAttacksWithChange p 4 [])]
      choose "Bamf defense" (isAbilityOfCard bamf)
      offers isDefend `shouldReturn` False
      isExhaustedPlayer p `shouldReturn` False
      pass
      damageOf p `shouldReturn` 2
      isExhaustedPlayer p `shouldReturn` False
      discardOf p >>= (`shouldSatisfy` elem bamf)

  it "Tally Ho returns the exact Bamf discarded for the defense and damages the attacker" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      m <- addMinion p Minions.tigerShark
      bamf <- bamfOn p (EnemyMinionId m)
      tally <- putInHand p "48011"
      resource <- putInHand p "01086"
      runNow [MinionMessage m (MinionAttacks p)]
      choose "Bamf defense" (isAbilityOfCard bamf)
      playWith tally [resource]
      pass
      handOf p >>= (`shouldSatisfy` elem bamf)
      minionDamageOf m `shouldReturn` Just 3
      damageOf p `shouldReturn` 0

  it "Tally Ho is not playable after a normal basic defense" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      m <- addMinion p Minions.hydraMercenary
      tally <- putInHand p "48011"
      _ <- putInHand p "01088"
      runNow [MinionMessage m (MinionAttacks p)]
      choose "defend normally" isDefend
      offers (isPlay tally) `shouldReturn` False
      pass

  it "Port and Punch deals three to its chosen enemy and three more to every Bamf host" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      v <- villainIdNow
      _ <- bamfOn p (EnemyVillainId v)
      _ <- paying p "48007" ["01088"]
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 6

  it "Teleport Drop discards an enemy's Bamf as a cost, deals eight and stuns that host" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      v <- villainIdNow
      bamf <- bamfOn p (EnemyVillainId v)
      _ <- paying p "48008" ["01088"]
      pass
      villainDamageNow `shouldReturn` 8
      getModifiers v >>= (`shouldSatisfy` notElem CannotBeStunned)
      discardOf p >>= (`shouldSatisfy` elem bamf)
      playerAttrs p >>= (\a -> playerIdentityStunned a `shouldBe` False)

  it "Teleport Drop cannot be played without an attached Bamf" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      dropCard <- putInHand p "48008"
      _ <- putInHand p "01088"
      offers (isPlay dropCard) `shouldReturn` False

  it "Scout Ahead removes three threat and can decline its optional discard" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      s <- mainSchemeIdNow
      resolve [MainSchemeMessage s (MainSchemePlaceThreat 4)]
      before <- mainSchemeThreatNow
      _ <- paying p "48009" ["01086"]
      chooseMainScheme
      pass
      mainSchemeThreatNow `shouldReturn` before - 3

  it "Port Away discards Bamf from hand, changes form and readies Nightcrawler" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      resolve [IdentityMessage p ExhaustedIdentity]
      bamf <- putInHand p "48006"
      _ <- paying p "48010" []
      whenM (offers $ isTargetChoice bamf) $ choose "discard Bamf" (isTargetChoice bamf)
      pass
      playerIdentitySide <$> playerAttrs p `shouldReturn` B
      isExhaustedPlayer p `shouldReturn` False
      discardOf p >>= (`shouldSatisfy` elem bamf)

  it "Daytripper searches for Bamf, attaches it, then damages each Bamf host" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      [bamf] <- stackDeck p ["48006"]
      daytripper <- paying p "48002" ["01088"]
      choose "Daytripper's entry response" (isAbilityOfCard daytripper)
      choose "search Bamf" (isTargetChoice bamf)
      chooseVillain
      pass
      villainDamageNow `shouldReturn` 1
      [u] <- upgradesWith "48006"
      v <- villainIdNow
      upgradeAttachedEnemy u `shouldBe` Just (EnemyVillainId v)

  it "Scout Ahead's optional second removal excludes the first scheme" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      side <- revealNow p Schemes.breakinAndTakin
      pass
      main <- mainSchemeIdNow
      resolve [MainSchemeMessage main (MainSchemePlaceThreat 4)]
      bamf <- putInHand p "48006"
      _ <- paying p "48009" ["01086"]
      chooseMainScheme
      choose "discard Bamf for second scheme" (isTargetChoice bamf)
      offers (isTargetChoice main) `shouldReturn` False
      choose "remove from different scheme" (isTargetChoice $ sideSchemeOf side)
      pass
      discardOf p >>= (`shouldSatisfy` elem bamf)

  it "Port Away works from alter-ego form and cannot be played without a hand Bamf" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      clearHand p
      event <- putInHand p "48010"
      offers (isPlay event) `shouldReturn` False
      bamf <- putInHand p "48006"
      resolve [IdentityMessage p ExhaustedIdentity]
      playWith event []
      whenM (offers $ isTargetChoice bamf) $ choose "discard Bamf" (isTargetChoice bamf)
      pass
      playerIdentitySide <$> playerAttrs p `shouldReturn` A
      isExhaustedPlayer p `shouldReturn` False

  it "Teleport Drop cannot choose a guarded villain's Bamf as its attack target" do
    tg <- testGame ["nightcrawler"]
    inGame tg do
      let p = player1 tg
      asHero p
      v <- villainIdNow
      _ <- bamfOn p (EnemyVillainId v)
      _ <- addMinion p Minions.hydraMercenary
      event <- putInHand p "48008"
      _ <- putInHand p "01088"
      offers (isPlay event) `shouldReturn` False

  it "Port Away lets Ant-Man choose another hero form rather than forcing alter-ego" do
    tg <- testGame ["ant-man"]
    inGame tg do
      let p = player1 tg
      changeFormTo p A
      pass
      clearHand p
      bamf <- putInHand p "48006"
      event <- putInHand p "48010"
      playWith event []
      whenM (offers $ isTargetChoice bamf) $ choose "discard Bamf" (isTargetChoice bamf)
      choose "change to Giant" (isFormChoice $ Just C)
      pass
      playerIdentitySide <$> playerAttrs p `shouldReturn` C

  it "Cutlasses' named Nightcrawler bonus works when another player controls the upgrade" do
    tg <- testGame ["nightcrawler", "spider-man"]
    inGame tg do
      let p = player1 tg
          other = player2 tg
      asHero p
      _ <- putIntoPlay other "48004"
      heroStats p `shouldReturn` (2,2,4)
      getModifiers p >>= (`shouldSatisfy` elem (KeywordModifier $ Retaliate 1))

  it "Chapel's named recovery bonus applies to Kurt even if another player controls Chapel" do
    tg <- testGame ["nightcrawler", "spider-man"]
    inGame tg do
      let p = player1 tg
          other = player2 tg
      _ <- putIntoPlay other "48003"
      getModifiers p >>= (`shouldSatisfy` elem (RecoveryModifier 1))
