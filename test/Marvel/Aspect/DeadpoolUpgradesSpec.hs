module Marvel.Aspect.DeadpoolUpgradesSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity ( currentIdentity )
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Phase (Phase (..))
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Upgrade.Types qualified as Upgrade
import Marvel.Window qualified as W
import Test.Hspec qualified as Hspec
import TestImport

newtype PoolUpgradeModifiers = PoolUpgradeModifiers (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage PoolUpgradeModifiers where
  runMessage msg (PoolUpgradeModifiers attrs) = PoolUpgradeModifiers <$> runMessage msg attrs

modifyFor :: EntityMatcher -> [Modifier] -> GameAppT ()
modifyFor matcher modifiers = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (PoolUpgradeModifiers EffectAttrs
    { effectId = eid, effectCardCode = "44028", effectSource = GameRef
    , effectMatcher = matcher, effectModifiers = modifiers, effectEnds = Nothing })

asHero :: IdentityId -> GameAppT ()
asHero you = clearHand you >> changeForm you >> pass

upgradeAttrsOf :: PlayerCard -> GameAppT (Attrs Upgrade.Upgrade)
upgradeAttrsOf card = getsGame $ maybe (error "missing upgrade") toAttrs . HashMap.lookup (upgradeIdOf card) . gameUpgrades

isInPlay :: PlayerCard -> GameAppT Bool
isInPlay card = getsGame (HashMap.member (upgradeIdOf card) . gameUpgrades)

identityAbility :: IdentityId -> Choice -> Bool
identityAbility you = \case
  UseAbility ab -> abilitySource ab == IdentityRef you && abilityIndex ab == 1
  _ -> False

heroAttack :: IdentityId -> GameAppT Natural
heroAttack you = currentIdentity <$> getPlayer you >>= \case
  HeroSide h -> Hero.getModifiedAttack (toAttrs h)
  _ -> pure 0

phaseBegins :: W.WindowTiming -> Phase -> [Message]
phaseBegins timing phase = [CheckWindows [W.Window timing $ W.PhaseBegan phase]]

playAmbush :: IdentityId -> SideSchemeId -> GameAppT PlayerCard
playAmbush you scheme = do
  card <- putInHand you "44051"
  payment <- putInHand you "01083"
  playOn card scheme [payment]
  pass
  pure card

crisisCount :: GameAppT Natural
crisisCount = (\(c, _, _, _) -> c) <$> getEncounterIconCounts

spec :: Spec
spec = describe "Deadpool Pool upgrades" do
  describe "Git Gud" do
    it "costs zero after a non-win and two after a win, using the saved player report" do
      for_ [False, True] \won -> do
        tg <- testGame ["deadpool"]
        inGame tg do
          let you = player1 tg
          clearHand you
          resolve [CardSpecific (IdentityRef you) "previousGameWon" (toJSON won)]
          card <- putInHand you "44028"
          if won then do
            offers (isPlay card) `shouldReturn` False
            payment <- putInHand you "01088"
            playWith card [payment]
          else playWith card []
          pass
          isInPlay card `shouldReturn` True
          getPreviousGameWon you `shouldReturn` Just won

    it "saves another player's defeated identity, sets one HP and removes itself from the game" do
      tg <- testGame ["deadpool", "spider-man"]
      inGame tg do
        let you = player1 tg
            victim = player2 tg
        clearHand you
        card <- putIntoPlay you "44028"
        hp <- hitPointsOf victim
        runNow (damageIdentity victim GameRef $ toDamage hp FromAbility)
        choose "Git Gud saves the other player" (isAbilityOfCard card)
        pass
        damageOf victim `shouldReturn` (hp - 1)
        toCardCode <$> getPlayer victim `shouldReturn` "01001b"
        damageOf you `shouldReturn` 0
        isInPlay card `shouldReturn` False
        discardOf you >>= (`shouldSatisfy` notElem card)
        getMainSchemeTokenCount `shouldReturn` 0

    it "also replaces a defeat that is not caused by damage" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        card <- putIntoPlay you "44028"
        runNow [CheckWindows [W.Window W.Would $ W.IdentityDefeat you], IdentityMessage you IdentityDefeated]
        choose "replace defeat" (isAbilityOfCard card)
        pass
        damageOf you `shouldReturn` 8
        isInPlay card `shouldReturn` False

    it "lets the player choose the order of Git Gud and Deadpool, and only the chosen replacement resolves" do
      for_ [False, True] \deadpoolFirst -> do
        tg <- testGame ["deadpool"]
        inGame tg do
          let you = player1 tg
          asHero you
          card <- putIntoPlay you "44028"
          runNow (damageIdentity you GameRef $ toDamage 9 FromAbility)
          offers (identityAbility you) `shouldReturn` True
          offers (isAbilityOfCard card) `shouldReturn` True
          if deadpoolFirst
            then choose "Deadpool replaces defeat first" (identityAbility you)
            else choose "Git Gud replaces defeat first" (isAbilityOfCard card)
          pass
          toCardCode <$> getPlayer you `shouldReturn` "44001b"
          damageOf you `shouldReturn` 8
          getMainSchemeTokenCount `shouldReturn` (if deadpoolFirst then 1 else 0)
          isInPlay card `shouldReturn` deadpoolFirst

    it "does not rescue anyone while its printed text is blank" do
      tg <- testGame ["deadpool", "spider-man"]
      inGame tg do
        let you = player1 tg
            victim = player2 tg
        card <- putIntoPlay you "44028"
        modifyFor (UpgradeEntity $ UpgradeWithId $ upgradeIdOf card) [Blank]
        hp <- hitPointsOf victim
        runNow (damageIdentity victim GameRef $ toDamage hp FromAbility)
        offers (isAbilityOfCard card) `shouldReturn` False
        isInPlay card `shouldReturn` True

  describe "Healing Factor" do
    it "heals two from its controller and exhausts after the player phase begins, including in alter-ego" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        card <- putInHand you "44029"
        energy <- putInHand you "01088"
        genius <- putInHand you "01089"
        playWith card [energy, genius]
        pass
        setDamage you 3
        runNow (phaseBegins W.After PlayerPhase)
        choose "heal with Healing Factor" (isAbilityOfCard card)
        pass
        damageOf you `shouldReturn` 1
        Upgrade.upgradeExhausted <$> upgradeAttrsOf card `shouldReturn` True
        toCardCode <$> getPlayer you `shouldReturn` "44001b"

    it "does not respond before player-phase beginning, in the villain phase, at full health or when exhausted" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        card <- putIntoPlay you "44029"
        runNow (phaseBegins W.After PlayerPhase)
        offers (isAbilityOfCard card) `shouldReturn` False
        setDamage you 3
        runNow (phaseBegins W.When PlayerPhase)
        offers (isAbilityOfCard card) `shouldReturn` False
        runNow (phaseBegins W.After VillainPhase)
        offers (isAbilityOfCard card) `shouldReturn` False
        resolve [UpgradeMessage (upgradeIdOf card) ExhaustedUpgrade]
        runNow (phaseBegins W.After PlayerPhase)
        offers (isAbilityOfCard card) `shouldReturn` False

    it "does not heal another player's identity or offer an effect when its controller cannot heal" do
      tg <- testGame ["deadpool", "spider-man"]
      inGame tg do
        let you = player1 tg
            other = player2 tg
        card <- putIntoPlay you "44029"
        setDamage other 3
        runNow (phaseBegins W.After PlayerPhase)
        offers (isAbilityOfCard card) `shouldReturn` False
        setDamage you 3
        modifyFor (IdentityEntity $ IdentityWithId you) [CannotBeHealedByPlayerCards]
        runNow (phaseBegins W.After PlayerPhase)
        offers (isAbilityOfCard card) `shouldReturn` False

    it "allows one copy for each player and rejects a second under the same player's control" do
      tg <- testGame ["deadpool", "spider-man"]
      inGame tg do
        let you = player1 tg
            other = player2 tg
        clearHand you
        _ <- putIntoPlay other "44029"
        card <- putInHand you "44029"
        energy <- putInHand you "01088"
        genius <- putInHand you "01089"
        playWith card [energy, genius]
        pass
        another <- putInHand you "44029"
        _ <- putInHand you "01088"
        _ <- putInHand you "01089"
        offers (isPlay another) `shouldReturn` False

  describe "Stick-To-Itiveness" do
    it "spends a physical resource and exhausts to ready the hero" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        card <- putInHand you "44030"
        energy <- putInHand you "01088"
        playWith card [energy]
        pass
        resolve [IdentityMessage you ExhaustedIdentity]
        physical <- putInHand you "01090"
        choose "ready with Stick-To-Itiveness" (isAbilityOfCard card)
        payWith [physical]
        pass
        isExhaustedPlayer you `shouldReturn` False
        Upgrade.upgradeExhausted <$> upgradeAttrsOf card `shouldReturn` True
        discardOf you >>= (`shouldSatisfy` elem physical)

    it "needs an exhausted hero, a physical resource and permission to ready" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        card <- putIntoPlay you "44030"
        _ <- putInHand you "01090"
        resolve [IdentityMessage you ExhaustedIdentity]
        offers (isAbilityOfCard card) `shouldReturn` False
        changeForm you
        resolve [IdentityMessage you ReadiedIdentity]
        offers (isAbilityOfCard card) `shouldReturn` False
        resolve [IdentityMessage you ExhaustedIdentity]
        clearHand you
        _ <- putInHand you "01089"
        offers (isAbilityOfCard card) `shouldReturn` False
        _ <- putInHand you "01090"
        modifyFor (IdentityEntity $ IdentityWithId you) [CannotReady]
        offers (isAbilityOfCard card) `shouldReturn` False

    it "has a deck limit of one" do
      deckBuildingError (DeckList "44001a" ["pool"] [("44030", 2)])
        `Hspec.shouldBe` Just "error.deckCopyLimit"

  describe "Ambush" do
    it "attaches to a side scheme, contributes a crisis icon and discards a non-Elite minion when the scheme is defeated" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        schemeCard <- revealNow you SideSchemes.bombScare
        let scheme = sideSchemeOf schemeCard
        ambush <- playAmbush you scheme
        crisisCount `shouldReturn` 1
        minion <- addMinion you Minions.hydraMercenary
        elite <- addMinion you Minions.modok
        runNow [SideSchemeMessage scheme (SideSchemeThreatRemoved GameRef 99)]
        choose "spring Ambush" (isAbilityOfCard ambush)
        offers (isTargetChoice minion) `shouldReturn` True
        offers (isTargetChoice elite) `shouldReturn` False
        choose "discard the non-Elite" (isTargetChoice minion)
        pass
        getsGame (HashMap.member minion . gameMinions) `shouldReturn` False
        getsGame (HashMap.member elite . gameMinions) `shouldReturn` True
        isInPlay ambush `shouldReturn` False
        crisisCount `shouldReturn` 0

    it "does not trigger for a different defeated scheme and permits only one copy per side scheme" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        first <- revealNow you SideSchemes.crowdControl
        ambush <- playAmbush you (sideSchemeOf first)
        _ <- addMinion you Minions.hydraMercenary
        another <- putInHand you "44051"
        payment <- putInHand you "01083"
        offers (isPlay another) `shouldReturn` False
        second <- revealNow you SideSchemes.breakinAndTakin
        choose "play another Ambush" (isPlay another)
        offers (isTargetChoice $ sideSchemeOf first) `shouldReturn` False
        choose "the other scheme" (isTargetChoice $ sideSchemeOf second)
        payWith [payment]
        pass
        resolve [RemoveFromPlay $ UpgradeRef $ upgradeIdOf another]
        runNow [SideSchemeMessage (sideSchemeOf second) (SideSchemeThreatRemoved GameRef 99)]
        offers (isAbilityOfCard ambush) `shouldReturn` False
        isInPlay ambush `shouldReturn` True

    it "cannot discard an Elite or protected minion, and loses both its ability and crisis icon while blank" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        scheme <- sideSchemeOf <$> revealNow you SideSchemes.bombScare
        ambush <- playAmbush you scheme
        _ <- addMinion you Minions.modok
        protected <- addMinion you Minions.hydraMercenary
        modifyFor (MinionEntity $ MinionWithId protected) [CannotBeDiscarded]
        modifyFor (UpgradeEntity $ UpgradeWithId $ upgradeIdOf ambush) [Blank]
        crisisCount `shouldReturn` 0
        runNow [SideSchemeMessage scheme (SideSchemeThreatRemoved GameRef 99)]
        offers (isAbilityOfCard ambush) `shouldReturn` False
        getsGame (HashMap.member protected . gameMinions) `shouldReturn` True

    it "excludes protected minions from its discard choices while allowing an unprotected one" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        scheme <- sideSchemeOf <$> revealNow you SideSchemes.crowdControl
        ambush <- playAmbush you scheme
        protected <- addMinion you Minions.hydraMercenary
        ordinary <- addMinion you Minions.shocker
        modifyFor (MinionEntity $ MinionWithId protected) [CannotBeDiscarded]
        runNow [SideSchemeMessage scheme (SideSchemeThreatRemoved GameRef 99)]
        choose "spring Ambush" (isAbilityOfCard ambush)
        offers (isTargetChoice protected) `shouldReturn` False
        choose "discard the unprotected minion" (isTargetChoice ordinary)
        pass
        getsGame (HashMap.member protected . gameMinions) `shouldReturn` True
        getsGame (HashMap.member ordinary . gameMinions) `shouldReturn` False

  describe "Bazooka" do
    it "discards itself to make a ranged attack counting all four encounter icon types" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        card <- putInHand you "44052"
        energy <- putInHand you "01088"
        playWith card [energy]
        pass
        _ <- putIntoPlay you "44024"
        scheme <- mainSchemeIdNow
        resolve [PlaceAccelerationToken scheme, PlaceAccelerationToken scheme]
        choose "fire Bazooka" (isAbilityOfCard card)
        isInPlay card `shouldReturn` False
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 4
        discardOf you >>= (`shouldSatisfy` elem card)

    it "ignores retaliation because the attack is ranged" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        card <- putIntoPlay you "44052"
        _ <- putIntoPlay you "44013"
        minion <- addMinion you Minions.whiplash
        choose "fire at a retaliating minion" (isAbilityOfCard card)
        choose "Whiplash" (isTargetChoice minion)
        pass
        minionDamageOf minion `shouldReturn` Just 1
        damageOf you `shouldReturn` 0

    it "does not fire for zero damage unless stunned, in which case it pays its discard cost and removes stun" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        card <- putIntoPlay you "44052"
        offers (isAbilityOfCard card) `shouldReturn` False
        resolve [IdentityMessage you IdentityStunned]
        choose "discard Bazooka to clear stun" (isAbilityOfCard card)
        pass
        isStunned you `shouldReturn` False
        isInPlay card `shouldReturn` False
        villainDamageNow `shouldReturn` 0

    it "is hero-only and has a deck limit of two" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        card <- putIntoPlay you "44052"
        _ <- putIntoPlay you "44013"
        offers (isAbilityOfCard card) `shouldReturn` False
      deckBuildingError (DeckList "44001a" ["pool"] [("44052", 3)])
        `Hspec.shouldBe` Just "error.deckCopyLimit"

  describe "Distraction" do
    it "prevents both attacks and schemes by its attached non-Elite minion and contributes a crisis icon" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        minion <- addMinion you Minions.shocker
        card <- putInHand you "44054"
        playOn card minion []
        pass
        crisisCount `shouldReturn` 1
        before <- mainSchemeThreat
        resolve [MinionMessage minion (MinionAttacks you), MinionMessage minion MinionSchemes]
        damageOf you `shouldReturn` 0
        mainSchemeThreat `shouldReturn` before
        resolve [RemoveFromPlay $ UpgradeRef $ upgradeIdOf card]
        crisisCount `shouldReturn` 0
        runNow [MinionMessage minion (MinionAttacks you)]
        pass
        damageOf you `shouldReturn` 2

    it "does not attach to Elite minions or to a minion that already has Distraction" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        elite <- addMinion you Minions.modok
        card <- putInHand you "44054"
        offers (isPlay card) `shouldReturn` False
        minion <- addMinion you Minions.hydraMercenary
        choose "play Distraction" (isPlay card)
        offers (isTargetChoice elite) `shouldReturn` False
        choose "the non-Elite" (isTargetChoice minion)
        payWith []
        pass
        another <- putInHand you "44054"
        offers (isPlay another) `shouldReturn` False

    it "allows activation again and removes the crisis icon when its text is blank" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        minion <- addMinion you Minions.hydraMercenary
        card <- putInHand you "44054"
        playOn card minion []
        pass
        modifyFor (UpgradeEntity $ UpgradeWithId $ upgradeIdOf card) [Blank]
        crisisCount `shouldReturn` 0
        runNow [MinionMessage minion (MinionAttacks you)]
        pass
        damageOf you `shouldReturn` 1

  describe "Laser Swords" do
    it "adds one ATK per printed encounter icon, excludes tokens and caps the bonus at four" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        asHero you
        card <- putInHand you "44055"
        payment <- putInHand you "44025"
        playWith card [payment]
        pass
        heroAttack you `shouldReturn` 2
        scheme <- mainSchemeIdNow
        resolve [PlaceAccelerationToken scheme, PlaceAccelerationToken scheme]
        heroAttack you `shouldReturn` 2
        _ <- putIntoPlay you "44013"
        heroAttack you `shouldReturn` 3
        _ <- putIntoPlay you "44014"
        heroAttack you `shouldReturn` 4
        _ <- putIntoPlay you "44024"
        heroAttack you `shouldReturn` 6
        modifyFor (UpgradeEntity $ UpgradeWithId $ upgradeIdOf card) [Blank]
        heroAttack you `shouldReturn` 2

    it "occupies both restricted slots and forces a discard when a third slot is used" do
      tg <- testGame ["deadpool"]
      inGame tg do
        let you = player1 tg
        clearHand you
        swords <- putInHand you "44055"
        payment <- putInHand you "44025"
        playWith swords [payment]
        pass
        isInPlay swords `shouldReturn` True
        katana <- putInHand you "44010"
        energy <- putInHand you "01088"
        playWith katana [energy]
        choose "discard the extra restricted weapon" (isTargetChoice $ upgradeIdOf katana)
        pass
        isInPlay swords `shouldReturn` True
        isInPlay katana `shouldReturn` False

    it "has a deck limit of one" do
      deckBuildingError (DeckList "44001a" ["pool"] [("44055", 2)])
        `Hspec.shouldBe` Just "error.deckCopyLimit"
