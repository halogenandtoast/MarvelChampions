module Marvel.Aspect.SpdrPackSpec (spec) where

import Control.Monad (replicateM_)
import Marvel.Identity (currentIdentity)
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ally.Runner qualified as Ally
import Marvel.Ability (HasAbilities)
import Marvel.Upgrade.Types (Upgrade (..), IsUpgrade (..))
import Marvel.Hero.Runner qualified as Hero
import Marvel.Matchers hiding (ExhaustedIdentity, ExhaustedAlly)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Phase
import Marvel.Query
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types qualified as Villain
import TestImport

pack :: IO TestGame
pack = testGame ["spdr"]

isDeclareWild :: Choice -> Bool
isDeclareWild = \case
  DeclareWild _ _ -> True
  _ -> False

ownAbility :: PlayerCard -> Choice -> Bool
ownAbility card choice = isAbilityOfCard card choice && case choice of
  UseAbility ab -> abilityIndex ab == 1
  _ -> False

pay :: IdentityId -> PlayerCard -> [CardCode] -> GameAppT ()
pay p card codes = traverse (putInHand p) codes >>= playWith card

heroPowers :: IdentityId -> GameAppT [Natural]
heroPowers p = currentIdentity <$> getPlayer p >>= \case
  HeroSide hero -> sequence [Hero.getModifiedThwart (toAttrs hero), Hero.getModifiedAttack (toAttrs hero), Hero.getModifiedDefense (toAttrs hero)]
  AlterEgoSide _ -> pure []

rhinoAttacks :: IdentityId -> GameAppT ()
rhinoAttacks p = do
  _ <- stackEncounterDeck [Treacheries.imTough]
  vid <- villainIdNow
  runNow [VillainMessage vid (VillainAttacks p)]
  chooseIf isNoResponse

allyDefends :: PlayerCard -> Choice -> Bool
allyDefends card = \case
  AllyDefend aid _ -> aid == allyIdOf card
  _ -> False

upgradeCounters :: PlayerCard -> GameAppT Natural
upgradeCounters card = getsGame (maybe 0 (upgradeUses . toAttrs) . HashMap.lookup (upgradeIdOf card) . gameUpgrades)

-- Isolate an attacker's immunity while leaving the attack itself ordinary.
data EnemyImmunity = EnemyImmunity (Attrs Upgrade) Target
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, HasAbilities)
instance IsUpgrade EnemyImmunity where
  toUpgradeAttrs (EnemyImmunity attrs _) = attrs
instance HasModifiersFor EnemyImmunity where
  getModifiersFor _ target (EnemyImmunity _ enemy) = pure [CannotTakeDamage | target == enemy]
instance RunMessage EnemyImmunity where
  runMessage msg (EnemyImmunity attrs enemy) = (`EnemyImmunity` enemy) <$> runMessage msg attrs

immuneVillain :: IdentityId -> GameAppT ()
immuneVillain p = do
  card <- putIntoPlay p "01008"
  attrs <- getsGame (toAttrs . fromMaybe (error "missing immunity probe") . HashMap.lookup (upgradeIdOf card) . gameUpgrades)
  vid <- villainIdNow
  withGame_ $ entitiesL . upgradesL . at (upgradeIdOf card) ?~ Upgrade (EnemyImmunity attrs $ toRef vid)

spec :: Spec
spec = describe "SP//dr pack" do
  describe "Daredevil" do
    it "moves one damage to the enemy after defending and surviving" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putIntoPlay p "31014"
        rhinoAttacks p
        choose "Daredevil defends" (allyDefends card)
        choose "move damage" (ownAbility card)
        pass
        allyDamage <$> allyIn "31014" `shouldReturn` 1
        villainDamageNow `shouldReturn` 1
    it "cannot move damage when an attack dealt none and he has no earlier damage" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putIntoPlay p "31014"
        resolve [GiveStatus (toTarget $ allyIdOf card) ToughStatus]
        rhinoAttacks p
        choose "Daredevil defends" (allyDefends card)
        offers (ownAbility card) `shouldReturn` False
        allyDamage <$> allyIn "31014" `shouldReturn` 0
    it "cannot move damage when the attacking enemy is immune to damage" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putIntoPlay p "31014"
        immuneVillain p
        rhinoAttacks p
        choose "Daredevil defends" (allyDefends card)
        offers (ownAbility card) `shouldReturn` False
        allyDamage <$> allyIn "31014" `shouldReturn` 2
        villainDamageNow `shouldReturn` 0
    it "has two consequential damage when thwarting" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        _card <- putIntoPlay p "31014"
        placeMainThreat 3
        attrs <- allyIn "31014"
        choose "Daredevil thwarts" (isAllyThwart attrs)
        chooseMainScheme
        allyDamage <$> allyIn "31014" `shouldReturn` 2

  describe "Spider-Man Noir" do
    it "can attack with zero ATK and still takes consequential damage" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "31015"
        attrs <- allyIn "31015"
        Ally.getModifiedAttack attrs `shouldReturn` 0
        choose "Noir attacks at zero" (isAllyAttack attrs)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 0
        allyDamage <$> allyIn "31015" `shouldReturn` 1
    it "can thwart a valid scheme with zero THW and still takes consequential damage" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "31015"
        placeMainThreat 3
        attrs <- allyIn "31015"
        Ally.getModifiedThwart attrs `shouldReturn` 0
        choose "Noir thwarts at zero" (isAllyThwart attrs)
        chooseMainScheme
        pass
        mainSchemeThreat `shouldReturn` 3
        allyDamage <$> allyIn "31015" `shouldReturn` 1
    it "can attempt a confused zero-power thwart without a target, clearing confusion without consequential damage" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        card <- putIntoPlay p "31015"
        resolve [GiveStatus (toTarget $ allyIdOf card) ConfusedStatus]
        attrs <- allyIn "31015"
        choose "clear confusion with a thwart attempt" (isAllyThwart attrs)
        pass
        after <- allyIn "31015"
        allyConfused after `shouldBe` False
        allyExhausted after `shouldBe` True
        allyDamage after `shouldBe` 0
        mainSchemeThreat `shouldReturn` 0
    it "attaches the resolved treachery facedown and grows both powers" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putIntoPlay p "31015"
        treachery <- revealNow p Treacheries.imTough
        choose "Noir captures treachery" (ownAbility card)
        pass
        attrs <- allyIn "31015"
        allyHeldTreacheries attrs `shouldBe` [treachery]
        Ally.getModifiedAttack attrs `shouldReturn` 1
        Ally.getModifiedThwart attrs `shouldReturn` 1
        encounterDiscard >>= (`shouldSatisfy` notElem treachery)
    it "stops at three attached cards" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putIntoPlay p "31015"
        replicateM_ 3 do
          _ <- revealNow p Treacheries.imTough
          choose "Noir captures" (ownAbility card)
          pass
        _ <- revealNow p Treacheries.imTough
        offers (ownAbility card) `shouldReturn` False
        attrs <- allyIn "31015"
        length (allyHeldTreacheries attrs) `shouldBe` 3
        Ally.getModifiedAttack attrs `shouldReturn` 3
    it "requires another Web-Warrior card and does not count himself" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        card <- putIntoPlay p "31015"
        _ <- revealNow p Treacheries.imTough
        offers (ownAbility card) `shouldReturn` False
    it "cannot capture another player's treachery" do
      tg <- testGame ["spdr", "spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putIntoPlay p "31015"
        _ <- revealNow (player2 tg) Treacheries.imTough
        offers (ownAbility card) `shouldReturn` False
    it "does not capture a fully canceled treachery" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        noir <- putIntoPlay p "31015"
        tingle <- putIntoPlay p "31020"
        _ <- revealNow p Treacheries.imTough
        choose "Spider-Tingle" (ownAbility tingle)
        choose "damage Suit" (isTargetChoice $ characterRef $ IdentityCharacter p)
        pass
        offers (ownAbility noir) `shouldReturn` False
        allyHeldTreacheries <$> allyIn "31015" `shouldReturn` []
    it "captures a later-canceled treachery when its incite keyword already resolved" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        noir <- putIntoPlay p "31015"
        sense <- putInHand p "01004"
        resource <- putInHand p "01093"
        treachery <- revealNow p Treacheries.huntedByHydra
        -- Model a keyword resolved before a later cancellation window. Ordinary early
        -- cancellation (the previous test) cancels incite too and cannot feed Noir.
        let tid = TreacheryId $ unCardId $ ecCardId treachery
        runNow [TreacheryMessage tid TreacheryIncite]
        playWith sense [resource]
        choose "Noir captures partially resolved treachery" (ownAbility noir)
        pass
        allyHeldTreacheries <$> allyIn "31015" `shouldReturn` [treachery]

  describe "Repurpose" do
    for_ [("$label.repurposeThwart", [4,2,2]), ("$label.repurposeAttack", [2,4,2]), ("$label.repurposeDefense", [2,2,4])] \(power, expected) ->
      it ("readies the hero and boosts the selected power through the round: " <> show power) do
        tg <- pack
        let p = player1 tg
        inGame tg do
          changeForm p
          tech <- putIntoPlay p "31011"
          resolve [IdentityMessage p ExhaustedIdentity]
          card <- putInHand p "31016"
          playWith card []
          chooseIf (isTargetChoice $ upgradeIdOf tech)
          choose "choose power" (isLabel power)
          isExhaustedNow p `shouldReturn` False
          upgradeCodes >>= (`shouldSatisfy` notElem "31011")
          heroPowers p `shouldReturn` expected
          runNow [EndPhase PlayerPhase]
          heroPowers p `shouldReturn` expected
          runNow [EndRound]
          heroPowers p `shouldReturn` [2,2,2]
    it "cannot be played without a discardable Tech upgrade" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        _ <- putIntoPlay p "31010"
        card <- putInHand p "31016"
        offers (isPlay card) `shouldReturn` False

  describe "Thwip Thwip!" do
    it "damages a controlled Web-Warrior and stuns two different enemies" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        minion <- addMinion p Minions.hydraMercenary
        card <- putInHand p "31017"
        pay p card ["01088"]
        choose "damage Suit" (isTargetChoice $ characterRef $ IdentityCharacter p)
        chooseVillain
        choose "stun minion" (isTargetChoice $ enemyRef $ EnemyMinionId minion)
        damageOf p `shouldReturn` 1
        Villain.villainStunned . toAttrs <$> getVillain `shouldReturn` True
        selectAny (EnemyWithId (EnemyMinionId minion) <> StunnedEnemy) `shouldReturn` True
    it "places both status cards on one Steady enemy" do
      tg <- testGameAgainst "01113" ["spdr"]
      let p = player1 tg
      inGame tg do
        changeForm p
        minion <- addMinion p Minions.smRhino
        card <- putInHand p "31017"
        pay p card ["01088"]
        choose "damage Suit" (isTargetChoice $ characterRef $ IdentityCharacter p)
        choose "first stun" (isTargetChoice $ enemyRef $ EnemyMinionId minion)
        choose "second stun" (isTargetChoice $ enemyRef $ EnemyMinionId minion)
        attrs <- getsGame (toAttrs . fromMaybe (error "Rhino") . HashMap.lookup minion . gameMinions)
        (minionStunned attrs, minionAdditionalStun attrs) `shouldBe` (True, True)
    it "can pay the dealt-damage cost with a tough character" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        resolve [IdentityMessage p IdentityBecomeTough]
        card <- putInHand p "31017"
        pay p card ["01088"]
        choose "damage tough Suit" (isTargetChoice $ characterRef $ IdentityCharacter p)
        chooseVillain
        pass
        damageOf p `shouldReturn` 0
        Villain.villainStunned . toAttrs <$> getVillain `shouldReturn` True
    it "cannot be played when no enemy can receive a stun status card" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        vid <- villainIdNow
        resolve [VillainMessage vid (VillainStunned GameRef)]
        card <- putInHand p "31017"
        _ <- putInHand p "01088"
        offers (isPlay card) `shouldReturn` False
    it "cannot use an uncontrolled character or be played without a controlled Web-Warrior" do
      tg <- testGame ["spider-man", "spdr"]
      let p = player1 tg
      inGame tg do
        changeForm p
        _ <- putIntoPlay (player2 tg) "27017"
        card <- putInHand p "31017"
        _ <- putInHand p "01088"
        offers (isPlay card) `shouldReturn` False

  describe "Energy Barrier reprint" do
    it "has three reflection counters and prevents one damage with a reflected point" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        card <- putIntoPlay p "31018"
        upgradeCounters card `shouldReturn` 3
        runNow $ damageIdentity p GameRef (toDamage 2 FromAbility)
        choose "Energy Barrier" (ownAbility card)
        chooseVillain
        pass
        damageOf p `shouldReturn` 1
        villainDamageNow `shouldReturn` 1
        upgradeCounters card `shouldReturn` 2

  describe "Forcefield Generator" do
    it "forcibly prevents damage and spends only as many counters as needed" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        card <- putIntoPlay p "31019"
        upgradeCounters card `shouldReturn` 6
        runNow $ damageIdentity p GameRef (toDamage 2 FromAbility)
        choose "Forcefield Generator forced interrupt" (ownAbility card)
        pass
        damageOf p `shouldReturn` 0
        upgradeCounters card `shouldReturn` 4
    it "discards when depleted and lets the unprevented remainder through" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        card <- putIntoPlay p "31019"
        runNow $ damageIdentity p GameRef (toDamage 8 FromAbility)
        choose "Forcefield Generator forced interrupt" (ownAbility card)
        pass
        damageOf p `shouldReturn` 2
        upgradeCodes >>= (`shouldSatisfy` notElem "31019")
    it "enforces max one per player" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "31019"
        second <- putInHand p "31019"
        _ <- putInHand p "01088"
        _ <- putInHand p "01089"
        offers (isPlay second) `shouldReturn` False

  describe "Spider-Tingle" do
    it "cancels a treachery's When Revealed effect, deals its cost and discards itself" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putIntoPlay p "31020"
        _ <- revealNow p Treacheries.imTough
        getsGame gameFocusedCards `shouldReturn` []
        choose "Spider-Tingle" (ownAbility card)
        getsGame gameFocusedCards `shouldReturn` []
        choose "damage Suit" (isTargetChoice $ characterRef $ IdentityCharacter p)
        pass
        damageOf p `shouldReturn` 1
        upgradeCodes >>= (`shouldSatisfy` notElem "31020")
        Villain.villainTough . toAttrs <$> getVillain `shouldReturn` False
    for_ [Treacheries.huntedByHydra, Treacheries.theWarsBeenBrought] \treacheryDef ->
      it ("cancels early printed keywords without feeding Noir: " <> show (toCardCode treacheryDef)) do
        tg <- pack
        let p = player1 tg
        inGame tg do
          changeForm p
          _noir <- putIntoPlay p "31015"
          tingle <- putIntoPlay p "31020"
          _ <- stackEncounterDeck [Minions.hydraMercenary]
          _ <- revealNow p treacheryDef
          getsGame gameFocusedCards `shouldReturn` []
          choose "Spider-Tingle before revelation" (ownAbility tingle)
          choose "damage Suit" (isTargetChoice $ characterRef $ IdentityCharacter p)
          pass
          damageOf p `shouldReturn` 1
          mainSchemeThreat `shouldReturn` 0
          selectListCount (MinionEngagedWith $ IdentityWithId p) `shouldReturn` 0
          allyHeldTreacheries <$> allyIn "31015" `shouldReturn` []
          upgradeCodes >>= (`shouldSatisfy` notElem "31020")
    it "still pays damage but stays in play when the encounter card is a minion" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putIntoPlay p "31020"
        _ <- revealNow p Minions.hydraMercenary
        choose "Spider-Tingle" (ownAbility card)
        choose "damage Suit" (isTargetChoice $ characterRef $ IdentityCharacter p)
        pass
        damageOf p `shouldReturn` 1
        upgradeCodes >>= (`shouldSatisfy` elem "31020")
        selectListCount (MinionEngagedWith $ IdentityWithId p) `shouldReturn` 1
    it "requires a Web-Warrior character under your control" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        card <- putIntoPlay p "31020"
        _ <- revealNow p Treacheries.imTough
        offers (ownAbility card) `shouldReturn` False

  describe "Spider-Ham" do
    it "takes boost-icon damage after attacking, with no consequential damage" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putIntoPlay p "31021"
        _ <- stackEncounterDeck [Treacheries.shadowOfThePast]
        attrs <- allyIn "31021"
        choose "Spider-Ham attacks" (isAllyAttack attrs)
        chooseVillain
        choose "Spider-Ham forced response" (ownAbility card)
        pass
        allyDamage <$> allyIn "31021" `shouldReturn` 2
        villainDamageNow `shouldReturn` 2
    it "also discards after thwarting and takes zero damage from zero boost icons" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putIntoPlay p "31021"
        _ <- stackEncounterDeck [Treacheries.imTough]
        placeMainThreat 3
        attrs <- allyIn "31021"
        choose "Spider-Ham thwarts" (isAllyThwart attrs)
        chooseMainScheme
        choose "Spider-Ham forced response" (ownAbility card)
        pass
        allyDamage <$> allyIn "31021" `shouldReturn` 0
        mainSchemeThreat `shouldReturn` 1
    it "requires a controlled Web-Warrior card to play" do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        card <- putInHand p "31021"
        _ <- putInHand p "01088"
        _ <- putInHand p "01089"
        offers (isPlay card) `shouldReturn` False

  describe "Spider-Man (Otto Octavius)" do
    it "readies a Tech upgrade and draws a card after being played from hand" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        tech <- putIntoPlay p "31011"
        resolve [UpgradeMessage (upgradeIdOf tech) ExhaustedUpgrade]
        card <- putInHand p "31022"
        pay p card ["01088"]
        before <- length <$> handOf p
        choose "Otto response" (ownAbility card)
        choose "ready Tech" (isTargetChoice $ upgradeIdOf tech)
        upgradeIsExhausted (upgradeIdOf tech) `shouldReturn` False
        length <$> handOf p `shouldReturn` before + 1
    it "readies a non-Tech upgrade without drawing" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        upgrade <- putIntoPlay p "31010"
        resolve [UpgradeMessage (upgradeIdOf upgrade) ExhaustedUpgrade]
        card <- putInHand p "31022"
        pay p card ["01088"]
        before <- length <$> handOf p
        choose "Otto response" (ownAbility card)
        choose "ready non-Tech" (isTargetChoice $ upgradeIdOf upgrade)
        upgradeIsExhausted (upgradeIdOf upgrade) `shouldReturn` False
        length <$> handOf p `shouldReturn` before
    it "does not trigger when merely put into play" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        tech <- putIntoPlay p "31011"
        resolve [UpgradeMessage (upgradeIdOf tech) ExhaustedUpgrade]
        card <- putIntoPlay p "31022"
        offers (ownAbility card) `shouldReturn` False
        upgradeIsExhausted (upgradeIdOf tech) `shouldReturn` True

  describe "Limitless Stamina and Unshakable" do
    it "readies a fourteen printed-HP hero" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        resolve [IdentityMessage p ExhaustedIdentity]
        card <- putInHand p "31023"
        pay p card ["01093"]
        isExhaustedNow p `shouldReturn` False
    it "uses printed hit points, so Endurance does not qualify a twelve-HP identity" do
      tg <- testGame ["spider-ham"]
      let p = player1 tg
      inGame tg do
        changeForm p
        _ <- putIntoPlay p "05023"
        first <- putInHand p "31023"
        second <- putInHand p "31024"
        _ <- putInHand p "01088"
        offers (isPlay first) `shouldReturn` False
        offers (isPlay second) `shouldReturn` False
    it "grants Steady, requiring two stuns and discarding both to replace an attack" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        card <- putInHand p "31024"
        pay p card ["01093"]
        resolve [IdentityMessage p IdentityStunned]
        identityMatches StunnedIdentity p `shouldReturn` False
        resolve [IdentityMessage p IdentityStunned]
        identityMatches StunnedIdentity p `shouldReturn` True
        choose "attack canceled by both status cards" (isHeroAttack p)
        identityMatches StunnedIdentity p `shouldReturn` False
        villainDamageNow `shouldReturn` 0

  describe "Clarity of Purpose" do
    it "generates a wild resource by exhausting and dealing damage to its attached identity" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        clarity <- putIntoPlay p "31029"
        card <- putInHand p "31023"
        choose "play Stamina" (isPlay card)
        choose "Clarity resource" (ownAbility clarity)
        choose "damage attached Suit" (isTargetChoice $ characterRef $ IdentityCharacter p)
        whenM (offers isDeclareWild) $ declareWildAs Energy
        chooseIf isFinishPayment
        pass
        damageOf p `shouldReturn` 1
        upgradeIsExhausted (upgradeIdOf clarity) `shouldReturn` True
    it "attaches to a friendly ally and damages that ally to pay" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        changeForm p
        ally <- putIntoPlay p "31014"
        clarity <- putInHand p "31029"
        resources <- traverse (putInHand p) ["01093"]
        playOn clarity (allyIdOf ally) resources
        card <- putInHand p "31023"
        choose "play Stamina" (isPlay card)
        choose "Clarity resource" (ownAbility clarity)
        choose "damage Daredevil" (isTargetChoice $ characterRef $ AllyCharacter $ allyIdOf ally)
        whenM (offers isDeclareWild) $ declareWildAs Energy
        chooseIf isFinishPayment
        pass
        allyDamage <$> allyIn "31014" `shouldReturn` 1
        damageOf p `shouldReturn` 0
    it "cannot attach a second copy to the same character" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "31029"
        second <- putInHand p "31029"
        _ <- putInHand p "01093"
        offers (isPlay second) `shouldReturn` False
    it "cannot generate a Hero Resource in alter-ego form" do
      tg <- pack
      let p = player1 tg
      inGame tg do
        clarity <- putIntoPlay p "31029"
        card <- putInHand p "31020"
        _ <- putInHand p "01093"
        choose "play Spider-Tingle" (isPlay card)
        offers (ownAbility clarity) `shouldReturn` False
