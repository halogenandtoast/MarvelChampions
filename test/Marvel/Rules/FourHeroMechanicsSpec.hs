module Marvel.Rules.FourHeroMechanicsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability (getAbilities, passesCriteria)
import Marvel.Attack (attack)
import Marvel.Message.Lifted qualified as Lifted
import Marvel.Ally.Types qualified as Ally
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Keyword (Keyword (Retaliate))
import Marvel.Matchers.Types
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier (Modifier (..))
import Marvel.Query (selectList)
import Marvel.Window qualified as W
import TestImport

newtype TestModifiers = TestModifiers (Attrs Effect)
  deriving anyclass (IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage TestModifiers where
  runMessage msg (TestModifiers a) = TestModifiers <$> runMessage msg a

giveModifiers :: EntityMatcher -> [Modifier] -> GameAppT ()
giveModifiers matcher ms = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (TestModifiers EffectAttrs
    { effectId = eid, effectCardCode = "01039", effectSource = GameRef
    , effectMatcher = matcher, effectModifiers = ms, effectEnds = Nothing })

allyDamageNow :: AllyId -> GameAppT Natural
allyDamageNow aid = getsGame (maybe 0 (Ally.allyDamage . toAttrs) . HashMap.lookup aid . gameAllies)

spec :: Spec
spec = describe "four hero pack engine mechanics" do
  it "a friendly ally attack deals its ATK without exhausting and takes consequential damage" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      card <- putIntoPlay p "01051"
      let aid = allyIdOf card
      resolve [FriendlyCharacterAttacks (AllyCharacter aid) p]
      damageOf p `shouldReturn` 2
      allyDamageNow aid `shouldReturn` 1
      getsGame (fmap (Ally.allyExhausted . toAttrs) . HashMap.lookup aid . gameAllies) `shouldReturn` Just False

  it "a stunned friendly ally loses stun in place of attacking and takes no consequential damage" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      card <- putIntoPlay p "01051"
      let aid = allyIdOf card
      resolve [GiveStatus (AllyRef aid) StunnedStatus, FriendlyCharacterAttacks (AllyCharacter aid) p]
      damageOf p `shouldReturn` 0
      allyDamageNow aid `shouldReturn` 0
      getsGame (fmap (Ally.allyStunned . toAttrs) . HashMap.lookup aid . gameAllies) `shouldReturn` Just False

  it "tough prevents a friendly attack but does not prevent its consequential damage" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      card <- putIntoPlay p "01051"
      resolve [GiveStatus (IdentityRef p) ToughStatus, FriendlyCharacterAttacks (AllyCharacter $ allyIdOf card) p]
      damageOf p `shouldReturn` 0
      allyDamageNow (allyIdOf card) `shouldReturn` 1
      (.playerIdentityTough) . toAttrs <$> getPlayer p `shouldReturn` False

  it "attack reduction applies before would-take-damage and does not reduce ability damage" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      giveModifiers (IdentityEntity $ IdentityWithId p) [AttackDamageReduction 1]
      card <- putIntoPlay p "01051"
      resolve [FriendlyCharacterAttacks (AllyCharacter $ allyIdOf card) p]
      damageOf p `shouldReturn` 1
      resolve [IdentityMessage p $ IdentityDamaged GameRef (toDamage 2 FromAbility)]
      damageOf p `shouldReturn` 3

  it "retaliate damages the friendly attacker before its consequential damage" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      giveModifiers (IdentityEntity $ IdentityWithId p) [KeywordModifier $ Retaliate 1]
      card <- putIntoPlay p "01051"
      resolve [FriendlyCharacterAttacks (AllyCharacter $ allyIdOf card) p]
      allyDamageNow (allyIdOf card) `shouldReturn` 2

  it "consequential reduction reduces attack and thwart damage to zero, never below" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      card <- putIntoPlay p "01051"
      let aid = allyIdOf card
      giveModifiers (AllyEntity $ AllyWithId aid) [ConsequentialDamageReduction 2]
      resolve [FriendlyCharacterAttacks (AllyCharacter aid) p, AllyMessage aid AllyThwartConsequential]
      allyDamageNow aid `shouldReturn` 0
      damageOf p `shouldReturn` 2

  it "generic ally attack windows see friendly attacks but minion-specific ones do not" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      card <- putIntoPlay p "01051"
      let aid = allyIdOf card
          window = W.Window W.After $ W.FriendlyCharacterAttack (AllyCharacter aid) p
      W.windowMatches (W.AllyAttacked W.After (AllyWithId aid) AnyEnemy) window GameRef `shouldReturn` True
      W.windowMatches (W.AllyAttacked W.After (AllyWithId aid) MinionEnemy) window GameRef `shouldReturn` False

  it "cannot attack and cannot defend exclude the identity's corresponding actions" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      giveModifiers (IdentityEntity $ IdentityWithId p) [CannotAttack, CannotDefend]
      selectList (IdentityWithId p <> IdentityCanBasicAttack) `shouldReturn` []
      selectList (IdentityWithId p <> IdentityCanAttack) `shouldReturn` []
      selectList (IdentityWithId p <> IdentityCanDefend) `shouldReturn` []
      resolve [FriendlyCharacterAttacks (IdentityCharacter p) p]
      damageOf p `shouldReturn` 0

  it "ignoring guard applies to the hero's attacks, while its ally still obeys guard" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- addMinion p Minions.hydraMercenary
      vid <- villainIdNow
      card <- putIntoPlay p "01051"
      giveModifiers (IdentityEntity $ IdentityWithId p) [IgnoresGuard]
      enemies <- selectList (AttackableBy $ IdentityWithId p)
      allyEnemies <- selectList (AttackableByAlly $ allyIdOf card)
      liftIO $ EnemyVillainId vid `elem` enemies `shouldBe` True
      liftIO $ EnemyVillainId vid `elem` allyEnemies `shouldBe` False

  it "a mass-form change counts as an additional form change" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      uid <- getRandom
      let window = W.Window W.After $ W.MassFormChanged p uid
      W.windowMatches (W.IdentityChangedAnyForm W.After $ IdentityWithId p) window GameRef `shouldReturn` True
      W.windowMatches (W.IdentityChangedToForm W.After $ IdentityWithId p) window GameRef `shouldReturn` False

  it "an ally cannot initiate a basic thwart using its hero's patrol exception" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      placeMainThreat 3
      card <- putIntoPlay p "01051"
      _ <- addMinion p Minions.headhuntersHenchman
      giveModifiers (IdentityEntity $ IdentityWithId p) [IgnoresPatrol, IgnoresCrisis]
      selectList SchemeYouCanThwart >>= (`shouldSatisfy` (not . null))
      ally <- getsGame (HashMap.lookup (allyIdOf card) . gameAllies)
      let thwarts = [a | Just x <- [ally], a <- getAbilities x, abilityIndex a == 300]
      length thwarts `shouldBe` 1
      for_ thwarts \a -> passesCriteria p a `shouldReturn` False

  it "mass-form flips respect a prohibition against changing form" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      uid <- getRandom
      allowed <- Lifted.runQueueT $ Lifted.capture $ Lifted.flipMassForm p uid
      length allowed `shouldBe` 1
      giveModifiers (IdentityEntity $ IdentityWithId p) [CannotChangeForm]
      blocked <- Lifted.runQueueT $ Lifted.capture $ Lifted.flipMassForm p uid
      blocked `shouldBe` []

  it "native enemy attacks against allies reduce damage before tough and preserve tough at zero" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      card <- putIntoPlay p "01051"
      let aid = allyIdOf card
      vid <- villainIdNow
      giveModifiers (AllyEntity $ AllyWithId aid) [AttackDamageReduction 1]
      resolve [GiveStatus (AllyRef aid) ToughStatus, AllyMessage aid $ AllyWasAttacked $ attack (EnemyVillainId vid) (AllyCharacter aid) 1]
      allyDamageNow aid `shouldReturn` 0
      getsGame (fmap (Ally.allyTough . toAttrs) . HashMap.lookup aid . gameAllies) `shouldReturn` Just True

  it "native enemy damage to allies is reduced exactly once" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      card <- putIntoPlay p "01051"
      let aid = allyIdOf card
      vid <- villainIdNow
      giveModifiers (AllyEntity $ AllyWithId aid) [AttackDamageReduction 1]
      resolve [AllyMessage aid $ AllyWasAttacked $ attack (EnemyVillainId vid) (AllyCharacter aid) 2]
      allyDamageNow aid `shouldReturn` 1
