module Marvel.Rules.CyclopsMechanicsSpec (spec) where

import Marvel.Ability qualified as A
import Marvel.Attack qualified as Attack
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Choice qualified as Choices
import Marvel.Cost qualified as Cost
import Marvel.Criteria
import Marvel.Payment.Types (Payment (ResourcePayment))
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet
import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Phase
import Marvel.Upgrade.Types (IsUpgrade, Upgrade (..))
import Marvel.Window qualified as W
import TestImport

newtype TestModifiers = TestModifiers (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage TestModifiers where
  runMessage msg (TestModifiers a) = TestModifiers <$> runMessage msg a

modifiersOn :: EntityMatcher -> [Modifier] -> GameAppT ()
modifiersOn matcher modifiers = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (TestModifiers EffectAttrs
    { effectId = eid
    , effectCardCode = "01039"
    , effectSource = GameRef
    , effectMatcher = matcher
    , effectModifiers = modifiers
    , effectEnds = Nothing
    })

-- A source that removes Temporary, as Field Commander does, without depending on
-- Cyclops's card implementation when exercising the keyword itself.
newtype TemporaryUpgrade = TemporaryUpgrade (Attrs Upgrade)
  deriving anyclass (IsUpgrade, A.HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage TemporaryUpgrade where
  runMessage msg (TemporaryUpgrade attrs) = TemporaryUpgrade <$> runMessage msg attrs

temporaryUpgrade :: IdentityId -> PlayerCard -> Upgrade
temporaryUpgrade you card = Upgrade $ TemporaryUpgrade $
  (toAttrs $ createUpgrade you card) {upgradeCardDef = (getCardDef card) {cdKeywords = HashSet.insert Temporary (cdKeywords $ getCardDef card)}}

newtype KeywordSuppressor = KeywordSuppressor (Attrs Upgrade)
  deriving anyclass (IsUpgrade, A.HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage KeywordSuppressor where
  runMessage msg (KeywordSuppressor a) = KeywordSuppressor <$> runMessage msg a

instance HasModifiersFor KeywordSuppressor where
  getModifiersFor _ (UpgradeRef _) _ = pure [LoseKeyword Temporary]
  getModifiersFor _ _ _ = pure []

resolveChoices :: IdentityId -> [Choice] -> GameAppT ()
resolveChoices you choices = resolve =<< concatMapM (Choices.choiceMessages you) choices

allyDamageOf :: AllyId -> GameAppT (Maybe Natural)
allyDamageOf aid = getsGame (fmap (allyDamage . toAttrs) . lookup aid . gameAllies)

newtype ScopedResource = ScopedResource (Attrs Upgrade)
  deriving anyclass IsUpgrade
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance HasModifiersFor ScopedResource

instance A.HasAbilities ScopedResource where
  getAbilities (ScopedResource attrs) =
    [A.ability attrs 1 A.Resource (OwnsThis <> PayingForAbility "01001a" 77) Cost.ExhaustCost (Pay $ ResourcePayment Energy)]

instance RunMessage ScopedResource where
  runMessage msg (ScopedResource attrs) = ScopedResource <$> runMessage msg attrs

newtype EnemyPenalty = EnemyPenalty (Attrs Upgrade)
  deriving anyclass (IsUpgrade, A.HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage EnemyPenalty where
  runMessage msg (EnemyPenalty attrs) = EnemyPenalty <$> runMessage msg attrs

instance HasModifiersFor EnemyPenalty where
  getModifiersFor _ target (EnemyPenalty attrs) = pure
    [AttackPenalty (upgradeUses attrs) | Just target == (enemyRef <$> upgradeAttachedEnemy attrs)]

giveEnemyPenalty :: IdentityId -> EnemyId -> Natural -> GameAppT ()
giveEnemyPenalty you enemy n = do
  source <- putIntoPlay you "01007"
  let uid = upgradeIdOf source
      attrs = (toAttrs $ createUpgrade you source) {upgradeAttachedEnemy = Just enemy, upgradeUses = n}
  withGame_ $ entitiesL . upgradesL . at uid ?~ Upgrade (EnemyPenalty attrs)

minionToughCount :: MinionId -> GameAppT Natural
minionToughCount mid = getsGame $ maybe 0 countTough . lookup mid . gameMinions
 where
  countTough minion = let attrs = toAttrs minion
    in (if minionTough attrs then 1 else 0) + minionAdditionalTough attrs

spec :: Spec
spec = describe "Cyclops engine mechanics" do
  it "adds attack-only damage to each instance, leaving ability damage unchanged" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      minion <- addMinion p Minions.abomination
      modifiersOn (MinionEntity $ MinionWithId minion) [AttackDamageTakenModifier 1]
      resolveChoices p [DamageEnemy (MinionRef minion) (IdentityRef p) (toDamage 1 FromAbility)]
      minionDamageOf minion `shouldReturn` Just 1
      resolveChoices p [DamageEnemy (MinionRef minion) (IdentityRef p) (toDamage 1 $ FromPlayerAttack p)]
      minionDamageOf minion `shouldReturn` Just 3

  it "does not turn zero attack damage into positive damage" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      minion <- addMinion p Minions.shocker
      modifiersOn (MinionEntity $ MinionWithId minion) [AttackDamageTakenModifier 1]
      resolveChoices p [DamageEnemy (MinionRef minion) (IdentityRef p) (toDamage 0 $ FromPlayerAttack p)]
      minionDamageOf minion `shouldReturn` Just 0

  it "gives attacks against an enemy overkill while preserving ordinary ability damage" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      minion <- addMinion p Minions.shocker
      modifiersOn (MinionEntity $ MinionWithId minion) [AttacksAgainstGainKeyword Overkill]
      resolveChoices p [DamageEnemy (MinionRef minion) (IdentityRef p) (toDamage 5 $ FromPlayerAttack p)]
      villainDamageNow `shouldReturn` 2
      other <- addMinion p Minions.shocker
      modifiersOn (MinionEntity $ MinionWithId other) [AttacksAgainstGainKeyword Overkill]
      resolveChoices p [DamageEnemy (MinionRef other) (IdentityRef p) (toDamage 5 FromAbility)]
      villainDamageNow `shouldReturn` 2

  it "uses modified THW for the hero's basic attack against a Befuddled enemy" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      minion <- addMinion p Minions.shocker
      modifiersOn (MinionEntity $ MinionWithId minion) [AttackWithThwart]
      modifiersOn (IdentityEntity $ IdentityWithId p) [ThwartModifier 1]
      choose "basic attack" (isHeroAttack p)
      choose "Shocker" (isTargetChoice minion)
      minionDamageOf minion `shouldReturn` Just 2

  it "uses an ally's THW for attack damage and keeps its ATK consequential icons" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      ally <- allyIdOf <$> putIntoPlay p "01002"
      minion <- addMinion p Minions.hydraMercenary
      modifiersOn (MinionEntity $ MinionWithId minion) [AttackWithThwart]
      modifiersOn (AllyEntity $ AllyWithId ally) [ThwartModifier 1]
      -- Black Cat has no ATK consequence icon and one THW consequence icon.
      -- The owner's ruling keeps ATK consequence even when substituting THW.
      resolve [AllyMessage ally $ AllyAttackLands (EnemyMinionId minion) (toDamage 1 $ FromAllyAttack ally)]
      minionDamageOf minion `shouldReturn` Just 2
      allyDamageOf ally `shouldReturn` Just 0

  it "reduces only attack consequential damage, to a minimum of zero" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      ally <- allyIdOf <$> putIntoPlay p "01068"
      modifiersOn (AllyEntity $ AllyWithId ally) [AttackConsequentialDamageReduction 3]
      resolve [AllyMessage ally AllyAttackConsequential]
      allyDamageOf ally `shouldReturn` Just 0
      resolve [AllyMessage ally AllyThwartConsequential]
      allyDamageOf ally `shouldReturn` Just 1

  it "discards a temporary upgrade in the round-ending interrupt window" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      card <- putIntoPlay p "01007"
      let uid = upgradeIdOf card
      withGame_ $ entitiesL . upgradesL . at uid ?~ temporaryUpgrade p card
      resolve [CheckWindows [W.Window W.When W.RoundEnded]]
      getsGame (HashMap.member uid . gameUpgrades) `shouldReturn` False

  it "does not discard a temporary upgrade while the keyword is removed" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      card <- putIntoPlay p "01007"
      let uid = upgradeIdOf card
          temporary = temporaryUpgrade p card
      withGame_ $ entitiesL . upgradesL . at uid ?~ temporary
      source <- putIntoPlay p "01007"
      let sid = upgradeIdOf source
      withGame_ $ entitiesL . upgradesL . at sid ?~ Upgrade (KeywordSuppressor $ toAttrs $ createUpgrade p source)
      resolve [CheckWindows [W.Window W.When W.RoundEnded]]
      getsGame (HashMap.member uid . gameUpgrades) `shouldReturn` True

  it "matches an identity's named ability without matching an upgrade's same index" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      let matcher = W.UsesAbility W.When (IdentityWithId p) 1
      W.windowMatches matcher (W.Window W.When $ W.UsedCardAbility p (IdentityRef p) 1) (IdentityRef p) `shouldReturn` True
      card <- putIntoPlay p "01007"
      W.windowMatches matcher (W.Window W.When $ W.UsedCardAbility p (toSource $ upgradeIdOf card) 1) (IdentityRef p) `shouldReturn` False

  it "offers an enters-play matcher when an ally was put into play" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      ally <- allyIdOf <$> putIntoPlay p "01068"
      W.windowMatches (W.AllyEntersPlay W.After $ AllyWithId ally)
        (W.Window W.After $ W.AllyEnteredPlay ally) (IdentityRef p) `shouldReturn` True

  it "counts a scoped resource for its named ability before payment and offers it during payment" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      changeForm p
      clearHand p
      card <- putIntoPlay p "01007"
      let uid = upgradeIdOf card
          named = A.ability p 77 A.Action IsSelf (Cost.ResourceCost Nothing) (Run [])
          other = A.ability p 78 A.Action IsSelf (Cost.ResourceCost Nothing) (Run [])
      withGame_ $ entitiesL . upgradesL . at uid ?~ Upgrade (ScopedResource $ toAttrs $ createUpgrade p card)
      Cost.passesCanAffordCost p named `shouldReturn` True
      Cost.passesCanAffordCost p other `shouldReturn` False
      messages <- Choices.choiceMessages p (UseAbility named)
      runNow messages
      offers (isAbilityOf uid) `shouldReturn` True

  it "takes the priority turn first without changing first-player order or adding a second turn" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
        q = fromMaybe (error "missing second player") (atMay tg.seats 1)
    inGame tg do
      order <- getsGame gamePlayerOrder
      modifiersOn (IdentityEntity $ IdentityWithId q) [TakesFirstPlayerPhaseTurn]
      clearQueue
      runNow [BeginPhase PlayerPhase]
      getActivePlayerId `shouldReturn` q
      getsGame gamePlayerOrder `shouldReturn` order
      pending <- withQueue $ \messages -> (messages, [who | IdentityMessage who BeginTurn <- messages])
      pending `shouldBe` [p]

  it "clamps a minion's negative ATK only after boost icons and attack-specific increases" do
    for_ [0 :: Int, 2] \change -> do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        minion <- addMinion p Minions.hydraMercenary
        modifiersOn (MinionEntity $ MinionWithId minion)
          [AttackPenalty $ 2 + fromIntegral change, KeywordModifier Villainous]
        void $ setEncounterDeck [Treacheries.explosion, Treacheries.explosion] []
        runNow [MinionMessage minion $ MinionAttacks p]
        getsGame gameState `shouldReturn` InProgress
        resolve [MinionMessage minion $ MinionAttackChanged change]
        pass
        damageOf p `shouldReturn` 1

  it "clamps a villain's negative ATK only after boost icons and attack-specific increases" do
    for_ [0 :: Int, 2] \change -> do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        villain <- villainIdNow
        giveEnemyPenalty p (EnemyVillainId villain) (3 + fromIntegral change)
        void $ setEncounterDeck [Treacheries.explosion, Treacheries.explosion] []
        runNow [VillainMessage villain $ VillainAttacks p]
        getsGame gameState `shouldReturn` InProgress
        resolve [VillainMessage villain $ VillainAttackChanged change]
        pass
        damageOf p `shouldReturn` 1

  it "refreshes the signed base without losing attack changes or gained flags" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      villain <- villainIdNow
      let original = (Attack.attackWith [Overkill, Piercing] (EnemyVillainId villain) (IdentityCharacter p) 0)
            {Attack.attackChange = 2, Attack.attackPrevented = True}
          refreshed = Attack.withAttackBase (-1) original
      Attack.attackValue refreshed `shouldBe` 1
      Attack.attackChange refreshed `shouldBe` 2
      Attack.attackOverkill refreshed `shouldBe` True
      Attack.attackPiercing refreshed `shouldBe` True
      Attack.attackPrevented refreshed `shouldBe` True

  it "loads attacks saved before signed attack intermediates were tracked" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      villain <- villainIdNow
      let original = Attack.attack (EnemyVillainId villain) (IdentityCharacter p) 3
          old = case toJSON original of
            Object fields -> Object $ KeyMap.delete "attackBaseReduction" fields
            other -> other
      case fromJSON old of
        Error problem -> fail problem
        Success restored -> Attack.attackValue restored `shouldBe` 3

  it "piercing discards every tough card and does not leave phantom additional tough cards" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      minion <- addMinion p Minions.armadillo
      minionToughCount minion `shouldReturn` 1
      resolve [GiveStatus (MinionRef minion) ToughStatus | _ <- [1 :: Int, 2]]
      minionToughCount minion `shouldReturn` 3
      resolveChoices p [DamageEnemy (MinionRef minion) (IdentityRef p) $ withPiercing $ toDamage 3 $ FromPlayerAttack p]
      minionDamageOf minion `shouldReturn` Just 3
      minionToughCount minion `shouldReturn` 0
      resolve [GiveStatus (MinionRef minion) ToughStatus]
      minionToughCount minion `shouldReturn` 1
      resolveChoices p [DamageEnemy (MinionRef minion) (IdentityRef p) $ toDamage 1 $ FromPlayerAttack p]
      minionDamageOf minion `shouldReturn` Just 3
      minionToughCount minion `shouldReturn` 0
      resolveChoices p [DamageEnemy (MinionRef minion) (IdentityRef p) $ toDamage 1 $ FromPlayerAttack p]
      minionDamageOf minion `shouldReturn` Just 4

  it "zero piercing damage keeps stacked tough and ordinary damage discards only one" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      minion <- addMinion p Minions.armadillo
      minionToughCount minion `shouldReturn` 1
      resolve [GiveStatus (MinionRef minion) ToughStatus]
      resolveChoices p [DamageEnemy (MinionRef minion) (IdentityRef p) $ withPiercing $ toDamage 0 $ FromPlayerAttack p]
      minionToughCount minion `shouldReturn` 2
      minionDamageOf minion `shouldReturn` Just 0
      resolveChoices p [DamageEnemy (MinionRef minion) (IdentityRef p) $ toDamage 1 $ FromPlayerAttack p]
      minionToughCount minion `shouldReturn` 1
      minionDamageOf minion `shouldReturn` Just 0
