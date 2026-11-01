module Marvel.Rules.DeadpoolEliminationSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability (windowAbility)
import Marvel.Ability.Types (AbilityType (..), HasAbilities (..))
import Marvel.Attack (attackDefended)
import Marvel.Cost.Types (Cost (NoCost))
import Marvel.Effect.Types (Attrs (..), Effect (..), IsEffect)
import Marvel.Keyword (Keyword (..))
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Modifier
import Marvel.Upgrade.Types qualified as Upgrade
import Marvel.Window qualified as W
import TestImport

newtype GroupModifiers = GroupModifiers (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance RunMessage GroupModifiers where
  runMessage message (GroupModifiers attrs) = GroupModifiers <$> runMessage message attrs

newtype DamageWindowProbe = DamageWindowProbe (Attrs Upgrade.Upgrade)
  deriving anyclass (Upgrade.IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance HasAbilities DamageWindowProbe where
  getAbilities (DamageWindowProbe attrs) =
    [windowAbility attrs 1 (W.WouldTakeDamage You FromAttack AnyValue) Interrupt NoCost (Run [])]

instance RunMessage DamageWindowProbe where
  runMessage message (DamageWindowProbe attrs) = DamageWindowProbe <$> runMessage message attrs

groupModifiers :: MinionId -> [Modifier] -> GameAppT ()
groupModifiers mid modifiers = do
  eid <- getRandom
  withGame_ $ entitiesL . effectsL . at eid ?~ Effect (GroupModifiers EffectAttrs
    { effectId = eid, effectCardCode = "01039", effectSource = GameRef
    , effectMatcher = MinionEntity (MinionWithId mid), effectModifiers = modifiers, effectEnds = Nothing })

data NestedAttackProbe = NestedAttackProbe (Attrs Upgrade.Upgrade) MinionId IdentityId Bool Bool
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, HasModifiersFor)

instance Upgrade.IsUpgrade NestedAttackProbe where
  toUpgradeAttrs (NestedAttackProbe attrs _ _ _ _) = attrs

instance HasAbilities NestedAttackProbe where
  getAbilities (NestedAttackProbe attrs mid _ used _) =
    [windowAbility attrs 1 (W.HeroDefended W.When (IdentityWithId $ Upgrade.upgradeController attrs) (EnemyWithId $ EnemyMinionId mid)) Interrupt NoCost (RunAbility (UpgradeRef $ Upgrade.upgradeId attrs) 1) | not used]

instance RunMessage NestedAttackProbe where
  runMessage message (NestedAttackProbe attrs mid target used sawParent) = L.runQueueT $ case message of
    RanAbility _ (UpgradeRef uid) 1 _ _ | uid == Upgrade.upgradeId attrs -> do
      L.enemyAttacks mid target
      L.cardSpecific attrs "afterNestedAttack" ()
      pure $ NestedAttackProbe attrs mid target True sawParent
    CardSpecific (UpgradeRef uid) "afterNestedAttack" _ | uid == Upgrade.upgradeId attrs -> do
      parent <- getEnemyAttack (EnemyMinionId mid)
      pure $ NestedAttackProbe attrs mid target used (maybe False attackDefended parent)
    _ -> (\next -> NestedAttackProbe next mid target used sawParent) <$> runMessage message attrs

useWhirlwind :: MinionId -> GameAppT ()
useWhirlwind mid = choose "resolve Whirlwind's forced interrupt" \case
  UseAbility ability -> abilitySource ability == MinionRef mid && abilityIndex ability == 1
  _ -> False

spec :: Spec
spec = describe "Deadpool multiplayer attack regression" do
  it "Whirlwind gives each hero a separate attack after the first hero defends" do
    tg <- testGame ["spider-man", "captain-marvel"]
    inGame tg do
      let first = player1 tg
          second = player2 tg
      clearHand first
      clearHand second
      runNow [IdentityMessage first (ChangedToForm A), IdentityMessage second (ChangedToForm A)]
      pass
      whirlwind <- addMinion first Minions.whirlwind
      pass
      runNow [MinionMessage whirlwind (MinionAttacks first)]
      useWhirlwind whirlwind
      choose "defend the first attack" \case
        Defend (EnemyMinionId mid) -> mid == whirlwind
        _ -> False
      damageOf first `shouldReturn` 0
      damageOf second `shouldReturn` 0
      fmap fst <$> asked `shouldReturn` Just second
      snapshot <- getGame
      case fromJSON (toJSON snapshot) of
        Error err -> error err
        Success restored -> withGame_ $ const restored
      pass
      damageOf first `shouldReturn` 0
      damageOf second `shouldReturn` 2
      runNow [MinionMessage whirlwind (MinionAttacks first)]
      useWhirlwind whirlwind
      pass
      damageOf first `shouldReturn` 2
      damageOf second `shouldReturn` 4

  it "keeps overkill damage in the same damage step as every hero's attack" do
    tg <- testGame ["captain-marvel", "iron-man"]
    inGame tg do
      let first = player1 tg
          second = player2 tg
      clearHand first
      clearHand second
      runNow [IdentityMessage first (ChangedToForm A), IdentityMessage second (ChangedToForm A)]
      pass
      maria <- putIntoPlay first "01067"
      whirlwind <- addMinion first Minions.whirlwind
      groupModifiers whirlwind [AttackModifier 3, KeywordModifier Overkill]
      runNow [MinionMessage whirlwind (MinionAttacks first)]
      useWhirlwind whirlwind
      choose "Maria defends" \case
        AllyDefend aid (EnemyMinionId mid) -> aid == allyIdOf maria && mid == whirlwind
        _ -> False
      pass
      damageOf first `shouldReturn` 3
      damageOf second `shouldReturn` 5

  it "redirects one attack to an enemy while the other hero takes their attack damage" do
    tg <- testGame ["psylocke", "captain-marvel"]
    inGame tg do
      let first = player1 tg
          second = player2 tg
      clearHand first
      clearHand second
      runNow [IdentityMessage first (ChangedToForm A), IdentityMessage second (ChangedToForm A)]
      pass
      redirect <- putInHand first "34033"
      payment <- putInHand first "01088"
      whirlwind <- addMinion first Minions.whirlwind
      runNow [MinionMessage whirlwind (MinionAttacks first)]
      useWhirlwind whirlwind
      playWith redirect [payment]
      chooseVillain
      pass
      damageOf first `shouldReturn` 0
      damageOf second `shouldReturn` 2
      villainDamageNow `shouldReturn` 2

  it "restores a defended parent attack after a nested Whirlwind activation and its cleanup" do
    tg <- testGame ["spider-man", "captain-marvel"]
    inGame tg do
      let first = player1 tg
          second = player2 tg
      clearHand first
      clearHand second
      runNow [IdentityMessage first (ChangedToForm A), IdentityMessage second (ChangedToForm A)]
      pass
      whirlwind <- addMinion first Minions.whirlwind
      probe <- putIntoPlay first "01008"
      attrs <- getsGame $ toAttrs . fromMaybe (error "missing nested probe") . HashMap.lookup (upgradeIdOf probe) . gameUpgrades
      withGame_ $ entitiesL . upgradesL . at (upgradeIdOf probe) ?~ Upgrade.Upgrade (NestedAttackProbe attrs whirlwind second False False)
      runNow [MinionMessage whirlwind (MinionAttacks first)]
      useWhirlwind whirlwind
      choose "defend the parent attack" \case
        Defend (EnemyMinionId mid) -> mid == whirlwind
        _ -> False
      choose "begin the nested activation" (isAbilityOfCard probe)
      useWhirlwind whirlwind
      pass
      damageOf first `shouldReturn` 2
      damageOf second `shouldReturn` 4
      fixture <- getsGame $ fromMaybe (error "missing nested probe after attack") . HashMap.lookup (upgradeIdOf probe) . gameUpgrades
      case fromJSON (toJSON fixture) of
        Success (NestedAttackProbe _ _ _ True True) -> pure ()
        _ -> error "nested attack did not restore the defended parent snapshot"

  it "finishes lethal enemy attack defeat windows after the eliminated identity leaves play" do
    tg <- testGame ["captain-marvel", "spider-man"]
    inGame tg do
      let victim = player1 tg
          survivor = player2 tg
      clearHand victim
      clearHand survivor
      changeForm victim
      pass
      setDamage victim 11
      mercenary <- addMinion victim Minions.hydraMercenary
      pass
      runNow [MinionMessage mercenary (MinionAttacks victim)]
      pass
      getsGame (HashMap.member victim . gamePlayers) `shouldReturn` False
      getsGame (HashMap.member survivor . gamePlayers) `shouldReturn` True
      damageOf survivor `shouldReturn` 0

  it "Whirlwind declares every defense before damage, including a hero defending another player" do
    tg <- testGame ["captain-marvel", "iron-man"]
    inGame tg do
      let attacked = player1 tg
          defender = player2 tg
      clearHand attacked
      clearHand defender
      runNow [IdentityMessage attacked (ChangedToForm A), IdentityMessage defender (ChangedToForm A)]
      pass
      setDamage defender 8
      whirlwind <- addMinion attacked Minions.whirlwind
      pass
      runNow [MinionMessage whirlwind (MinionAttacks attacked)]
      useWhirlwind whirlwind
      choose "ask Iron Man to defend" (isTargetChoice defender)
      choose "Iron Man defends" (isTargetChoice defender)
      damageOf defender `shouldReturn` 8
      fmap fst <$> asked `shouldReturn` Just defender
      pass
      getsGame (HashMap.member defender . gamePlayers) `shouldReturn` False
      damageOf attacked `shouldReturn` 0

  it "finishes a lethal attack after Wade Wilson and his controlled cards leave play" do
    tg <- testGame ["deadpool", "captain-marvel"]
    inGame tg do
      let victim = player1 tg
          survivor = player2 tg
      clearHand victim
      _ <- putIntoPlay victim "44009"
      _ <- putIntoPlay victim "44013"
      setDamage victim 8
      mercenary <- addMinion victim Minions.hydraMercenary
      pass
      runNow [MinionMessage mercenary (MinionAttacks victim)]
      pass
      getsGame (HashMap.member victim . gamePlayers) `shouldReturn` False
      getsGame (HashMap.member survivor . gamePlayers) `shouldReturn` True

  it "offers Deadpool one defeat replacement after simultaneous lethal attacks hit him twice" do
    tg <- testGame ["captain-marvel", "deadpool"]
    inGame tg do
      let attacked = player1 tg
          defender = player2 tg
      clearHand attacked
      clearHand defender
      runNow [IdentityMessage attacked (ChangedToForm A), IdentityMessage defender (ChangedToForm A)]
      pass
      setDamage defender 8
      whirlwind <- addMinion attacked Minions.whirlwind
      runNow [MinionMessage whirlwind (MinionAttacks attacked)]
      useWhirlwind whirlwind
      choose "ask Deadpool to defend" (isTargetChoice defender)
      choose "Deadpool defends" (isTargetChoice defender)
      pass
      getsGame (HashMap.member defender . gamePlayers) `shouldReturn` True
      damageOf defender `shouldReturn` 11
      toCardCode <$> getPlayer defender `shouldReturn` "44001a"
      choose "replace the single simultaneous defeat" \case
        UseAbility ability -> abilitySource ability == IdentityRef defender && abilityIndex ability == 1
        _ -> False
      pass
      getsGame (HashMap.member defender . gamePlayers) `shouldReturn` True
      damageOf defender `shouldReturn` 8
      toCardCode <$> getPlayer defender `shouldReturn` "44001b"
      getMainSchemeTokenCount `shouldReturn` 1

  it "strips tough for piercing and keeps damage visible to Backflip's prevention" do
    tg <- testGame ["spider-man", "captain-marvel"]
    inGame tg do
      let first = player1 tg
          second = player2 tg
      clearHand first
      clearHand second
      runNow [IdentityMessage first (ChangedToForm A), IdentityMessage second (ChangedToForm A)]
      pass
      backflip <- putInHand first "01003"
      whirlwind <- addMinion first Minions.whirlwind
      groupModifiers whirlwind [KeywordModifier Piercing]
      resolve [GiveStatus (IdentityRef first) ToughStatus, GiveStatus (IdentityRef second) ToughStatus]
      pass
      runNow [MinionMessage whirlwind (MinionAttacks first)]
      useWhirlwind whirlwind
      choose "leave the first attack undefended" isNoDefender
      choose "Captain Marvel declines" (isLabel "$label.declineToDefend")
      choose "leave the second attack undefended" isNoDefender
      choose "Spider-Man declines" (isLabel "$label.declineToDefend")
      playWith backflip []
      pass
      damageOf first `shouldReturn` 0
      damageOf second `shouldReturn` 2

  it "cannot use Backflip to defend a second simultaneous attack after defending another player" do
    tg <- testGame ["captain-marvel", "spider-man"]
    inGame tg do
      let attacked = player1 tg
          defender = player2 tg
      clearHand attacked
      clearHand defender
      runNow [IdentityMessage attacked (ChangedToForm A), IdentityMessage defender (ChangedToForm A)]
      pass
      probe <- putIntoPlay defender "01008"
      attrs <- getsGame $ toAttrs . fromMaybe (error "missing damage probe") . HashMap.lookup (upgradeIdOf probe) . gameUpgrades
      withGame_ $ entitiesL . upgradesL . at (upgradeIdOf probe) ?~ Upgrade.Upgrade (DamageWindowProbe attrs)
      backflip <- putInHand defender "01003"
      whirlwind <- addMinion attacked Minions.whirlwind
      runNow [MinionMessage whirlwind (MinionAttacks attacked)]
      useWhirlwind whirlwind
      choose "ask Spider-Man to defend" (isTargetChoice defender)
      choose "Spider-Man defends" (isTargetChoice defender)
      choose "leave Spider-Man's own attack undefended" isNoDefender
      choose "Captain Marvel declines" (isLabel "$label.declineToDefend")
      offers (isAbilityOfCard probe) `shouldReturn` True
      offers (isPlay backflip) `shouldReturn` False
      pass
      damageOf attacked `shouldReturn` 0
      damageOf defender `shouldReturn` 2

  it "discards The Merc with the Mouth when its owner is eliminated, before surviving players are queried" do
    tg <- testGame ["deadpool", "captain-marvel"]
    inGame tg do
      let victim = player1 tg
          survivor = player2 tg
      clearHand victim
      merc <- revealNow victim Obligations.theMercWithTheMouth
      pass
      setDamage victim 8
      minion <- addMinion victim Minions.hydraMercenary
      runNow [MinionMessage minion (MinionAttacks victim)]
      pass
      getModifiers (IdentityRef survivor) >>= (`shouldSatisfy` notElem CannotResolvePlayerCardAbilities)
      getsGame (HashMap.member victim . gamePlayers) `shouldReturn` False
      getsGame (HashMap.null . gameObligations) `shouldReturn` True
      encounterDiscardNow >>= (`shouldSatisfy` elem merc)
