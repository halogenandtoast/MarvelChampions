module Marvel.Rules.GambitMechanicsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability (windowAbility)
import Marvel.Question (runAbility)
import Marvel.Ability.Types (AbilityType (ForcedInterrupt, ForcedResponse), HasAbilities (..))
import Marvel.Cost.Types (Cost (NoCost))
import Marvel.Matchers.Types
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Types (minionConfused)
import Marvel.Damage (toDamage, Damage (damageOverkill), DamageSource (FromAbility, FromPlayerAttack))
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Modifier (HasModifiersFor)
import Marvel.Upgrade.Types (Upgrade (..), IsUpgrade (..))
import Marvel.Window qualified as W
import TestImport

-- A replacement probe lets the general departure contract be tested independently
-- of the Gambit pack's attachment implementation.
data MinionDepartureProbe = MinionDepartureProbe (Attrs Upgrade) MinionId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, HasModifiersFor)

instance IsUpgrade MinionDepartureProbe where
  toUpgradeAttrs (MinionDepartureProbe attrs _) = attrs

instance HasAbilities MinionDepartureProbe where
  getAbilities (MinionDepartureProbe attrs mid) =
    [windowAbility attrs 1 (W.MinionWouldLeavePlay $ MinionWithId mid)
      ForcedInterrupt NoCost (runAbility attrs 1)]

instance RunMessage MinionDepartureProbe where
  runMessage msg probe@(MinionDepartureProbe attrs mid) = L.runQueueT $ case msg of
    RanAbility _ target 1 _ _ | target == toRef attrs -> do
      L.cancelMinionLeavingPlay mid
      L.healAllDamage mid
      L.removeFromPlay attrs
      pure probe
    _ -> (\updated -> MinionDepartureProbe updated mid) <$> runMessage msg attrs

probe :: IdentityId -> MinionId -> CardCode -> GameAppT PlayerCard
probe p mid code = do
  card <- putIntoPlay p code
  attrs <- getsGame (toAttrs . fromMaybe (error "missing departure probe") . HashMap.lookup (upgradeIdOf card) . gameUpgrades)
  withGame_ $ entitiesL . upgradesL . at (upgradeIdOf card) ?~ Upgrade (MinionDepartureProbe attrs mid)
  pure card

-- A response fixture models a form change resolving between two activations.
newtype ChangeFormAfterAttack = ChangeFormAfterAttack (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance HasAbilities ChangeFormAfterAttack where
  getAbilities (ChangeFormAfterAttack attrs) =
    [windowAbility attrs 1 (W.EnemyAttacked W.After AnyEnemy $ IdentityWithId $ upgradeController attrs)
      ForcedResponse NoCost (runAbility attrs 1)]

instance RunMessage ChangeFormAfterAttack where
  runMessage msg probe@(ChangeFormAfterAttack attrs) = L.runQueueT $ case msg of
    RanAbility _ target 1 _ _ | target == toRef attrs -> do
      L.changeToAlterEgo (upgradeController attrs)
      L.removeFromPlay attrs
      pure probe
    _ -> ChangeFormAfterAttack <$> runMessage msg attrs

settle :: GameAppT ()
settle = go (100 :: Int)
 where
  go 0 = error "Gambit mechanics fixture did not settle"
  go n = openQuestion >>= \case
    Just (_, QuestionLabel "$label.yourTurn" _ _) -> pure ()
    Just (you, q) -> do
      let choices = choicesOf q
          passing c = isNoResponse c || isNoDefender c || isLabel "$label.declineToDefend" c
      answerQuestion you (fromMaybe 0 $ headMay [i | (i,c) <- zip [0..] choices, passing c])
      go (n-1)
    Nothing -> pure ()

spec :: Spec
spec = describe "Gambit general mechanics" do
  for_ [("discard", RemoveFromPlay), ("return to hand", ReturnToHand), ("remove from game", RemoveFromGame), ("victory display", AddToVictoryDisplay)] $ \(cause, leave) ->
    it ("replaces a minion's " <> cause <> " while preserving its in-play state") do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        mid <- addMinion p Minions.shocker
        resolve [MinionMessage mid (MinionDamaged GameRef $ toDamage 1 FromAbility), GiveStatus (MinionRef mid) ConfusedStatus]
        replacement <- probe p mid "01008"
        resolve [leave $ MinionRef mid]
        minionDamageOf mid `shouldReturn` Just 0
        confused <- getsGame (fmap (minionConfused . toAttrs) . HashMap.lookup mid . gameMinions)
        confused `shouldBe` Just True
        getsGame (HashMap.member (upgradeIdOf replacement) . gameUpgrades) `shouldReturn` False
        discard <- encounterDiscardNow
        discard `shouldSatisfy` all ((/= unMinionId mid) . unCardId . ecCardId)

  it "closes a replaced departure before another replacement can trigger" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.shocker
      first <- probe p mid "01008"
      second <- probe p mid "01093"
      resolve [RemoveFromPlay $ MinionRef mid]
      minionDamageOf mid `shouldReturn` Just 0
      survivors <- getsGame $ \g -> filter (`HashMap.member` gameUpgrades g) [upgradeIdOf first, upgradeIdOf second]
      length survivors `shouldBe` 1

  it "does not create departure windows for a minion already out of play" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.shocker
      resolve [RemoveFromPlay $ MinionRef mid]
      replacement <- probe p mid "01008"
      resolve [RemoveFromPlay $ MinionRef mid]
      getsGame (HashMap.member (upgradeIdOf replacement) . gameUpgrades) `shouldReturn` True

  it "replaces lethal defeat before overkill damage or after-defeat responses" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.shocker
      _ <- probe p mid "01008"
      before <- villainDamageNow
      let damage = (toDamage 6 $ FromPlayerAttack p) {damageOverkill = True}
      resolve [MinionMessage mid $ MinionDamaged (IdentityRef p) damage]
      minionDamageOf mid `shouldReturn` Just 0
      villainDamageNow `shouldReturn` before
      windows <- getCurrentWindows
      windows `shouldSatisfy` all (\case
        W.Window W.After (W.DefeatedMinion mid' _ _ _) -> mid /= mid'
        _ -> True)

  it "resolves when-defeated replacements before the would-leave replacement" do
    tg <- testGame ["spider-man"]
    let p = player1 tg
    inGame tg do
      mid <- addMinion p Minions.shocker
      replacement <- probe p mid "01008"
      _ <- revealNow p Attachments.biomechanicalUpgrades
      settle
      resolve [MinionMessage mid $ MinionDamaged (IdentityRef p) (toDamage 6 $ FromPlayerAttack p)]
      minionDamageOf mid `shouldReturn` Just 0
      getsGame (HashMap.member (upgradeIdOf replacement) . gameUpgrades) `shouldReturn` True
      encounterDiscardCodes >>= (`shouldSatisfy` elem "01185")

  for_ [("Spot's shuffle", Minions.spot, 4), ("The Sleeper's removal", Minions.theSleeper, 5)] $ \(name, def, health) ->
    it ("can replace " <> name <> " without losing its state or attachments") do
      tg <- testGame ["spider-man"]
      let p = player1 tg
      inGame tg do
        mid <- addMinion p def
        _ <- probe p mid "01008"
        deck <- encounterDeckSize
        resolve [MinionMessage mid $ MinionDamaged (IdentityRef p) (toDamage health $ FromPlayerAttack p)]
        minionDamageOf mid `shouldReturn` Just 0
        encounterDeckSize `shouldReturn` deck
