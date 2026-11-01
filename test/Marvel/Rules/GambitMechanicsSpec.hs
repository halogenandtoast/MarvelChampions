module Marvel.Rules.GambitMechanicsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability (windowAbility)
import Marvel.Ability.Types (AbilityType (ForcedInterrupt, ForcedResponse), HasAbilities (..))
import Marvel.Cost.Types (Cost (NoCost))
import Marvel.Matchers.Types
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as Minions
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

  it "checks each Frenzy activation's form after responses to the previous attack" do
    tg <- testGame ["gambit", "spider-man"]
    let p = player1 tg
    inGame tg do
      _ <- addMinion p Minions.exodus
      _ <- addMinion p Minions.mgAmeliaVoght
      changeForm p
      pass
      clearHand p
      fixture <- putIntoPlay p "01008"
      attrs <- getsGame (toAttrs . fromMaybe (error "missing form-change fixture") . HashMap.lookup (upgradeIdOf fixture) . gameUpgrades)
      withGame_ $ entitiesL . upgradesL . at (upgradeIdOf fixture) ?~ Upgrade (ChangeFormAfterAttack attrs)
      _ <- setEncounterDeck [Treacheries.advance, Treacheries.advance, Treacheries.advance] []
      before <- mainSchemeThreat
      _ <- revealNow p Treacheries.acolyteFrenzy
      settle
      damageOf p `shouldReturn` 1
      after <- mainSchemeThreat
      after `shouldSatisfy` (> before)

  it "can pay Rogue's continuously reduced cost with one initial charge and only Molecular Acceleration" do
    tg <- testGame ["gambit"]
    let p = player1 tg
    inGame tg do
      changeForm p
      pass
      resolve [PlaceCounters (IdentityRef p) 1]
      clearHand p
      rogue <- putInHand p "37002"
      resource <- putInHand p "37010"
      offers (isPlay rogue) `shouldReturn` True
      choose "play Rogue" (isPlay rogue)
      choose "spend Molecular Acceleration" (isPayWith resource)
      playerIdentityCounters <$> playerAttrs p `shouldReturn` 1
      choose "place the charge before spending" (isLabel "$label.molecularAcceleration")
      settle
      playerIdentityCounters <$> playerAttrs p `shouldReturn` 2
      alliesWith "37002" >>= (\cards -> length cards `shouldBe` 1)
      discardOf p >>= (`shouldSatisfy` elem resource)

  it "does not speculate a Molecular Acceleration discount when it still cannot cover Rogue" do
    tg <- testGame ["gambit"]
    let p = player1 tg
    inGame tg do
      changeForm p
      pass
      clearHand p
      rogue <- putInHand p "37002"
      _ <- putInHand p "37010"
      offers (isPlay rogue) `shouldReturn` False

  it "does not give Molecular Acceleration's prospective discount in alter-ego" do
    tg <- testGame ["gambit"]
    let p = player1 tg
    inGame tg do
      resolve [PlaceCounters (IdentityRef p) 1]
      clearHand p
      rogue <- putInHand p "37002"
      _ <- putInHand p "37010"
      offers (isPlay rogue) `shouldReturn` False

  it "cannot finish paying Rogue cheaply after declining the spending interrupt" do
    tg <- testGame ["gambit"]
    let p = player1 tg
    inGame tg do
      changeForm p
      pass
      resolve [PlaceCounters (IdentityRef p) 1]
      clearHand p
      rogue <- putInHand p "37002"
      resource <- putInHand p "37010"
      choose "play Rogue" (isPlay rogue)
      choose "spend Molecular Acceleration" (isPayWith resource)
      choose "decline the counter" (isLabel "$label.noResponse")
      settle
      alliesWith "37002" `shouldReturn` []
      playerIdentityCounters <$> playerAttrs p `shouldReturn` 1
      handOf p >>= (`shouldSatisfy` elem rogue)
      handOf p >>= (`shouldSatisfy` elem resource)
