-- | The seven-card print-and-play Kree Fanatic modular encounter set.
module Marvel.KreeFanatic where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Cards qualified as A
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Stats
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as M
import Marvel.Minion.Runner ()
import Marvel.Minion.Types
import Marvel.Modifier
import Marvel.Phase
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as S
import Marvel.SideScheme.Types
import Marvel.Treachery.Cards qualified as T
import Marvel.Treachery.Types
import Marvel.Keyword (Keyword (Overkill))
import Marvel.Window qualified as W

ronanKreeFanatic :: MinionCard RonanKreeFanatic
ronanKreeFanatic = minion RonanKreeFanatic M.ronanKreeFanatic (Sch 3) (Atk 3) (HP 9)

newtype RonanKreeFanatic = RonanKreeFanatic (Attrs Minion)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor RonanKreeFanatic where
  getModifiersFor _ target (RonanKreeFanatic a) | isTarget a target = do
    players <- fromIntegral <$> getStartingPlayerCount
    pure [CannotBeStunned, HitPointModifier (9 * (players - 1))]
  getModifiersFor _ _ _ = pure []

instance HasAbilities RonanKreeFanatic where
  getAbilities (RonanKreeFanatic a) =
    [windowAbility a 1 (W.PhaseBegins W.When VillainPhase) ForcedInterrupt NoCost $ runAbility a 1]

instance RunMessage RonanKreeFanatic where
  runMessage msg e@(RonanKreeFanatic a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      heroes <- selectList $ IdentityWithFewestRemainingHitPoints HeroIdentity
      first <- selectJust FirstPlayer
      unless (null heroes) $ chooseOrRunOne first [targeting (IdentityRef you) (engageMinion (minionId a) you) | you <- heroes]
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> e <$ (putBoostIntoPlay a =<< getBoostedPlayer enemy)
    _ -> RonanKreeFanatic <$> runMessage msg a

judgeJuryExecutioner :: SideSchemeCard JudgeJuryExecutioner
judgeJuryExecutioner = sideScheme JudgeJuryExecutioner S.judgeJuryExecutioner (Static 3)

newtype JudgeJuryExecutioner = JudgeJuryExecutioner (Attrs SideScheme)
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsSideScheme JudgeJuryExecutioner where
  sideSchemeAbilities (JudgeJuryExecutioner a) =
    [windowAbility a 1 (W.CharacterDefeatedByEnemyAttack W.After AnyEnemy) ForcedResponse NoCost $ runAbility a 1]

instance RunMessage JudgeJuryExecutioner where
  runMessage msg e@(JudgeJuryExecutioner a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 _ _ -> e <$ placeThreatOnEach a 2 MainScheme
    Boost (RevealedAsBoost target enemy) | isTarget a target -> e <$ (putBoostIntoPlay a =<< getBoostedPlayer enemy)
    _ -> JudgeJuryExecutioner <$> runMessage msg a

theAccused :: AttachmentCard TheAccused
theAccused = attachment TheAccused A.theAccused

newtype TheAccused = TheAccused (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities TheAccused where
  getAbilities (TheAccused a) =
    [windowAbility a 1 (W.EnemyAttacked W.When AnyEnemy $ IdentityWithId you) ForcedInterrupt NoCost $ runAbility a 1 | Just you <- [attachmentIdentity a]]
      <> [windowAbility a 2 (W.MinionDefeated W.After $ MinionIs M.ronanKreeFanatic) ForcedResponse NoCost $ runAbility a 2]

instance RunMessage TheAccused where
  runMessage msg e@(TheAccused a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> e <$ attachToIdentity aid you
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ [enemy | W.EnemyAttack enemy _ <- windows] (`changeAttack` 1)
      pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> e <$ removeFromPlay a
    Boost (RevealedAsBoost target enemy) | isTarget a target -> e <$ (putBoostIntoPlay a =<< getBoostedPlayer enemy)
    _ -> TheAccused <$> runMessage msg a

bringTheHammerDown, youDareOpposeMe :: TreacheryCard KreeFanaticTreachery
bringTheHammerDown = treachery KreeFanaticTreachery T.bringTheHammerDown
youDareOpposeMe = treachery KreeFanaticTreachery T.youDareOpposeMe

newtype KreeFanaticTreachery = KreeFanaticTreachery (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage KreeFanaticTreachery where
  runMessage msg e@(KreeFanaticTreachery a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> case toCardCode a of
      "90004" -> do
        ronans <- selectList $ MinionIs M.ronanKreeFanatic
        if null ronans then pure $ KreeFanaticTreachery $ a & surgeL .~ True
        else e <$ traverse_ (\mid -> getMinionEngagedPlayer mid >>= traverse_ (minionActivatesAgainst mid)) ronans
      _ -> do
        discardTopOfEncounterDeckThen 5 $ traverse_ (dealFoundEncounterCard you) . filter (\c -> toCardCode c `elem` ["90001", "90002", "90003", "90004", "90005"])
        pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      if toCardCode a == "90005" then attackGains enemy Overkill
      else createEffect T.bringTheHammerDown (enemyRef enemy) (EnemyEntity $ EnemyWithId enemy)
      pure e
    _ -> KreeFanaticTreachery <$> runMessage msg a

bringTheHammerDownEffect :: CardEffect HammerDownEffect
bringTheHammerDownEffect = effect HammerDownEffect T.bringTheHammerDown

newtype HammerDownEffect = HammerDownEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

instance RunMessage HammerDownEffect where
  runMessage msg e@(HammerDownEffect a) = runQueueT $ case msg of
    CheckWindows windows -> do
      when (any (\case W.Window W.After (W.EnemyAttackDefeatedCharacter enemy _) -> enemyRef enemy == refocus (effectSource a); _ -> False) windows) $ do
        selectJust FirstPlayer >>= dealEncounterCard
        disableEffect (effectId a)
      when (any (\case W.Window W.After (W.EnemyActivation enemy _) -> enemyRef enemy == refocus (effectSource a); _ -> False) windows) $ disableEffect (effectId a)
      pure e
    _ -> HammerDownEffect <$> runMessage msg a

instance IsMinion RonanKreeFanatic where
  minionBaseHitPoints _ = perPlayerMinionBaseHitPoints 9
