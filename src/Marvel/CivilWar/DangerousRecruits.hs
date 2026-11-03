module Marvel.CivilWar.DangerousRecruits where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Cost
import Marvel.GameValue
import Marvel.SideScheme.Types
import Marvel.Window qualified as W
import Marvel.Game.Source
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Runner
import Marvel.Treachery.Types

newtype Bullseye = Bullseye (Attrs Minion)
  deriving anyclass (IsMinion,HasModifiersFor,HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

bullseye :: MinionCard Bullseye
bullseye = minion Bullseye C.cw56088 (Sch 2) (Atk 2) (HP 3)

instance RunMessage Bullseye where
  runMessage msg e@(Bullseye a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      leader <- friendlyLeaderFor you
      candidates <- maybe (pure []) (\vid -> inAnyArea $ selectList $ AttachmentOnEnemy (EnemyVillainId vid)) leader
      attachments <- filterM (\aid -> do
        def <- getsGame (`abilityCardDef` AttachmentRef aid)
        modifiers <- getModifiers (AttachmentRef aid)
        pure $ maybe False (notElem Permanent . cdKeywords) def && all (`notElem` modifiers) [CannotBeDiscarded,CannotLeavePlay,CannotLeavePlayByCardAbilities]) candidates
      if null attachments
        then discardableControlledCardsMatching you (CardWithType UpgradeType) >>= chooseAndDiscardControlled you
        else chooseForEnemyTeam you a [targeting (AttachmentRef aid) (removeFromPlay aid) | aid <- attachments]
      pure e
    _ -> Bullseye <$> runMessage msg a


newtype Venom = Venom (Attrs Minion)
  deriving anyclass (IsMinion,HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

venom :: MinionCard Venom
venom = minion Venom C.cw56087 (Sch 1) (Atk 3) (HP 4)

instance HasModifiersFor Venom where
  getModifiersFor _ (MinionRef mid) (Venom a) | mid == minionId a = pure [KeywordModifier Piercing]
  getModifiersFor _ _ _ = pure []

instance RunMessage Venom where
  runMessage msg e@(Venom a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      friendlyLeaderFor you >>= \case
        Just vid -> do
          first <- asYou you (selectJust FirstPlayer)
          minionAttacksEnemyAs mid first vid
        Nothing -> minionActivatesAgainst mid you
      pure e
    _ -> Venom <$> runMessage msg a


newtype ExcessiveForce = ExcessiveForce (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

excessiveForce :: TreacheryCard ExcessiveForce
excessiveForce = treachery ExcessiveForce C.cw56090

instance RunMessage ExcessiveForce where
  runMessage msg e@(ExcessiveForce a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      minions <- asYou you (selectList AnyMinion)
      if null minions then gainSurge a else placeThreatOnEach a (fromIntegral $ length minions) EncounterMainScheme
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      attacker <- enemyLeaderFor you
      defender <- friendlyLeaderFor you
      for_ [(vid,leader) | Just vid <- [attacker],Just leader <- [defender]] $ \(vid,leader) -> do
        first <- firstPlayerAtLeader vid
        villainAttacksCharacterWithoutBoost vid first leader
      pure e
    _ -> ExcessiveForce <$> runMessage msg a


newtype JusticeLikeLightning = JusticeLikeLightning (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

justiceLikeLightning :: TreacheryCard JusticeLikeLightning
justiceLikeLightning = treachery JusticeLikeLightning C.cw56089

instance RunMessage JusticeLikeLightning where
  runMessage msg e@(JusticeLikeLightning a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      minions <- asYou you (selectList AnyMinion)
      available <- filterM (fmap (notElem CannotActivate) . getModifiers . MinionRef) minions
      pairs <- fmap catMaybes $ for available $ \mid -> fmap (mid,) <$> getMinionEngagedPlayer mid
      let activate (mid,engaged) = enemyActivationAfterCurrent mid (minionActivatesAgainst mid engaged)
      case pairs of
        [] -> enemyLeaderFor you >>= traverse_ (\vid -> enemyActivationAfterCurrent vid $ enemyActivatesWithoutBoost a vid you)
        [pair] -> activate pair
        _ -> do
          first <- asYou you (selectJust FirstPlayer)
          chooseOneAtATimePrompted first "$label.chooseActivationOrder"
            [targeting (MinionRef mid) (activate pair) | pair@(mid,_) <- pairs]
      pure e
    _ -> JusticeLikeLightning <$> runMessage msg a


newtype DangerousRecruits = DangerousRecruits (Attrs SideScheme)
  deriving anyclass HasModifiersFor
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

dangerousRecruits :: SideSchemeCard DangerousRecruits
dangerousRecruits = sideScheme DangerousRecruits C.cw56091 (Static 3)

instance IsSideScheme DangerousRecruits where
  toSideSchemeAttrs (DangerousRecruits a) = a
  sideSchemeAbilities (DangerousRecruits a) =
    [windowAbility a 1 (W.MinionDefeated W.When $ MinionEngagedWith AnyIdentity) ForcedInterrupt NoCost (runAbility a 1)]

instance RunMessage DangerousRecruits where
  runMessage msg e@(DangerousRecruits a) = runQueueT $ case msg of
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ [mid | W.DefeatedMinion mid _ _ _ <- windows] $ \mid -> do
        sameBoard <- (==) <$> getAreaOfTarget a <*> getAreaOfTarget mid
        when sameBoard $ do
          you <- getMinionEngagedPlayer mid
          for_ you $ \owner -> enemyLeaderFor owner >>= traverse_ (giveDefeatedMinionAsBoost mid)
      pure e
    _ -> DangerousRecruits <$> runMessage msg a
