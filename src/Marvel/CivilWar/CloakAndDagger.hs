module Marvel.CivilWar.CloakAndDagger where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Cost
import Marvel.Damage
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Runner
import Marvel.SideScheme.Types
import Marvel.Treachery.Types
import Marvel.Window qualified as W

newtype CloakAndDaggerMinion = CloakAndDaggerMinion (Attrs Minion)
  deriving anyclass (IsMinion,HasModifiersFor,HasAbilities)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

cloakAndDaggerMinions :: [MinionCard CloakAndDaggerMinion]
cloakAndDaggerMinions =
  [minion CloakAndDaggerMinion C.cw56194 (Sch 2) (Atk 1) (HP 4)
  ,minion CloakAndDaggerMinion C.cw56195 (Sch 1) (Atk 2) (HP 3)]

instance RunMessage CloakAndDaggerMinion where
  runMessage msg e@(CloakAndDaggerMinion a) = runQueueT $ case msg of
    MinionMessage mid (RevealMinion you) | mid == minionId a -> do
      if toCardCode a == "56194" then do
        placeChosenThreat you a 2
        removeFriendlyThreat you a 2
      else do
        enemyLeaderFor you >>= traverse_ (\vid -> healFrom a vid 2)
        damageControlledCharacter you a 2
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target -> do
      you <- getBoostedPlayer enemy
      let cloak = toCardCode a == "56194"
      partner <- asYou you $ selectAny $ MinionIs $ if cloak then C.cw56195 else C.cw56194
      if partner then revealBoost a you
      else if cloak then placeChosenThreat you a 1 else damageControlledCharacter you a 1
      pure e
    _ -> CloakAndDaggerMinion <$> runMessage msg a

placeChosenThreat :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Natural -> m ()
placeChosenThreat you source n = do
  schemes <- asYou you $ selectList AnyScheme
  unless (null schemes) $ chooseOne you [targeting (schemeRef sid) (placeThreatOn source sid n) | sid <- schemes]

removeFriendlyThreat :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Natural -> m ()
removeFriendlyThreat you source n = do
  leader <- friendlyLeaderFor you
  for_ leader $ \vid -> do
    area <- getAreaOfTarget vid
    schemes <- getMainSchemesIn area
    traverse_ (\sid -> removeSchemeThreat source (SchemeMainSchemeId sid) n) schemes

damageControlledCharacter :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Natural -> m ()
damageControlledCharacter you source n = do
  characters <- asYou you $ selectList $ CharacterControlledBy (IdentityWithId you)
  unless (null characters) $ chooseOne you
    [targeting (characterRef character) (damageCharacter character source $ toDamage n FromAbility) | character <- characters]

newtype Darkforce = Darkforce (Attrs Attachment)
  deriving anyclass (IsAttachment,HasModifiersFor)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

darkforce :: AttachmentCard Darkforce
darkforce = attachment Darkforce C.cw56196

instance HasAbilities Darkforce where
  getAbilities (Darkforce a) =
    [windowAbility a 1 (W.VillainSchemedPlacingThreatOnMainScheme $ VillainWithId vid) ForcedResponse NoCost (runAbility a 1)
    | Just (EnemyVillainId vid) <- [attachmentEnemy a]]

instance RunMessage Darkforce where
  runMessage msg e@(Darkforce a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      enemyLeaderFor you >>= \case
        Nothing -> removeFromPlay a
        Just vid -> do
          modifiers <- getModifiers (VillainRef vid)
          if any (`elem` modifiers) [CannotHaveAttachedCards,CannotHaveAttachedEncounterCards]
            then removeFromPlay a
            else attachTo aid vid >> giveStatus vid ToughStatus
      pure e
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ [ (vid,n) | W.VillainSchemeResolvedOnMainScheme vid n <- windows, Just (EnemyVillainId vid) == attachmentEnemy a ] $ \(vid,n) -> do
        you <- firstPlayerAtLeader vid
        removeFriendlyThreat you a n
      removeFromPlay a
      pure e
    _ -> Darkforce <$> runMessage msg a

newtype Lightforce = Lightforce (Attrs Treachery)
  deriving anyclass IsTreachery
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

lightforce :: TreacheryCard Lightforce
lightforce = treachery Lightforce C.cw56197

instance RunMessage Lightforce where
  runMessage msg e@(Lightforce a) = runQueueT $ case msg of
    TreacheryMessage tid (RevealTreachery you) | tid == treacheryId a -> do
      enemyLeaderFor you >>= traverse_ (\vid -> healFrom a vid 3)
      dealIndirectDamage you a 3
      pure e
    _ -> Lightforce <$> runMessage msg a

newtype CloakAndDaggerScheme = CloakAndDaggerScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme,HasModifiersFor)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

cloakAndDaggerScheme :: SideSchemeCard CloakAndDaggerScheme
cloakAndDaggerScheme = sideScheme CloakAndDaggerScheme C.cw56198 (Static 3)

instance RunMessage CloakAndDaggerScheme where
  runMessage msg (CloakAndDaggerScheme a) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId a -> do
      blanked <- isBlanked a
      unless blanked $ do
        area <- getAreaOfTarget a
        getActiveVillainIn area >>= traverse_ (\vid -> healFrom a vid 3 >> giveStatus vid ToughStatus)
      CloakAndDaggerScheme <$> runMessage msg a
    _ -> CloakAndDaggerScheme <$> runMessage msg a
