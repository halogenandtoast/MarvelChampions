module Marvel.CivilWar.Initiative where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Types
import Marvel.Trait qualified as T

newtype Drafted = Drafted (Attrs Attachment `With` Maybe IdentityId)
  deriving anyclass HasAbilities
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

instance IsAttachment Drafted where
  toAttachmentAttrs (Drafted (a `With` _)) = a

drafted :: AttachmentCard Drafted
drafted = attachment (\a -> Drafted (a `With` Nothing)) C.cw56080

instance HasModifiersFor Drafted where
  getModifiersFor _ (MinionRef mid) (Drafted (a `With` _))
    | attachmentEnemy a == Just (EnemyMinionId mid) = pure [HitPointModifier 2,TraitModifier T.Avenger]
  getModifiersFor _ _ _ = pure []

instance RunMessage Drafted where
  runMessage msg e@(Drafted (a `With` resolver)) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      competition <- getCompetition
      case competition of
        Nothing -> do
          discardUntil FromEncounterDeck (CardWithType MinionType) a
          cardSpecific a "checkAttached" ()
        Just _ -> do
          opponents <- enemyTeamPlayers you
          candidates <- fmap concat $ for opponents $ \opponent -> asYou opponent $
            selectList (MinionEngagedWith $ IdentityWithId opponent)
          minions <- filterM canAttach candidates
          chooseForEnemyTeam you a
            [targeting (MinionRef mid) $ do
              attachTo aid mid
              engageMinion mid you
            | mid <- minions]
          cardSpecific a "checkAttached" ()
      pure $ Drafted (a `With` Just you)
    WithDiscardedMatch (isTarget a -> True) FromEncounterDeck (EncounterCard card) -> do
      for_ resolver $ \you -> do
        revealFoundEncounterCard you card
        cardSpecific a "attach" (MinionId $ unCardId $ ecCardId card)
      pure e
    CardSpecific (isTarget a -> True) "attach" (fromJSON -> Success mid) -> do
      present <- selectAny (MinionWithId mid)
      when present $ whenM (canAttach mid) $ attachTo (attachmentId a) mid
      pure e
    CardSpecific (isTarget a -> True) "checkAttached" _ -> do
      unless (isJust $ attachmentEnemy a) $ removeFromPlay a
      pure e
    _ -> (\a' -> Drafted (a' `With` resolver)) <$> runMessage msg a
   where
    canAttach mid = do
      modifiers <- getModifiers (MinionRef mid)
      pure $ CannotHaveAttachedCards `notElem` modifiers && CannotHaveAttachedEncounterCards `notElem` modifiers

newtype FiftyStateInitiative = FiftyStateInitiative (Attrs SideScheme)
  deriving anyclass (IsSideScheme,HasModifiersFor)
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

fiftyStateInitiative :: SideSchemeCard FiftyStateInitiative
fiftyStateInitiative = sideScheme FiftyStateInitiative C.cw56082 (Static 3)

instance RunMessage FiftyStateInitiative where
  runMessage msg (FiftyStateInitiative a) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId a -> do
      cards <- filter (cardMatch $ CardWithType MinionType <> CardWithTrait T.Avenger) <$> getEncounterDiscard
      unless (null cards) $ shuffleIntoEncounterDeck cards
      FiftyStateInitiative <$> runMessage msg a
    _ -> FiftyStateInitiative <$> runMessage msg a
