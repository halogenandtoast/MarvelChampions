module Marvel.CivilWar.DangerousRecruits where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Game.Source
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Runner

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
