module Marvel.Attachment.Attachments.LifeDrain where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria ()
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Identity.Types ()
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Status ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers ()
import Marvel.Attachment.Types

lifeDrain :: AttachmentCard LifeDrain
lifeDrain = attachment LifeDrain Cards.lifeDrain

newtype LifeDrain = LifeDrain (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities LifeDrain where
  getAbilities a@(LifeDrain attrs) = case attachmentEnemy attrs of
    Just enemy -> [windowAbility a 1 (W.EnemyAttacked W.When (EnemyWithId enemy) AnyIdentity) ForcedInterrupt NoCost (runAbility a 1)]
    Nothing -> []
instance RunMessage LifeDrain where
  runMessage msg a@(LifeDrain attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      minions <- selectList AnyMinion
      ranked <- for minions $ \mid -> (mid,) <$> getEnemyPrintedHitPoints (EnemyMinionId mid)
      let highest = maximum (0 : map snd ranked)
      if null ranked then surge you >> removeFromPlay attrs else
        chooseOrRunOne you [targeting mid $ do
          attachTo aid mid
          cardSpecific attrs "activate" (you, mid) | (mid,hp) <- ranked, hp == highest]
      pure a
    CardSpecific (isTarget attrs -> True) "activate" (fromJSON -> Success (you, mid)) -> do
      hero <- identityMatches HeroIdentity you
      stopped <- if hero then characterHasCondition StunnedStatus (MinionCharacter mid) else characterHasCondition ConfusedStatus (MinionCharacter mid)
      blocked <- elem CannotActivate <$> getModifiers (MinionRef mid)
      if hero then enemyAttacks mid you else minionStep mid (MinionSchemesAgainst you)
      when (stopped || blocked) $ surge you
      pure a
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ (headMay $ concatMap (\case W.EnemyAttack _ you -> [you]; W.EnemyAttackAlly _ _ you -> [you]; _ -> []) windows) $ \you -> damageIdentity you attrs (toDamage 2 FromAbility)
      for_ (attachmentEnemy attrs) enemyBecomesTough
      pure a
    _ -> LifeDrain <$> runMessage msg attrs
