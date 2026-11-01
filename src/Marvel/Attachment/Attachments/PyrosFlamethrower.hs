module Marvel.Attachment.Attachments.PyrosFlamethrower where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.GameValue ()
import Marvel.Hand ()
import Marvel.Id ()
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
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W
import Marvel.Iceman.Cards qualified as Cards
import Marvel.Iceman.Helpers ()
import Marvel.Attachment.Types

pyrosFlamethrower :: AttachmentCard PyrosFlamethrower
pyrosFlamethrower = attachment PyrosFlamethrower Cards.pyrosFlamethrower

newtype PyrosFlamethrower = PyrosFlamethrower (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities PyrosFlamethrower where
  getAbilities a@(PyrosFlamethrower attrs) = case attachmentEnemy attrs of
    Just enemy -> [windowAbility a 1 (W.EnemyAttacked W.When (EnemyWithId enemy) AnyIdentity) ForcedInterrupt NoCost (runAbility a 1)]
    Nothing -> []
instance RunMessage PyrosFlamethrower where
  runMessage msg a@(PyrosFlamethrower attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      minions <- selectList (MinionIs Cards.pyroIceman)
      if null minions then surge you >> removeFromPlay attrs
        else chooseOrRunOne you [targeting mid $ attachTo aid mid | mid <- minions]
      pure a
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ (headMay $ concatMap (\case W.EnemyAttack _ you -> [you]; W.EnemyAttackAlly _ _ you -> [you]; _ -> []) windows) $ \you -> discardFromDeck you 1 attrs
      pure a
    WithDiscarded (isTarget attrs -> True) FromDeck cards -> do
      let n = fromIntegral $ sum [length $ printedResources $ getCardDef c | c <- onlyPlayerCards cards]
      for_ (attachmentEnemy attrs) $ \enemy -> enemyActivationBonus enemy n
      pure a
    _ -> PyrosFlamethrower <$> runMessage msg attrs
