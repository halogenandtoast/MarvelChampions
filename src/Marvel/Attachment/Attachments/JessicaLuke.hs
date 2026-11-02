{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Attachment.Attachments.JessicaLuke where
import Marvel.Prelude
import Control.Monad (replicateM_)
import Data.Maybe (maybeToList)
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Status
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.JessicaJones.Cards qualified as Cards
import Marvel.JessicaJones.Helpers
import Marvel.Attack (Attack (..))
import Marvel.Phase
import Marvel.Attachment.Types

newtype JLAttachment = JLAttachment (Attrs Attachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsAttachment

jl62031 :: AttachmentCard JLAttachment
jl62031 = attachment JLAttachment Cards.jl62031

instance HasModifiersFor JLAttachment where
  getModifiersFor _ target (JLAttachment a) = pure [AttackModifier 1 | Just target == (enemyRef <$> attachmentEnemy a)]

instance HasAbilities JLAttachment where
  getAbilities (JLAttachment a) =
    [limitedWindowAbility a 1 (W.EnemyAttackedAnyTarget W.When $ EnemyWithId enemy) ForcedInterrupt NoCriteria NoCost (runAbility a 1) | enemy <- maybeToList (attachmentEnemy a)] <>
    [ability a 2 HeroAction NoCriteria (MultiResourceCost [Just R.Mental, Just R.Mental]) (runAbility a 2)]

instance RunMessage JLAttachment where
  runMessage msg e@(JLAttachment a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      cottonmouth <- selectOne (MinionIs Cards.jl62029)
      case cottonmouth of
        Just mid -> attachTo aid mid
        Nothing -> withTheVillainIfAny you (traverse_ $ attachTo aid)
      pure e
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      for_ (attachmentEnemy a) $ \enemy -> do
        attack <- getEnemyAttack enemy
        traverse_ (removeAllTough . attackCharacter) attack
      pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> e <$ removeFromPlay a
    _ -> JLAttachment <$> runMessage msg a
