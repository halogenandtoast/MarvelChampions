module Marvel.Attachment.Attachments.SonicConverter (
  sonicConverter,
  SonicConverter (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Attrs
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Resource
import Marvel.Source
import Marvel.Target
import Marvel.Window

sonicConverter :: AttachmentCard SonicConverter
sonicConverter = attachment SonicConverter Cards.sonicConverter

newtype SonicConverter = SonicConverter AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance HasModifiersFor SonicConverter where
  getModifiersFor _ (VillainTarget vid) (SonicConverter a)
    | Just (EnemyVillainId vid) == attachmentEnemy a = pure [AttackModifier 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities SonicConverter where
  getAbilities (SonicConverter a) = case attachmentEnemy a of
    Just enemyId ->
      [ windowAbility
            a
            1
            (EnemyAttackedAndDamaged (EnemyWithId enemyId) AnyCharacter)
            ForcedResponse
            NoCost
          $ RunAbility (toTarget a) 1
      , ability
          a
          2
          HeroAction
          NoCriteria
          (MultiResourceCost [Just Energy, Just Mental, Just Physical])
          $ DiscardTarget (toTarget a)
      ]
    _ -> []

instance RunMessage SonicConverter where
  runMessage msg a@(SonicConverter attrs) = case msg of
    AttachmentMessage aid msg' | aid == toId attrs -> case msg' of
      RevealAttachment _ -> do
        villainId <- selectJust ActiveVillain
        push $ VillainMessage villainId $ AttachedToVillain aid
        pure . SonicConverter $ attrs & enemyL ?~ EnemyVillainId villainId
      _ -> SonicConverter <$> runMessage msg attrs
    RanAbility target 1 [EnemyAttacksAndDamages _ cid]
      | isTarget attrs target -> do
        case cid of
          IdentityCharacter ident ->
            push $ IdentityMessage ident IdentityStunned
          AllyCharacter ident -> push $ AllyMessage ident AllyStunned
          _ -> error "impossible"
        pure a
    _ -> SonicConverter <$> runMessage msg attrs
