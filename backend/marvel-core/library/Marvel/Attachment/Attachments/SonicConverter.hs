module Marvel.Attachment.Attachments.SonicConverter (
  sonicConverter,
  SonicConverter (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
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
import Marvel.Ref
import Marvel.Resource
import Marvel.Window

sonicConverter :: AttachmentCard SonicConverter
sonicConverter = attachment SonicConverter Cards.sonicConverter

newtype SonicConverter = SonicConverter (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor SonicConverter where
  getModifiersFor _ (VillainRef vid) (SonicConverter a)
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
          $ runAbility a 1
      , ability
          a
          2
          HeroAction
          NoCriteria
          (MultiResourceCost [Just Energy, Just Mental, Just Physical])
          $ DiscardTarget (toRef a)
      ]
    _ -> []

instance RunMessage SonicConverter where
  runMessage msg a@(SonicConverter attrs) = case msg of
    AttachmentMessage ident msg' | ident == attachmentId attrs -> case msg' of
      RevealAttachment _ -> do
        villainId <- selectJust ActiveVillain
        push $ VillainMessage villainId $ AttachedToVillain ident
        pure . SonicConverter $ attrs & enemyL ?~ EnemyVillainId villainId
      _ -> SonicConverter <$> runMessage msg attrs
    RanAbility _ (isTarget a -> True) 1 [EnemyAttacksAndDamages _ cid] _ -> do
      case cid of
        IdentityCharacter ident -> push $ IdentityMessage ident IdentityStunned
        AllyCharacter ident -> push $ AllyMessage ident AllyStunned
        _ -> error "impossible"
      pure a
    _ -> SonicConverter <$> runMessage msg attrs
