module Marvel.Attachment.Attachments.NextEvolutionSuperStrengthSet where

import Marvel.Attack (attackCharacter)
import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword qualified as Keyword
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Resource
import Marvel.Status
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.NextEvolution.Sinister

import Marvel.Card
import Marvel.Entity
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types hiding (attachToTheVillain)
import Marvel.Ability (HasAbilities)
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype SuperStrengthSetAttachment = SuperStrengthSetAttachment (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40155 :: AttachmentCard SuperStrengthSetAttachment
ne40155 = attachment SuperStrengthSetAttachment Cards.ne40155

ne40156 :: AttachmentCard SuperStrengthSetAttachment
ne40156 = attachment SuperStrengthSetAttachment Cards.ne40156

ne40157 :: AttachmentCard SuperStrengthSetAttachment
ne40157 = attachment SuperStrengthSetAttachment Cards.ne40157

instance RunMessage SuperStrengthSetAttachment where
  runMessage msg card@(SuperStrengthSetAttachment a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment _) | aid == attachmentId a -> card <$ attachToTheVillain aid
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      when (toCardCode a == "40156") $ for_ (attachmentEnemy a) (`giveStatus` ToughStatus)
      removeFromPlay a
      pure card
    _ -> SuperStrengthSetAttachment <$> runMessage msg a

instance HasAbilities SuperStrengthSetAttachment where
  getAbilities card@(SuperStrengthSetAttachment a) = case toCardCode a of
    "40156" -> [ability card 1 HeroAction NoCriteria (MultiResourceCost [Just Physical, Just Physical]) (runAbility card 1)]
    "40157" -> [limitedWindowAbility card 1 (W.EnemyAttackedAnyTarget W.After $ maybe (NotEnemy AnyEnemy) EnemyWithId $ attachmentEnemy a)
      ForcedResponse NoCriteria NoCost (runAbility card 1)]
    _ -> []

instance HasModifiersFor SuperStrengthSetAttachment where
  getPrintedModifiersFor _ target (SuperStrengthSetAttachment a) = pure $
    if Just target /= (enemyRef <$> attachmentEnemy a) then [] else case toCardCode a of
      "40151" -> [AttackModifier 1]
      "40155" -> [AttackModifier 1]
      "40157" -> [AttackModifier 3]
      "40159" -> [SchemeModifier 1]
      _ -> []
  getModifiersFor source target card@(SuperStrengthSetAttachment a) = do
    text <- if Just target /= (enemyRef <$> attachmentEnemy a) then pure [] else case toCardCode a of
      "40151" -> pure [TraitModifier Trait.Aerial, KeywordModifier Keyword.Overkill]
      "40152" -> do
        targetCharacter <- case attachmentEnemy a of
          Just (EnemyVillainId vid) -> fmap attackCharacter <$> getVillainAttack vid
          _ -> pure Nothing
        nonAerial <- maybe (pure False) (fmap not . characterMatches (CharacterWithTrait Trait.Aerial)) targetCharacter
        pure [modifier | nonAerial, modifier <- [AttackModifier 1, AttacksIgnoreRetaliate]]
      "40153" -> pure [OnlyAerialOrRangedAttacksCanDamage]
      "40155" -> pure [TraitModifier Trait.Brute, KeywordModifier Keyword.Steady]
      "40156" -> pure [AttackDamageReduction 1]
      "40157" -> pure [KeywordModifier Keyword.Ranged]
      "40159" -> pure [TraitModifier Trait.Psionic, KeywordModifier $ Keyword.Retaliate 1]
      _ -> pure []
    pure text
