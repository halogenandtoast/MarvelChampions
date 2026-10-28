-- | Thanos's attachments (The Mad Titan's Shadow, Thanos): each attaches to Thanos as it
-- is revealed.
module Marvel.Attachment.Attachments.Thanos (
  thanossArmor,
  ThanossArmor (..),
  thanossHelmet,
  ThanossHelmet (..),
  masterOfTheStones,
  MasterOfTheStones (..),
  discardAfterBasicAttack,
  onTheVillain,
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Window qualified as W

-- | What the attachment gives the villain it is attached to.
onTheVillain :: (Applicative m) => Attrs Attachment -> Target -> [Modifier] -> m [Modifier]
onTheVillain a (VillainRef vid) ms | attachmentEnemy a == Just (EnemyVillainId vid) = pure ms
onTheVillain _ _ _ = pure []

-- | "Hero Response: after a hero makes a basic attack against Thanos, spend these
-- resources → discard this card": offered to the player whose hero attacked.
discardAfterBasicAttack :: Attrs Attachment -> [Resource] -> [Ability]
discardAfterBasicAttack a resources = case attachmentEnemy a of
  Just enemy ->
    [ windowAbility a 1 (W.MakesBasicAttackAgainst W.After You (EnemyWithId enemy)) HeroResponse (MultiResourceCost $ map Just resources) $
        DiscardTarget (toRef a)
    ]
  Nothing -> []

thanossArmor :: AttachmentCard ThanossArmor
thanossArmor = attachment ThanossArmor Cards.thanossArmor

-- | Forced Interrupt: when Thanos would take any amount of damage, reduce that amount by
-- 1 ('DamageReduction', from any source). Hero Response: [energy][physical] → discard.
newtype ThanossArmor = ThanossArmor (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor ThanossArmor where
  getModifiersFor _ target (ThanossArmor a) = onTheVillain a target [DamageReduction 1]

instance HasAbilities ThanossArmor where
  getAbilities (ThanossArmor a) = discardAfterBasicAttack a [Energy, Physical]

instance RunMessage ThanossArmor where
  runMessage msg a@(ThanossArmor attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> a <$ attachToTheVillain attrs
    _ -> ThanossArmor <$> runMessage msg attrs

thanossHelmet :: AttachmentCard ThanossHelmet
thanossHelmet = attachment ThanossHelmet Cards.thanossHelmet

-- | Thanos gains retaliate 1. Hero Response: [mental][physical] → discard.
newtype ThanossHelmet = ThanossHelmet (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor ThanossHelmet where
  getModifiersFor _ target (ThanossHelmet a) = onTheVillain a target [KeywordModifier (Retaliate 1)]

instance HasAbilities ThanossHelmet where
  getAbilities (ThanossHelmet a) = discardAfterBasicAttack a [Mental, Physical]

instance RunMessage ThanossHelmet where
  runMessage msg a@(ThanossHelmet attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> a <$ attachToTheVillain attrs
    _ -> ThanossHelmet <$> runMessage msg attrs

masterOfTheStones :: AttachmentCard MasterOfTheStones
masterOfTheStones = attachment MasterOfTheStones Cards.masterOfTheStones

-- | +1 ATK, +1 SCH. Forced Interrupt: when Thanos activates, put the top card of the
-- Infinity Stone deck into play (the Infinity Gauntlet resolves it as the activation
-- ends). At the end of this activation, discard Master of the Stones.
newtype MasterOfTheStones = MasterOfTheStones (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor MasterOfTheStones where
  getModifiersFor _ target (MasterOfTheStones a) = onTheVillain a target [AttackModifier 1, SchemeModifier 1]

instance RunMessage MasterOfTheStones where
  runMessage msg a@(MasterOfTheStones attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> a <$ attachToTheVillain attrs
    CheckWindows ws
      | Just enemy <- attachmentEnemy attrs
      , you : _ <- [p | W.Window W.When (W.EnemyActivation e p) <- ws, e == enemy] -> do
          putTopInfinityStoneIntoPlay you
          pure a
      | Just enemy <- attachmentEnemy attrs
      , notNull [() | W.Window W.After (W.EnemyActivation e _) <- ws, e == enemy] -> do
          removeFromPlay attrs
          pure a
    _ -> MasterOfTheStones <$> runMessage msg attrs
