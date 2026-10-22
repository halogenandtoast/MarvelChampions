module Marvel.Attachment.Attachments.RammingSpeed (
  rammingSpeed,
  RammingSpeed (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Window
import Marvel.Window qualified as W

rammingSpeed :: AttachmentCard RammingSpeed
rammingSpeed = attachment RammingSpeed Cards.rammingSpeed

newtype RammingSpeed = RammingSpeed (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Attach to Bulldozer. +2 ATK. [star] Forced Interrupt: when Bulldozer attacks you, you
-- must defend against his attacks with an ally you control, if able.
instance HasModifiersFor RammingSpeed where
  getModifiersFor _ (VillainRef vid) (RammingSpeed a) | attachmentEnemy a == Just (EnemyVillainId vid) = pure [AttackModifier 2, DefendedByAllyIfAble]
  getModifiersFor _ _ _ = pure []

-- | [star] Forced Response: after Bulldozer attacks you, discard this card.
instance HasAbilities RammingSpeed where
  getAbilities (RammingSpeed a) = case attachmentEnemy a of
    Just enemy@(EnemyVillainId _) ->
      [windowAbility a 1 (EnemyAttacked After (EnemyWithId enemy) AnyIdentity) ForcedResponse NoCost (DiscardTarget $ toRef a)]
    _ -> []

instance RunMessage RammingSpeed where
  runMessage msg a@(RammingSpeed attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      villainNamed "Bulldozer" >>= \case
        Just v -> attachTo ident v
        Nothing -> removeFromPlay attrs
      pure a
    _ -> RammingSpeed <$> runMessage msg attrs
