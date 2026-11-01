module Marvel.Attachment.Attachments.RadioactiveBuildup (
  radioactiveBuildup,
  RadioactiveBuildup (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria ()
import Marvel.Entity ()
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Window
import Marvel.Window qualified as W ()
import Marvel.Attack

radioactiveBuildup :: AttachmentCard RadioactiveBuildup
radioactiveBuildup = attachment RadioactiveBuildup Cards.radioactiveBuildup

newtype RadioactiveBuildup = RadioactiveBuildup (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Attach to Thunderball. +3 ATK.
instance HasModifiersFor RadioactiveBuildup where
  getModifiersFor _ (VillainRef vid) (RadioactiveBuildup a) | attachmentEnemy a == Just (EnemyVillainId vid) = pure [AttackModifier 3]
  getModifiersFor _ _ _ = pure []

-- | [star] Forced Response: after Thunderball attacks, discard this card.
instance HasAbilities RadioactiveBuildup where
  getAbilities (RadioactiveBuildup a) = case attachmentEnemy a of
    Just enemy@(EnemyVillainId _) ->
      [windowAbility a 1 (EnemyAttacked After (EnemyWithId enemy) AnyIdentity) ForcedResponse NoCost (DiscardTarget $ toRef a)]
    _ -> []

-- | Excess damage dealt by Thunderball is placed as threat on his side scheme: what his
-- attack deals past the remaining hit points of the character it is against.
instance RunMessage RadioactiveBuildup where
  runMessage msg a@(RadioactiveBuildup attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      villainNamed "Thunderball" >>= \case
        Just v -> attachTo ident v
        Nothing -> removeFromPlay attrs
      pure a
    IdentityMessage _ (IdentityWasAttacked attack') -> excess attack'
    AllyMessage _ (AllyWasAttacked attack') -> excess attack'
    _ -> RadioactiveBuildup <$> runMessage msg attrs
   where
    excess attack' = case attachmentEnemy attrs of
      Just enemy@(EnemyVillainId v) | attackEnemy attack' == enemy -> do
        n <- getExcessDamage (attackCharacter attack') (attackDamageDealt attack')
        when (n > 0) $ traverse_ (`placeThreat` n) =<< signatureSchemeOf v
        pure a
      _ -> pure a
