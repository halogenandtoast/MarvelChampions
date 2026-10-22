module Marvel.Attachment.Attachments.MagicCrowbar (
  magicCrowbar,
  MagicCrowbar (..),
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

magicCrowbar :: AttachmentCard MagicCrowbar
magicCrowbar = attachment MagicCrowbar Cards.magicCrowbar

newtype MagicCrowbar = MagicCrowbar (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Attach to Wrecker. +1 ATK.
instance HasModifiersFor MagicCrowbar where
  getModifiersFor _ (VillainRef vid) (MagicCrowbar a) | attachmentEnemy a == Just (EnemyVillainId vid) = pure [AttackModifier 1]
  getModifiersFor _ _ _ = pure []

-- | [star] Forced Response: after Wrecker attacks, place 1 threat on the side scheme with
-- the least threat. Hero Action: exhaust your hero and discard 1 card at random from your
-- hand -> discard this card.
instance HasAbilities MagicCrowbar where
  getAbilities (MagicCrowbar a) = case attachmentEnemy a of
    Just enemy@(EnemyVillainId _) ->
      [ windowAbility a 1 (EnemyAttacked After (EnemyWithId enemy) AnyIdentity) ForcedResponse NoCost (runAbility a 1)
      , ability a 2 HeroAction NoCriteria (ExhaustCost <> DiscardRandomHandCardsCost 1) (DiscardTarget $ toRef a)
      ]
    _ -> []

instance RunMessage MagicCrowbar where
  runMessage msg a@(MagicCrowbar attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      villainNamed "Wrecker" >>= \case
        Just v -> attachTo ident v
        Nothing -> removeFromPlay attrs
      pure a
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      withSideSchemeByThreat LeastThreat AnySideScheme (`placeThreat` 1)
      pure a
    _ -> MagicCrowbar <$> runMessage msg attrs
