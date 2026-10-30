module Marvel.Attachment.Attachments.BallAndChain (
  ballAndChain,
  BallAndChain (..),
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

ballAndChain :: AttachmentCard BallAndChain
ballAndChain = attachment BallAndChain Cards.ballAndChain

newtype BallAndChain = BallAndChain (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Attach to Thunderball. +1 ATK.
instance HasModifiersFor BallAndChain where
  getModifiersFor _ (VillainRef vid) (BallAndChain a) | attachmentEnemy a == Just (EnemyVillainId vid) = pure [AttackModifier 1]
  getModifiersFor _ _ _ = pure []

-- | [star] Forced Response: after Thunderball attacks, place 1 threat on the main
-- scheme. Hero Action: exhaust your hero and discard 1 card at random from your hand ->
-- discard this card.
instance HasAbilities BallAndChain where
  getAbilities (BallAndChain a) = case attachmentEnemy a of
    Just enemy@(EnemyVillainId _) ->
      [ windowAbility a 1 (EnemyAttacked After (EnemyWithId enemy) AnyIdentity) ForcedResponse NoCost (runAbility a 1)
      , ability a 2 HeroAction NoCriteria (ExhaustCost <> DiscardRandomHandCardsCost 1) (DiscardTarget $ toRef a)
      ]
    _ -> []

instance RunMessage BallAndChain where
  runMessage msg a@(BallAndChain attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      villainNamed "Thunderball" >>= \case
        Just v -> attachTo ident v
        Nothing -> removeFromPlay attrs
      pure a
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      traverse_ (`placeThreat` 1) =<< selectOne EncounterMainScheme
      pure a
    _ -> BallAndChain <$> runMessage msg attrs
