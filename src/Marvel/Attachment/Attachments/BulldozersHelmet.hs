module Marvel.Attachment.Attachments.BulldozersHelmet (
  bulldozersHelmet,
  BulldozersHelmet (..),
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

bulldozersHelmet :: AttachmentCard BulldozersHelmet
bulldozersHelmet = attachment (BulldozersHelmet . (`With` Meta 0)) Cards.bulldozersHelmet

newtype Meta = Meta {dealt :: Natural}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving newtype (Show, Eq)

newtype BulldozersHelmet = BulldozersHelmet (Attrs Attachment `With` Meta)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsAttachment BulldozersHelmet where
  toAttachmentAttrs (BulldozersHelmet (a `With` _)) = a

-- | Attach to Bulldozer. +1 ATK.
instance HasModifiersFor BulldozersHelmet where
  getModifiersFor _ (VillainRef vid) (BulldozersHelmet (a `With` _)) | attachmentEnemy a == Just (EnemyVillainId vid) = pure [AttackModifier 1]
  getModifiersFor _ _ _ = pure []

-- | [star] Forced Response: after Bulldozer attacks you, discard 1 card from the top of
-- your deck for each point of damage dealt by this attack. Hero Action: exhaust your hero
-- and discard 1 card at random from your hand -> discard this card.
instance HasAbilities BulldozersHelmet where
  getAbilities (BulldozersHelmet (a `With` _)) = case attachmentEnemy a of
    Just enemy@(EnemyVillainId _) ->
      [ windowAbility a 1 (EnemyAttacked After (EnemyWithId enemy) AnyIdentity) ForcedResponse NoCost (runAbility a 1)
      , ability a 2 HeroAction NoCriteria (ExhaustCost <> DiscardRandomHandCardsCost 1) (DiscardTarget $ toRef a)
      ]
    _ -> []

-- | The damage his attack deals is counted as it is dealt, each attack afresh.
instance RunMessage BulldozersHelmet where
  runMessage msg a@(BulldozersHelmet (attrs `With` meta)) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      villainNamed "Bulldozer" >>= \case
        Just v -> attachTo ident v
        Nothing -> removeFromPlay attrs
      pure a
    VillainMessage vid (VillainBeginAttack _) | ours vid -> pure $ BulldozersHelmet (attrs `With` Meta 0)
    VillainMessage vid (VillainBeginAttackWithoutBoost _) | ours vid -> pure $ BulldozersHelmet (attrs `With` Meta 0)
    (attackDamageWindow -> Just (enemy, _, n)) | Just enemy == attachmentEnemy attrs -> do
      BulldozersHelmet attrs' <- BulldozersHelmet . (`With` meta) <$> runMessage msg attrs
      pure $ BulldozersHelmet (withAttrs attrs' `With` Meta (dealt meta + n))
    RanAbility _ (isTarget attrs -> True) 1 [EnemyAttack _ you] _ -> do
      when (dealt meta > 0) $ discardTopOfDeck you (dealt meta)
      pure $ BulldozersHelmet (attrs `With` Meta 0)
    _ -> BulldozersHelmet . (`With` meta) <$> runMessage msg attrs
   where
    ours vid = attachmentEnemy attrs == Just (EnemyVillainId vid)
    withAttrs (x `With` _) = x
