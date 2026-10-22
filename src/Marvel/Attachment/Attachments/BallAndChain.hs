module Marvel.Attachment.Attachments.BallAndChain (
  ballAndChain,
  BallAndChain (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card
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

ballAndChain :: AttachmentCard BallAndChain
ballAndChain = attachment BallAndChain Cards.ballAndChain

newtype BallAndChain = BallAndChain (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | +1 ATK and +1 SCH to Absorbing Man.
instance HasModifiersFor BallAndChain where
  getModifiersFor _ (VillainRef vid) (BallAndChain a)
    | Just (EnemyVillainId vid) == attachmentEnemy a = pure [AttackModifier 1, SchemeModifier 1]
  getModifiersFor _ _ _ = pure []

-- | Hero Action: spend a [physical] resource → shuffle this card into the encounter deck.
instance HasAbilities BallAndChain where
  getAbilities (BallAndChain a) = case attachmentEnemy a of
    Just _ -> [ability a 1 HeroAction NoCriteria (MultiResourceCost [Just Physical]) $ runAbility a 1]
    Nothing -> []

instance RunMessage BallAndChain where
  runMessage msg a@(BallAndChain attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      villainId <- selectJust ActiveVillain
      attachedTo villainId ident
      pure . BallAndChain $ attrs & enemyL ?~ EnemyVillainId villainId
    -- it leaves play for the encounter deck, by way of its discard pile
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      removeFromPlay attrs
      case toCard attrs of
        EncounterCard card -> shuffleIntoEncounterDeck [card]
        _ -> pure ()
      pure a
    -- [star] Boost: reveal this card
    Boost (RevealedAsBoost target enemy) | isTarget attrs target -> do
      you <- getBoostedPlayer enemy
      revealBoost attrs you
      pure a
    _ -> BallAndChain <$> runMessage msg attrs
