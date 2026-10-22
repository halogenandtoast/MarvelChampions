module Marvel.Attachment.Attachments.AbsorbingMansBallAndChain (
  absorbingMansBallAndChain,
  AbsorbingMansBallAndChain (..),
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

absorbingMansBallAndChain :: AttachmentCard AbsorbingMansBallAndChain
absorbingMansBallAndChain = attachment AbsorbingMansBallAndChain Cards.absorbingMansBallAndChain

newtype AbsorbingMansBallAndChain = AbsorbingMansBallAndChain (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | +1 ATK and +1 SCH to Absorbing Man.
instance HasModifiersFor AbsorbingMansBallAndChain where
  getModifiersFor _ (VillainRef vid) (AbsorbingMansBallAndChain a)
    | Just (EnemyVillainId vid) == attachmentEnemy a = pure [AttackModifier 1, SchemeModifier 1]
  getModifiersFor _ _ _ = pure []

-- | Hero Action: spend a [physical] resource → shuffle this card into the encounter deck.
instance HasAbilities AbsorbingMansBallAndChain where
  getAbilities (AbsorbingMansBallAndChain a) = case attachmentEnemy a of
    Just _ -> [ability a 1 HeroAction NoCriteria (MultiResourceCost [Just Physical]) $ runAbility a 1]
    Nothing -> []

instance RunMessage AbsorbingMansBallAndChain where
  runMessage msg a@(AbsorbingMansBallAndChain attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      villainId <- selectJust ActiveVillain
      attachedTo villainId ident
      pure . AbsorbingMansBallAndChain $ attrs & enemyL ?~ EnemyVillainId villainId
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
    _ -> AbsorbingMansBallAndChain <$> runMessage msg attrs
