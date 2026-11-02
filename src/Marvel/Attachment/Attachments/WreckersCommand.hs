module Marvel.Attachment.Attachments.WreckersCommand (
  wreckersCommand,
  WreckersCommand (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source ()
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

wreckersCommand :: AttachmentCard WreckersCommand
wreckersCommand = attachment WreckersCommand Cards.wreckersCommand

newtype WreckersCommand = WreckersCommand (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Attach to Wrecker. +1 SCH.
instance HasModifiersFor WreckersCommand where
  getModifiersFor _ (VillainRef vid) (WreckersCommand a) | attachmentEnemy a == Just (EnemyVillainId vid) = pure [SchemeModifier 1]
  getModifiersFor _ _ _ = pure []

-- | [star] Forced Response: after Wrecker schemes, place 1 threat on each other villain's
-- side scheme. Hero Action: spend [physical] [physical] resources -> discard this card.
instance HasAbilities WreckersCommand where
  getAbilities (WreckersCommand a) = case attachmentEnemy a of
    Just (EnemyVillainId vid) ->
      [ windowAbility a 1 (W.VillainSchemed After (VillainWithId vid)) ForcedResponse NoCost (runAbility a 1)
      , ability a 2 HeroAction NoCriteria (MultiResourceCost [Just Physical, Just Physical]) (DiscardTarget $ toRef a)
      ]
    _ -> []

instance RunMessage WreckersCommand where
  runMessage msg a@(WreckersCommand attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      villainNamed "Wrecker" >>= \case
        Just v -> attachTo ident v
        Nothing -> removeFromPlay attrs
      pure a
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      villains <- selectList AnyVillain
      for_ [v | v <- villains, Just (EnemyVillainId v) /= attachmentEnemy attrs] \v ->
        traverse_ (`placeThreat` 1) =<< signatureSchemeOf v
      pure a
    _ -> WreckersCommand <$> runMessage msg attrs
