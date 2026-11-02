-- | The attachment of The Mad Titan's Shadow's campaign set.
module Marvel.Attachment.Attachments.MadTitansShadowCampaign (
  jormungand,
  Jormungand (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity (isTarget)
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W

jormungand :: AttachmentCard Jormungand
jormungand = attachment Jormungand Cards.jormungand

-- | Attach to Loki. Loki gets +4[per_hero] hit points: his dial goes up as it attaches,
-- and down as it leaves with him still in play (as Beetle Armor Mk IV). Forced
-- Interrupt: when Loki is defeated, remove this card from the game, so the Loki who
-- takes his place does not keep it (and nothing is taken off the defeated one).
newtype Jormungand = Jormungand (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Jormungand where
  getAbilities (Jormungand a) = case attachmentEnemy a of
    Just (EnemyVillainId vid) ->
      [windowAbility a 1 (W.VillainIsDefeated W.When (VillainWithId vid)) ForcedInterrupt NoCost $ runAbility a 1]
    _ -> []

instance RunMessage Jormungand where
  runMessage msg a@(Jormungand attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      villain <- selectOne ActiveVillain
      case villain of
        Nothing -> removeFromGame attrs
        Just vid -> do
          attachTo (attachmentId attrs) vid
          changeVillainHitPoints vid . (4 *) =<< getStartingPlayerCount
      pure a
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> a <$ removeFromGame attrs
    RemoveFromPlay target | target == toTarget attrs -> do
      for_ (attachmentEnemy attrs) \case
        EnemyVillainId vid -> do
          inPlay <- selectAny (VillainWithId vid)
          when inPlay $ changeVillainHitPoints vid . negate . (4 *) =<< getStartingPlayerCount
        EnemyMinionId _ -> pure ()
        EnemyAttachmentId _ -> pure ()
      Jormungand <$> runMessage msg attrs
    _ -> Jormungand <$> runMessage msg attrs
