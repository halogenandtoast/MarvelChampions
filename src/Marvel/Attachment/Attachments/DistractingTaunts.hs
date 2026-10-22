module Marvel.Attachment.Attachments.DistractingTaunts (
  distractingTaunts,
  DistractingTaunts (..),
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

distractingTaunts :: AttachmentCard DistractingTaunts
distractingTaunts = attachment DistractingTaunts Cards.distractingTaunts

newtype DistractingTaunts = DistractingTaunts (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Attach to Piledriver. Piledriver gets +3 hit points. Players cannot attack other
-- villains.
instance HasModifiersFor DistractingTaunts where
  getModifiersFor _ (VillainRef vid) (DistractingTaunts a) = pure case attachmentEnemy a of
    Just (EnemyVillainId pile)
      | pile == vid -> [HitPointModifier 3]
      | otherwise -> [CannotBeAttacked]
    _ -> []
  getModifiersFor _ _ _ = pure []

-- | Response: after your hero attacks Piledriver, spend [physical] [physical] resources
-- -> discard this card.
instance HasAbilities DistractingTaunts where
  getAbilities (DistractingTaunts a) = case attachmentEnemy a of
    Just enemy@(EnemyVillainId _) ->
      [ windowAbility a 1 (IdentityAttacked After (You <> HeroIdentity) (EnemyWithId enemy)) Response (MultiResourceCost [Just Physical, Just Physical]) (DiscardTarget $ toRef a)
      ]
    _ -> []

-- | The hit points are his while it is attached: gained as it is, lost as it leaves with
-- him still in play.
instance RunMessage DistractingTaunts where
  runMessage msg a@(DistractingTaunts attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      villainNamed "Piledriver" >>= \case
        Just v -> do
          attachTo ident v
          changeVillainHitPoints v 3
        Nothing -> removeFromPlay attrs
      pure a
    RemoveFromPlay target | target == toTarget attrs -> do
      for_ (attachmentEnemy attrs) \case
        EnemyVillainId v -> do
          inPlay <- selectAny (VillainWithId v)
          when inPlay $ changeVillainHitPoints v (-3)
        _ -> pure ()
      DistractingTaunts <$> runMessage msg attrs
    _ -> DistractingTaunts <$> runMessage msg attrs
