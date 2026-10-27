module Marvel.Attachment.Attachments.ChallengeAccepted (
  challengeAccepted,
  ChallengeAccepted (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source (getSourceController)
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Window qualified as W

challengeAccepted :: AttachmentCard ChallengeAccepted
challengeAccepted = attachment ChallengeAccepted Cards.challengeAccepted

newtype ChallengeAccepted = ChallengeAccepted (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | The attached enemy gets +2 ATK (printed; MarvelCDB leaves it out).
instance HasModifiersFor ChallengeAccepted where
  getModifiersFor _ target (ChallengeAccepted a)
    | Just enemy <- attachmentEnemy a, target == enemyRef enemy = pure [AttackModifier 2]
  getModifiersFor _ _ _ = pure []

-- | Surge. Attach to the enemy with the highest ATK (the player decides ties).
-- Forced Response: After Drax deals 4 or more damage to attached enemy with a single
-- attack, discard this card.
instance RunMessage ChallengeAccepted where
  runMessage msg a@(ChallengeAccepted attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment you) | ident == attachmentId attrs -> do
      enemies <- selectList EnemyWithHighestAttack
      case enemies of
        [] -> removeFromPlay attrs
        [enemy] -> attachTo ident enemy
        _ -> chooseOne you [targeting (enemyRef enemy) (attachTo ident enemy) | enemy <- enemies]
      pure a
    -- the attack's own damage, or what its overkill dealt past a minion it defeated
    CheckWindows ws | Just enemy <- attachmentEnemy attrs -> do
      attackers <- fmap catMaybes $ sequence
        [ case damageSource d of
            FromPlayerAttack i -> pure (Just i)
            FromOverkill | isPlayerSource source -> getSourceController source
            _ -> pure Nothing
        | W.Window W.After (W.EnemyTookDamageFrom e d source) <- ws
        , e == enemy
        , damageAmount d >= 4
        ]
      byDrax <- anyM (identityMatches (IdentityWithTitle "Drax")) attackers
      a <$ when byDrax (removeFromPlay attrs)
    _ -> ChallengeAccepted <$> runMessage msg attrs
   where
    -- a hero's basic attack, or an attack event
    isPlayerSource = \case
      IdentityRef _ -> True
      EventRef _ -> True
      _ -> False
