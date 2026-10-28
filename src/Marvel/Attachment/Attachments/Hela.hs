-- | The Hela set's attachments (The Mad Titan's Shadow, scenario 4): each attaches to
-- Hela, the villain, as it is revealed, and as its boost ability resolves.
module Marvel.Attachment.Attachments.Hela (
  nightsword,
  Nightsword (..),
  helasCrown,
  HelasCrown (..),
  helasCloak,
  HelasCloak (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Attachments.Thanos (onTheVillain)
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity (isTarget)
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W

-- | Revealed: attach to Hela. [star] Boost: attach this card to Hela (with no villain in
-- play, nothing).
attachToHela :: (ReverseQueue m) => Attrs Attachment -> Message -> Maybe (m ())
attachToHela attrs = \case
  AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> Just (attachToTheVillain attrs)
  Boost (RevealedAsBoost target _) | isTarget attrs target -> Just do
    selectOne ActiveVillain >>= traverse_ \villain -> do
      putBoostIntoPlay attrs =<< getActivePlayerId
      attachTo (attachmentId attrs) villain
  _ -> Nothing

nightsword :: AttachmentCard Nightsword
nightsword = attachment Nightsword Cards.nightsword

-- | +1 ATK. [star] Hela's attacks gain piercing.
newtype Nightsword = Nightsword (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Nightsword where
  getModifiersFor _ target (Nightsword a) = onTheVillain a target [AttackModifier 1, KeywordModifier Piercing]

instance RunMessage Nightsword where
  runMessage msg a@(Nightsword attrs) = runQueueT $ case msg of
    _ | Just body <- attachToHela attrs msg -> a <$ body
    _ -> Nightsword <$> runMessage msg attrs

helasCrown :: AttachmentCard HelasCrown
helasCrown = attachment HelasCrown Cards.helasCrown

-- | +1 SCH. [star] Forced Response: after Hela schemes, give her a facedown boost card
-- (it stays on her for her next activation).
newtype HelasCrown = HelasCrown (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor HelasCrown where
  getModifiersFor _ target (HelasCrown a) = onTheVillain a target [SchemeModifier 1]

instance HasAbilities HelasCrown where
  getAbilities (HelasCrown a) = case attachmentEnemy a of
    Just (EnemyVillainId vid) -> [windowAbility a 1 (W.VillainSchemed W.After (VillainWithId vid)) ForcedResponse NoCost $ runAbility a 1]
    _ -> []

instance RunMessage HelasCrown where
  runMessage msg a@(HelasCrown attrs) = runQueueT $ case msg of
    _ | Just body <- attachToHela attrs msg -> a <$ body
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      for_ (attachmentEnemy attrs) \case
        EnemyVillainId vid -> dealBoost (VillainRef vid)
        _ -> pure ()
      pure a
    _ -> HelasCrown <$> runMessage msg attrs

helasCloak :: AttachmentCard HelasCloak
helasCloak = attachment HelasCloak Cards.helasCloak

-- | Hela gains stalwart.
newtype HelasCloak = HelasCloak (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor HelasCloak where
  getModifiersFor _ target (HelasCloak a) = onTheVillain a target [KeywordModifier Stalwart]

instance RunMessage HelasCloak where
  runMessage msg a@(HelasCloak attrs) = runQueueT $ case msg of
    _ | Just body <- attachToHela attrs msg -> a <$ body
    _ -> HelasCloak <$> runMessage msg attrs
