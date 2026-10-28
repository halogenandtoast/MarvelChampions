-- | The Loki set's attachments (The Mad Titan's Shadow, scenario 5): each attaches to
-- Loki, the villain, as it is revealed.
module Marvel.Attachment.Attachments.Loki (
  lokisStaff,
  LokisStaff (..),
  lokisCrown,
  LokisCrown (..),
  lokisCape,
  LokisCape (..),
  masterOfIllusions,
  MasterOfIllusions (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Attachments.Thanos (discardAfterBasicAttack, onTheVillain)
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Damage
import Marvel.Entity (isTarget)
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Window qualified as W

-- | "[star] Boost: Attach this card to Loki": with no villain in play, nothing.
attachAsBoost :: (ReverseQueue m) => Attrs Attachment -> Message -> Maybe (m ())
attachAsBoost attrs = \case
  Boost (RevealedAsBoost target _) | isTarget attrs target -> Just do
    selectOne ActiveVillain >>= traverse_ \villain -> do
      putBoostIntoPlay attrs =<< getActivePlayerId
      attachTo (attachmentId attrs) villain
  _ -> Nothing

-- | Revealed: attach to Loki.
attachToLoki :: (ReverseQueue m) => Attrs Attachment -> Message -> Maybe (m ())
attachToLoki attrs = \case
  AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> Just (attachToTheVillain attrs)
  _ -> Nothing

lokisStaff :: AttachmentCard LokisStaff
lokisStaff = attachment LokisStaff Cards.lokisStaff

-- | +1 ATK. Hero Response: after you make a basic attack against Loki, spend [energy]
-- [physical] resources → discard this card. [star] Boost: attach this card to Loki.
newtype LokisStaff = LokisStaff (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor LokisStaff where
  getModifiersFor _ target (LokisStaff a) = onTheVillain a target [AttackModifier 1]

instance HasAbilities LokisStaff where
  getAbilities (LokisStaff a) = discardAfterBasicAttack a [Energy, Physical]

instance RunMessage LokisStaff where
  runMessage msg a@(LokisStaff attrs) = runQueueT $ case msg of
    _ | Just body <- attachToLoki attrs msg <|> attachAsBoost attrs msg -> a <$ body
    _ -> LokisStaff <$> runMessage msg attrs

lokisCrown :: AttachmentCard LokisCrown
lokisCrown = attachment LokisCrown Cards.lokisCrown

-- | +1 SCH. Hero Response: after you make a basic attack against Loki, spend [mental]
-- [physical] resources → discard this card. [star] Boost: attach this card to Loki.
newtype LokisCrown = LokisCrown (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor LokisCrown where
  getModifiersFor _ target (LokisCrown a) = onTheVillain a target [SchemeModifier 1]

instance HasAbilities LokisCrown where
  getAbilities (LokisCrown a) = discardAfterBasicAttack a [Mental, Physical]

instance RunMessage LokisCrown where
  runMessage msg a@(LokisCrown attrs) = runQueueT $ case msg of
    _ | Just body <- attachToLoki attrs msg <|> attachAsBoost attrs msg -> a <$ body
    _ -> LokisCrown <$> runMessage msg attrs

lokisCape :: AttachmentCard LokisCape
lokisCape = attachment LokisCape Cards.lokisCape

-- | Forced Response: after Loki is swapped with a set-aside Loki villain, give him a
-- tough status card. Hero Response: after you make a basic attack against Loki, spend
-- [energy] [mental] resources → discard this card.
newtype LokisCape = LokisCape (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities LokisCape where
  getAbilities (LokisCape a) =
    discardAfterBasicAttack a [Energy, Mental] <> case attachmentEnemy a of
      Just (EnemyVillainId vid) ->
        [windowAbility a 2 (W.VillainWasSwapped W.After (VillainWithId vid)) ForcedResponse NoCost $ runAbility a 2]
      _ -> []

instance RunMessage LokisCape where
  runMessage msg a@(LokisCape attrs) = runQueueT $ case msg of
    _ | Just body <- attachToLoki attrs msg -> a <$ body
    RanAbility _ (isTarget attrs -> True) 2 windows _ -> do
      for_ [vid | W.VillainSwapped vid <- windows] enemyBecomesTough
      pure a
    _ -> LokisCape <$> runMessage msg attrs

masterOfIllusions :: AttachmentCard MasterOfIllusions
masterOfIllusions = attachment MasterOfIllusions Cards.masterOfIllusions

-- | Forced Interrupt: when Loki would take damage from an attack (its overkill too, RR
-- Overkill), discard the top card of the encounter deck. If that card is a treachery,
-- prevent all damage from this attack (the instance pending, and every other the attack
-- deals him until it ends) and discard this card. Damage he cannot take opens no such
-- window.
newtype MasterOfIllusions = MasterOfIllusions (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities MasterOfIllusions where
  getAbilities (MasterOfIllusions a) = case attachmentEnemy a of
    Just (EnemyVillainId vid) ->
      [windowAbility a 1 (W.VillainWouldTakeDamage (VillainWithId vid) DamageFromAnAttack) ForcedInterrupt NoCost $ runAbility a 1]
    _ -> []

instance RunMessage MasterOfIllusions where
  runMessage msg a@(MasterOfIllusions attrs) = runQueueT $ case msg of
    _ | Just body <- attachToLoki attrs msg -> a <$ body
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [vid | W.DamagedVillain vid _ <- windows] \vid -> do
        top <- take 1 <$> getEncounterDeck
        discardTopOfEncounterDeck 1
        when (any (cardMatch $ CardWithType TreacheryType) top) do
          let damaged = \case
                VillainMessage vid' (VillainDamaged _ _) -> vid' == vid
                _ -> False
          -- ahead of the damage pending: the attack it is, by what makes it, deals him
          -- no damage until it ends
          replaceMatchingMessage
            ( \case
                m@(VillainMessage v (VillainDamaged source _)) -> [VillainMessage v (VillainPreventAttackDamage source), m]
                m -> [m]
            )
            damaged
          removeFromPlay attrs
      pure a
    _ -> MasterOfIllusions <$> runMessage msg attrs
