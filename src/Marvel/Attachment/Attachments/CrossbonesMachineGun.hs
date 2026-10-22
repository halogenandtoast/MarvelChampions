module Marvel.Attachment.Attachments.CrossbonesMachineGun (
  crossbonesMachineGun,
  CrossbonesMachineGun (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Boost (boostCount)
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.GameValue
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window

crossbonesMachineGun :: AttachmentCard CrossbonesMachineGun
crossbonesMachineGun = attachment CrossbonesMachineGun Cards.crossbonesMachineGun

newtype CrossbonesMachineGun = CrossbonesMachineGun (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | [star] Forced Interrupt: when Crossbones attacks you, remove 1 ammo counter and
-- discard the top card of the encounter deck → take indirect damage equal to its boost
-- icons.
instance HasAbilities CrossbonesMachineGun where
  getAbilities (CrossbonesMachineGun a) = case attachmentEnemy a of
    Just enemy | attachmentCounters a > 0 ->
      [windowAbility a 1 (EnemyAttacked When (EnemyWithId enemy) You) ForcedInterrupt NoCost $ runAbility a 1]
    _ -> []

-- | Attach to Crossbones. Uses (2[per_hero] ammo counters): with none left it is
-- discarded.
instance RunMessage CrossbonesMachineGun where
  runMessage msg a@(CrossbonesMachineGun attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      selectOne ActiveVillain >>= \case
        Nothing -> CrossbonesMachineGun attrs <$ removeFromPlay attrs
        Just villainId -> do
          attachedTo villainId ident
          ammo <- fromIntegral <$> fromGameValue (PerPlayer 2)
          placeCounters attrs ammo
          pure . CrossbonesMachineGun $ attrs & enemyL ?~ EnemyVillainId villainId
    RanAbility you (isTarget a -> True) 1 _ _ -> do
      discardTopOfEncounterDeckThen 1 \cards -> do
        let n = sum $ map (boostCount . cdBoostIcons . getCardDef) cards
        when (n > 0) $ dealIndirectDamage you attrs n
      removeCounters attrs 1
      pure a
    RemoveCounters target n | target == toRef attrs -> do
      when (attachmentCounters attrs <= n) $ removeFromPlay attrs
      CrossbonesMachineGun <$> runMessage msg attrs
    _ -> CrossbonesMachineGun <$> runMessage msg attrs
