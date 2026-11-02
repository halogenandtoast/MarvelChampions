module Marvel.Attachment.Attachments.SizeIncrease (
  sizeIncrease,
  SizeIncrease (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Entity
import Marvel.Question
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Window hiding (VillainSchemed)
import Marvel.Window qualified as W

sizeIncrease :: AttachmentCard SizeIncrease
sizeIncrease = attachment SizeIncrease Cards.sizeIncrease

newtype SizeIncrease = SizeIncrease (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | +2 SCH and +2 ATK to the attached enemy.
instance HasModifiersFor SizeIncrease where
  getModifiersFor _ target (SizeIncrease a)
    | Just enemy <- attachmentEnemy a, isEnemy enemy = pure [SchemeModifier 2, AttackModifier 2]
   where
    isEnemy = \case
      EnemyVillainId vid -> target == VillainRef vid
      EnemyMinionId mid -> target == MinionRef mid
      EnemyAttachmentId aid -> target == AttachmentRef aid
  getModifiersFor _ _ _ = pure []

-- | Forced Response: After attached enemy activates, remove 1 size counter from here: its
-- attack, or its scheme.
instance HasAbilities SizeIncrease where
  getAbilities (SizeIncrease a) = case attachmentEnemy a of
    Just enemy | attachmentCounters a > 0 ->
      [ windowAbility a 1 (WindowOneOf [EnemyAttacked After (EnemyWithId enemy) AnyIdentity, schemed enemy]) ForcedResponse NoCost $
          runAbility a 1
      ]
    _ -> []
   where
    schemed = \case
      EnemyVillainId vid -> W.VillainSchemed After (VillainWithId vid)
      EnemyMinionId mid -> MinionHasSchemed After (MinionWithId mid)
      EnemyAttachmentId _ -> WindowOneOf []

-- | Attach to Yellowjacket, if able. If you cannot, attach to the villain. Uses (3 size
-- counters): with none left it is discarded.
instance RunMessage SizeIncrease where
  runMessage msg a@(SizeIncrease attrs) = runQueueT $ case msg of
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      yellowjacket <- selectOne $ MinionIs Minions.yellowjacket
      villain <- selectOne ActiveVillain
      case (EnemyMinionId <$> yellowjacket) <|> (EnemyVillainId <$> villain) of
        Nothing -> SizeIncrease attrs <$ removeFromPlay attrs
        Just enemy -> do
          attachedTo enemy ident
          placeCounters attrs 3
          pure . SizeIncrease $ attrs & enemyL ?~ enemy
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      removeCounters attrs 1
      pure a
    RemoveCounters target n | target == toRef attrs -> do
      when (attachmentCounters attrs <= n) $ removeFromPlay attrs
      SizeIncrease <$> runMessage msg attrs
    _ -> SizeIncrease <$> runMessage msg attrs
