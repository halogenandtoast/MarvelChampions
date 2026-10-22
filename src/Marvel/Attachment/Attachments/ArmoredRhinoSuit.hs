module Marvel.Attachment.Attachments.ArmoredRhinoSuit where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Damage
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W

armoredRhinoSuit :: AttachmentCard ArmoredRhinoSuit
armoredRhinoSuit = attachment ArmoredRhinoSuit Cards.armoredRhinoSuit

newtype ArmoredRhinoSuit = ArmoredRhinoSuit (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities ArmoredRhinoSuit where
  getAbilities (ArmoredRhinoSuit a) = case attachmentEnemy a of
    Just (EnemyVillainId vid) ->
      [ windowAbility
          a
          1
          (W.VillainDamaged W.When $ VillainWithId vid)
          ForcedInterrupt
          NoCost
          $ runAbility a 1
      ]
    _ -> []

getDetails :: [W.WindowType] -> (VillainId, Natural)
getDetails [] = error "Invalid call"
getDetails (W.DamagedVillain vid n : _) = (vid, damageAmount n)
getDetails (_ : xs) = getDetails xs

instance RunMessage ArmoredRhinoSuit where
  runMessage msg a@(ArmoredRhinoSuit attrs) = runQueueT $ case msg of
    AttachmentMessage ident msg' | ident == attachmentId attrs -> case msg' of
      RevealAttachment _ -> do
        -- with no villain in play to attach to, it is discarded
        selectOne ActiveVillain >>= \case
          Nothing -> ArmoredRhinoSuit attrs <$ removeFromPlay attrs
          Just villainId -> do
            attachedTo villainId ident
            pure . ArmoredRhinoSuit $ attrs & enemyL ?~ EnemyVillainId villainId
      AttachmentDamaged n -> do
        when (attachmentDamage attrs + n >= 5) $ removeFromPlay a
        pure . ArmoredRhinoSuit $ attrs & damageL +~ n
      _ -> ArmoredRhinoSuit <$> runMessage msg attrs
    RanAbility _ (isTarget a -> True) 1 (getDetails -> (vid, dmg)) _ -> do
      flip insteadOf (damageAttachment (attachmentId attrs) dmg) \case
        VillainMessage vid' (VillainDamaged _ _) | vid == vid' -> True
        _ -> False
      pure a
    _ -> ArmoredRhinoSuit <$> runMessage msg attrs
