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
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
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
  runMessage msg a@(ArmoredRhinoSuit attrs) = case msg of
    AttachmentMessage ident msg' | ident == attachmentId attrs -> case msg' of
      RevealAttachment _ -> do
        villainId <- selectJust ActiveVillain
        push $ VillainMessage villainId $ AttachedToVillain ident
        pure . ArmoredRhinoSuit $ attrs & enemyL ?~ EnemyVillainId villainId
      AttachmentDamaged n -> do
        when
          (attachmentDamage attrs + n >= 5)
          (push $ RemoveFromPlay (toTarget a))
        pure . ArmoredRhinoSuit $ attrs & damageL +~ n
      _ -> ArmoredRhinoSuit <$> runMessage msg attrs
    RanAbility (isTarget a -> True) 1 (getDetails -> (vid, dmg)) _ -> do
      replaceMatchingMessage
        (const [AttachmentMessage (attachmentId attrs) (AttachmentDamaged dmg)])
        $ \case
          VillainMessage vid' (VillainDamaged _ _) | vid == vid' -> True
          _ -> False
      pure a
    _ -> ArmoredRhinoSuit <$> runMessage msg attrs
