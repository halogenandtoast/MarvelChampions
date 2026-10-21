module Marvel.Attachment.Attachments.BiomechanicalUpgrades (
  biomechanicalUpgrades,
  BiomechanicalUpgrades (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Choice
import Marvel.Cost
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

biomechanicalUpgrades :: AttachmentCard BiomechanicalUpgrades
biomechanicalUpgrades = attachment BiomechanicalUpgrades Cards.biomechanicalUpgrades

newtype BiomechanicalUpgrades = BiomechanicalUpgrades (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities BiomechanicalUpgrades where
  getAbilities (BiomechanicalUpgrades a) = case attachmentEnemy a of
    Just (EnemyMinionId mid) ->
      [ windowAbility a 1 (W.MinionDefeated W.When $ MinionWithId mid) ForcedInterrupt NoCost $
          RunAbility (toTarget a) 1
      ]
    _ -> []

instance RunMessage BiomechanicalUpgrades where
  runMessage msg a@(BiomechanicalUpgrades attrs) = case msg of
    -- attach to the minion with the highest printed hit points without another
    -- Biomechanical Upgrades attached; with none, it is discarded
    AttachmentMessage ident (RevealAttachment identityId) | ident == attachmentId attrs -> do
      minions <-
        selectList $
          MinionWithHighestPrintedHitPointsAmong $
            NotMinion $
              MinionWithAttachment (AttachmentIs Cards.biomechanicalUpgrades)
      if null minions
        then push $ RemoveFromPlay (toTarget attrs)
        else
          chooseOrRunOne
            identityId
            [ TargetLabel (toRef mid) [Run [AttachmentMessage ident $ AttachedToEnemy (EnemyMinionId mid)]]
            | mid <- minions
            ]
      pure a
    -- when the minion would be defeated, all its damage is healed instead and this is
    -- discarded
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> case attachmentEnemy attrs of
      Just (EnemyMinionId mid) -> do
        let
          defeat = \case
            MinionMessage mid' MinionDefeated -> mid == mid'
            CheckWindows [W.Window W.After (W.DefeatedMinion mid' _)] -> mid == mid'
            _ -> False
        cancelMatchingMessage defeat
        cancelMatchingMessage defeat
        pushAll [MinionMessage mid MinionHealAllDamage, RemoveFromPlay (toTarget attrs)]
        pure a
      _ -> pure a
    _ -> BiomechanicalUpgrades <$> runMessage msg attrs
