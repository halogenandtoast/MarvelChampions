module Marvel.Attachment.Attachments.UpgradedDrones (
  upgradedDrones,
  UpgradedDrones (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Environment.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message
import Marvel.Minion.Cards qualified as Cards
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Resource

upgradedDrones :: AttachmentCard UpgradedDrones
upgradedDrones = attachment UpgradedDrones Cards.upgradedDrones

newtype UpgradedDrones = UpgradedDrones (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Each facedown Drone minion gets +1 ATK and +1 hit point.
instance HasModifiersFor UpgradedDrones where
  getModifiersFor _ (MinionRef mid) _ = do
    facedown <- minionMatches (MinionIs Cards.drone) mid
    pure $ if facedown then [AttackModifier 1, HitPointModifier 1] else []
  getModifiersFor _ _ _ = pure []

instance HasAbilities UpgradedDrones where
  getAbilities (UpgradedDrones a) =
    [ ability
        a
        1
        HeroAction
        NoCriteria
        (MultiResourceCost [Just Energy, Just Mental, Just Physical])
        (TargetLabel (toRef a) [DiscardTarget $ toRef a])
    ]

instance RunMessage UpgradedDrones where
  runMessage msg a@(UpgradedDrones attrs) = case msg of
    -- attached to the Ultron Drones environment
    AttachmentMessage ident (RevealAttachment _) | ident == attachmentId attrs -> do
      environments <- selectList (EnvironmentIs Cards.ultronDrones)
      pushAll [EnvironmentMessage e (AttachedToEnvironment ident) | e <- take 1 environments]
      pure a
    _ -> UpgradedDrones <$> runMessage msg attrs
