module Marvel.Attachment.Attachments.CyborgTech where

import Marvel.Prelude
import Data.HashSet qualified as HashSet
import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Card.Code
import Marvel.Card.Def (cdTraits)
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait

cyborgTech :: AttachmentCard CyborgTech
cyborgTech = attachment CyborgTech Cards.cyborgTech

newtype CyborgTech = CyborgTech (Attrs Attachment)
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor CyborgTech where
  getPrintedModifiersFor _ (MinionRef mid) (CyborgTech attrs)
    | attachmentEnemy attrs == Just (EnemyMinionId mid) = pure [AttackModifier 1]
  getPrintedModifiersFor _ _ _ = pure []
  getModifiersFor _ (MinionRef mid) (CyborgTech attrs)
    | attachmentEnemy attrs == Just (EnemyMinionId mid) = pure [HitPointModifier 3, KeywordModifier $ Retaliate 1]
  getModifiersFor _ _ _ = pure []

instance RunMessage CyborgTech where
  runMessage msg a@(CyborgTech attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      mids <- selectList AnyMinion
      counts <- for mids $ \mid -> do
        traits <- getsGame $ maybe mempty cdTraits . (`abilityCardDef` MinionRef mid)
        gained <- getGainedTraits mid
        pure (mid, HashSet.size $ traits <> gained)
      case counts of
        [] -> removeFromPlay attrs >> surge you
        _ -> do
          let most = maximum (map snd counts)
          players <- getPlayers
          chooseOrRunOne (fromMaybe you $ headMay players) [targeting mid (attachTo aid mid) | (mid, n) <- counts, n == most]
      pure a
    Boost (RevealedAsBoost target _) | isTarget attrs target -> do
      you <- getActivePlayerId
      dealBoostAsEncounterCard attrs you
      pure a
    _ -> CyborgTech <$> runMessage msg attrs
