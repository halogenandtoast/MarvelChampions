module Marvel.Event.Events.PhaseDisruption where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Ability qualified as Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Query
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Trait qualified as Trait ()
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Payment ()
import Marvel.Upgrade.Cards qualified as Upgrades ()
import Marvel.Window qualified as W ()

phaseDisruption :: EventCard PhaseDisruption
phaseDisruption = event PhaseDisruption Cards.phaseDisruption

newtype PhaseDisruption = PhaseDisruption (Attrs Event)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

phaseTargets :: (HasGame m) => m [EnemyId]
phaseTargets = do
  enemies <- selectList AnyEnemy
  filterM (\enemy -> do
    confused <- enemyMatches ConfusedEnemy enemy
    stalwart <- isStalwartEnemy (enemyRef enemy)
    attachment <- selectAny (AttachmentOnEnemyWithHeroAbility enemy)
    pure (attachment || (not confused && not stalwart))) enemies

instance IsEvent PhaseDisruption where
  eventTargets _ = Just . map enemyRef <$> phaseTargets

instance RunMessage PhaseDisruption where
  runMessage msg e@(PhaseDisruption attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you payment _) | eid == eventId attrs -> do
      let enemy = case paymentTarget payment of
            Just (VillainRef vid) -> Just (EnemyVillainId vid)
            Just (MinionRef mid) -> Just (EnemyMinionId mid)
            _ -> Nothing
      for_ enemy \target -> do
        confuse target attrs
        cardSpecific attrs "discardAttachment" (you, target)
      pure e
    CardSpecific (isTarget attrs -> True) "discardAttachment" (fromJSON -> Success (you, enemy)) -> do
      attachments <- selectList (AttachmentOnEnemyWithHeroAbility enemy)
      unless (null attachments) $ chooseOne you [targeting aid (removeFromPlay aid) | aid <- attachments]
      pure e
    _ -> PhaseDisruption <$> runMessage msg attrs
