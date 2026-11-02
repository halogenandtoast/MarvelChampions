module Marvel.Attachment.Attachments.EnergyTruncheonShuri where
import Marvel.Prelude
import Marvel.Query
import Marvel.Id
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Question (runAbility)
import Marvel.Window qualified as W ()
import Marvel.Trait qualified as T ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Card
import Marvel.Card.Id ()
import Marvel.Silk.Helpers ()
import Marvel.Keyword

import Marvel.Ability
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.BlackPantherShuri.Cards qualified as Cards
import Marvel.Attachment.Types

energyTruncheonShuri :: AttachmentCard EnergyTruncheonShuri
energyTruncheonShuri = attachment EnergyTruncheonShuri Cards.energyTruncheonShuri
newtype EnergyTruncheonShuri = EnergyTruncheonShuri (Attrs Attachment)
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance RunMessage EnergyTruncheonShuri where
  runMessage msg u@(EnergyTruncheonShuri attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment _) | aid == attachmentId attrs -> do
      joystick <- selectOne (MinionIs Cards.joystickShuri)
      enemy <- case joystick of
        Just mid -> pure $ Just $ EnemyMinionId mid
        Nothing -> fmap EnemyVillainId <$> selectOne ActiveVillain
      for_ enemy $ \host -> do
        attachTo (attachmentId attrs) host
        attachedTo host (attachmentId attrs)
      pure $ EnergyTruncheonShuri attrs {attachmentEnemy = enemy}
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      for_ (attachmentEnemy attrs) $ \enemy -> do
        enemyAttacks enemy you
        cardSpecific attrs "reward" you
      pure u
    CardSpecific (isTarget attrs -> True) "reward" (fromJSON -> Success (you :: IdentityId)) -> do
      removeFromPlay attrs
      drawCards you 1
      pure u
    _ -> EnergyTruncheonShuri <$> runMessage msg attrs

instance HasAbilities EnergyTruncheonShuri where
  getAbilities u@(EnergyTruncheonShuri _attrs) = [ability u 1 HeroAction NoCriteria NoCost (runAbility u 1)]

instance HasModifiersFor EnergyTruncheonShuri where
  getModifiersFor _ target (EnergyTruncheonShuri attrs) = pure [modifier | Just target == (enemyRef <$> attachmentEnemy attrs), modifier <- [AttackModifier 1, KeywordModifier Piercing]]
