module Marvel.Attachment.Attachments.NextEvolutionMisterSinister where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword qualified as Keyword
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Resource
import Marvel.Status
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
import Marvel.NextEvolution.Sinister

import Marvel.Card
import Marvel.Entity
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types hiding (attachToTheVillain)
import Marvel.Ability (HasAbilities)
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype MisterSinisterAttachment = MisterSinisterAttachment (Attrs Attachment)
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40144 :: AttachmentCard MisterSinisterAttachment
ne40144 = attachment MisterSinisterAttachment Cards.ne40144

instance RunMessage MisterSinisterAttachment where
  runMessage msg card@(MisterSinisterAttachment a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment _) | aid == attachmentId a -> card <$ attachToTheVillain aid
    RanAbility _ (isTarget a -> True) 1 windows _ -> do
      for_ [(enemy, source, dmg) | W.EnemyDamageWouldBeDealt enemy source dmg <- windows] \(enemy, source, dmg) -> do
        player <- getSourceController source
        for_ player \you -> do
          affordable <- canAffordCost you (toRef a) (MultiResourceCost [Just Mental, Just Mental])
          chooseOne you $
            [labeled "$label.spendMentalMental" $ do
              continuation <- capture $ removeFromPlay a
              payCostThen you a (MultiResourceCost [Just Mental, Just Mental]) continuation
            | affordable]
            <> [labeled "$label.redirectDamage" $ do
              cancelEnemyDamage enemy
              characters <- friendlyCharactersWithFewestRemainingHitPoints
              chooseOne you [targeting (characterRef character) $ do
                case character of
                  IdentityCharacter p -> damageIdentity p source dmg
                  _ -> damageCharacter character source dmg
                removeFromPlay a
                | character <- characters]]
      pure card
    _ -> MisterSinisterAttachment <$> runMessage msg a

instance HasAbilities MisterSinisterAttachment where
  getAbilities card@(MisterSinisterAttachment a) =
    [limitedWindowAbility card 1 (W.EnemyWouldBeDealtDamageBy (maybe (NotEnemy AnyEnemy) EnemyWithId $ attachmentEnemy a) $ DamageByPlayer AnyIdentity)
      ForcedInterrupt NoCriteria NoCost (runAbility card 1)]
