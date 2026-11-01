module Marvel.Attachment.Attachments.NextEvolutionNeStryfe where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Ability
import Marvel.Question (runAbility)
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source ()
import Marvel.Id
import Marvel.Matchers
import Marvel.Query
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Resource
import Marvel.Window qualified as W
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Modifier
import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype NeStryfeAttachment = NeStryfeAttachment (Attrs Attachment)
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40169 :: AttachmentCard NeStryfeAttachment
ne40169 = attachment NeStryfeAttachment Cards.ne40169

ne40170 :: AttachmentCard NeStryfeAttachment
ne40170 = attachment NeStryfeAttachment Cards.ne40170

ne40171 :: AttachmentCard NeStryfeAttachment
ne40171 = attachment NeStryfeAttachment Cards.ne40171

ne40172 :: AttachmentCard NeStryfeAttachment
ne40172 = attachment NeStryfeAttachment Cards.ne40172

ne40173 :: AttachmentCard NeStryfeAttachment
ne40173 = attachment NeStryfeAttachment Cards.ne40173

instance HasModifiersFor NeStryfeAttachment where
  getModifiersFor _ target (NeStryfeAttachment a) | Just you <- attachmentIdentity a, target == toRef you = pure $ case toCardCode a of
    "40171" -> [CardsEnterPlayExhausted $ CardMatchesAny $ map CardWithType [AllyType, UpgradeType, SupportType]]
    "40172" -> [AdditionalResourceCost (CardMatchesAny $ map CardWithType [AllyType, SupportType]) 2]
    "40173" -> [AttackPenalty 1, ThwartPenalty 1]
    _ -> []
  getModifiersFor _ _ _ = pure []

instance HasAbilities NeStryfeAttachment where
  getAbilities (NeStryfeAttachment a) = case attachmentIdentity a of
    Just you -> case toCardCode a of
      "40170" ->
        [windowAbility a 1 (W.PlaysCard W.After (IdentityWithId you) (CardMatchesAny $ map CardWithType [EventType, UpgradeType])) ForcedResponse NoCost (runAbility a 1),
         windowAbility a 2 (W.UsesRecovery $ IdentityWithId you) Response (ResourceCost $ Just Mental) (runAbility a 2)]
      "40171" -> [ability a 2 AlterEgoAction (SelfMatches $ IdentityWithId you) (ExhaustControlledCardsCost 3) (runAbility a 2)]
      "40172" -> [windowAbility a 2 (W.PlaysCard W.After (IdentityWithId you) (CardMatchesAny $ map CardWithType [AllyType, SupportType])) Response (ExhaustCharacterCost $ CharacterWithId $ IdentityCharacter you) (runAbility a 2)]
      "40173" -> [ability a 2 HeroAction (SelfMatches (IdentityWithId you) <> IdentityAttackedAndThwartedThisPhase (IdentityWithId you)) NoCost (runAbility a 2)]
      "40169" -> [windowAbility a 3 (W.EnemyTookDamage (EnemyWithTitle "Stryfe") AnyDamage) ForcedResponse NoCost (runAbility a 3)]
      _ -> []
    Nothing -> [windowAbility a 3 (W.EnemyTookDamage (EnemyWithTitle "Stryfe") AnyDamage) ForcedResponse NoCost (runAbility a 3) | toCardCode a == "40169", isJust (attachmentAlly a)]

instance RunMessage NeStryfeAttachment where
  runMessage msg e@(NeStryfeAttachment a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      if toCardCode a == "40169"
        then do
          grasp <- selectAny (SideSchemeIs SideSchemes.ne40168a)
          hope <- selectOne (AllyWithTitle "Hope Summers")
          case (grasp, hope) of
            (True, Just ally) -> attachToAlly aid ally
            _ -> attachToIdentity aid you
        else attachToIdentity aid you
      pure e
    RanAbility you (isTarget a -> True) 1 _ _ -> damageIdentity you a (toDamage 1 FromAbility) >> pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> removeFromPlay a >> pure e
    RanAbility _ (isTarget a -> True) 3 windows _ -> do
      for_ [damage | W.EnemyTookDamageFrom _ damage _ <- windows] $ \damage -> do
        traverse_ (\you -> damageIdentity you a $ toDamage (damageAmount damage) FromAbility) (attachmentIdentity a)
        traverse_ (\ally -> damageCharacter ally a $ toDamage (damageAmount damage) FromAbility) (attachmentAlly a)
      removeFromPlay a
      pure e
    _ -> NeStryfeAttachment <$> runMessage msg a
