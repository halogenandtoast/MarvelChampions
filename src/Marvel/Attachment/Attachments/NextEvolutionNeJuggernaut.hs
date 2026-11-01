module Marvel.Attachment.Attachments.NextEvolutionNeJuggernaut where

import Data.Maybe (maybeToList)

import Marvel.Prelude
import Marvel.Id
import Marvel.Status ()
import Data.HashMap.Strict qualified as HashMap ()
import Marvel.Ability
    ( AbilityType(HeroResponse, HeroAction, ForcedResponse),
      HasAbilities(..),
      ability,
      label,
      windowAbility )
import Marvel.Attack
import Marvel.Cost
import Marvel.Count ()
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Difficulty ()
import Marvel.Game.Source
import Marvel.Identity.Types
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
    ( Message(AllyMessage, AttachmentMessage, CardSpecific, RanAbility,
              IdentityMessage),
      RunMessage(..),
      AttachmentMessage(RevealAttachment),
      IdentityMessage(IdentityWasAttacked),
      AllyMessage(AllyWasAttacked),
      runQueueT,
      attachTo,
      placeCounters,
      removeCounters,
      removeFromPlay )
import Marvel.Modifier
    ( HasModifiersFor(getModifiersFor),
      Modifier(KeywordModifier, DamageTakenFromPrintedResource) )
import Marvel.NextEvolution.Juggernaut
import Marvel.Payment ()
import Marvel.Projection
import Marvel.Query ()
import Marvel.Question
import Marvel.Resource.Types
import Marvel.Window qualified as W
import Marvel.Card
import Marvel.Entity
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types


import Marvel.Ref
import Marvel.GameValue ()
import Marvel.Stats ()
import Marvel.Hp ()


newtype NeJuggernautAttachment = NeJuggernautAttachment (Attrs Attachment `With` Natural)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

ne40122a :: AttachmentCard NeJuggernautAttachment
ne40122a = attachment (NeJuggernautAttachment . (`With` (0 :: Natural))) Cards.ne40122a

ne40122b :: AttachmentCard NeJuggernautAttachment
ne40122b = attachment (NeJuggernautAttachment . (`With` (0 :: Natural))) Cards.ne40122b

ne40123 :: AttachmentCard NeJuggernautAttachment
ne40123 = attachment (NeJuggernautAttachment . (`With` (0 :: Natural))) Cards.ne40123


instance IsAttachment NeJuggernautAttachment where
  toAttachmentAttrs (NeJuggernautAttachment (attrs `With` _)) = attrs
instance IsRef NeJuggernautAttachment where
  toRef = toRef . toAttachmentAttrs

instance HasAbilities NeJuggernautAttachment where
  getAbilities a@(NeJuggernautAttachment (attrs `With` dealt)) = case toCardCode attrs of
    "40122a" -> [label ("$label.neHelmet" <> tshow resource) $ ability a idx HeroAction NoCriteria
      (MultiResourceCost $ replicate 3 $ Just resource) (runAbility a idx)
      | (idx,resource) <- [(1,Physical),(2,Mental),(3,Energy),(6,Wild)]]
    "40122b" -> [windowAbility a 4 (W.VillainSchemed W.After $ VillainWithId vid) ForcedResponse NoCost (runAbility a 4) | EnemyVillainId vid <- maybeToList $ attachmentEnemy attrs]
    "40123" -> [windowAbility a 5 (W.EnemyAttacked W.After (EnemyWithId enemy) AnyIdentity) HeroResponse
      (MultiResourceCost $ replicate (fromIntegral dealt) Nothing) (runAbility a 5) | enemy <- maybeToList $ attachmentEnemy attrs]
    _ -> []

instance HasModifiersFor NeJuggernautAttachment where
  getModifiersFor _ target (NeJuggernautAttachment (attrs `With` _)) | Just target == (enemyRef <$> attachmentEnemy attrs) = case toCardCode attrs of
    "40122a" -> pure [KeywordModifier Stalwart, KeywordModifier Overkill]
    "40122b" -> pure [DamageTakenFromPrintedResource Mental 1]
    "40123" -> do
      momentum <- juggernautMomentum
      pure [KeywordModifier $ Retaliate momentum]
    _ -> pure []
  getModifiersFor _ _ _ = pure []

instance RunMessage NeJuggernautAttachment where
  runMessage msg a@(NeJuggernautAttachment (attrs `With` dealt)) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment _) | aid == attachmentId attrs -> do
      juggernaut >>= traverse_ \vid -> do
        attachTo aid vid
        when (toCardCode attrs == "40123") $ placeCounters vid 1
      pure a
    CardSpecific (isTarget attrs -> True) "flipHelmet" _ -> do
      let next = if toCardCode attrs == "40122a" then Cards.ne40122b else Cards.ne40122a
      pure $ NeJuggernautAttachment (attrs {attachmentCardDef = next} `With` dealt)
    RanAbility _ (isTarget attrs -> True) idx _ _ | idx `elem` [1,2,3,6] -> do
      juggernaut >>= traverse_ \vid -> getCountersOn vid >>= removeCounters vid
      flipJuggernautHelmet
      pure a
    RanAbility _ (isTarget attrs -> True) 4 _ _ -> do
      juggernaut >>= traverse_ (\vid -> placeCounters vid 1)
      flipJuggernautHelmet
      pure a
    RanAbility _ (isTarget attrs -> True) 5 _ _ -> a <$ removeFromPlay attrs
    IdentityMessage you (IdentityWasAttacked attack) | Just (attackEnemy attack) == attachmentEnemy attrs -> do
      defense <- project PlayerIdentityDamageReduction you
      attrs' <- runMessage msg attrs
      pure $ NeJuggernautAttachment (attrs' `With` subtractNatural defense (attackValue attack))
    AllyMessage _ (AllyWasAttacked attack) | Just (attackEnemy attack) == attachmentEnemy attrs -> do
      attrs' <- runMessage msg attrs
      pure $ NeJuggernautAttachment (attrs' `With` attackValue attack)
    _ -> NeJuggernautAttachment . (`With` dealt) <$> runMessage msg attrs
