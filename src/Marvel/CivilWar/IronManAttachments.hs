module Marvel.CivilWar.IronManAttachments where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.Helpers
import Marvel.Cost
import Marvel.Damage
import Marvel.EncounterSet qualified as E
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Resource.Types
import Marvel.Trait (Trait (Aerial))
import Marvel.Window qualified as W

newtype IronManAttachment = IronManAttachment (Attrs Attachment)
  deriving anyclass IsAttachment
  deriving newtype (Show,Eq,ToJSON,FromJSON,HasCardCode,IsRef)

ironManDefinitions :: [CardDef]
ironManDefinitions = [C.cw56065,C.cw56066,C.cw56067,C.cw56068,C.cw56069]

ironManAttachments :: [AttachmentCard IronManAttachment]
ironManAttachments = map (attachment IronManAttachment) ironManDefinitions

instance HasModifiersFor IronManAttachment where
  getModifiersFor _ target (IronManAttachment a)
    | Just target == (enemyRef <$> attachmentEnemy a) = pure $ case toCardCode a of
        "56065" -> [AttackModifier 1,KeywordModifier Ranged]
        "56066" -> [AttackModifier 1,TraitModifier Aerial]
        "56067" -> [KeywordModifier Stalwart]
        "56068" -> [KeywordModifier $ Retaliate 1]
        _ -> []
  getModifiersFor _ _ _ = pure []

instance HasAbilities IronManAttachment where
  getAbilities (IronManAttachment a) = case attachmentEnemy a of
    Just (EnemyVillainId vid) | toCardCode a == "56069" ->
      [windowAbility a 2 (W.VillainWouldTakeDamageFrom (VillainWithId vid) AnyDamage) ForcedInterrupt NoCost (runAbility a 2)]
    Just enemy ->
      [windowAbility a 1 (W.MakesBasicAttackAgainst W.After You $ EnemyWithId enemy) HeroResponse cost (runAbility a 1)]
      where
        cost = MultiResourceCost $ map Just $ if toCardCode a `elem` ["56065","56066"] then [Energy,Physical] else [Mental,Physical]
    _ -> []

instance RunMessage IronManAttachment where
  runMessage msg e@(IronManAttachment a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      asYou you (leaderOfFamily E.CwIronMan) >>= \case
        Nothing -> removeFromPlay a
        Just vid -> do
          attachTo aid vid
          when (toCardCode a `elem` ["56067","56068"]) $ giveStatus vid ToughStatus
      pure e
    AttachmentMessage aid (AttachmentDamaged amount) | aid == attachmentId a, toCardCode a == "56069" -> do
      let total = attachmentDamage a + amount
      when (total >= 5) $ removeFromPlay a
      pure $ IronManAttachment a {attachmentDamage = total}
    RanAbility _ (isTarget a -> True) 1 _ _ -> e <$ removeFromPlay a
    RanAbility _ (isTarget a -> True) 2 windows _ -> do
      for_ [(vid,source,damage) | W.VillainDamageFrom vid source damage <- windows] $ \(vid,source,damage) -> do
        incoming <- incomingEnemyDamage (EnemyVillainId vid) source damage
        when (damageAmount incoming > 0) $
          insteadOf (\case VillainMessage target (VillainDamaged _ _) -> target == vid; _ -> False)
            (damageAttachment (attachmentId a) $ damageAmount incoming)
      pure e
    Boost (RevealedAsBoost target enemy) | isTarget a target, toCardCode a `elem` ["56065","56066","56069"] -> do
      revealBoost a =<< getBoostedPlayer enemy
      pure e
    _ -> IronManAttachment <$> runMessage msg a
