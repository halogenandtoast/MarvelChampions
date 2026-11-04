module Marvel.Shadowland.Spells where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Types
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Shadowland.Cards qualified as Cards
import Marvel.Shadowland.Corruption
import Marvel.Window qualified as W

newtype ShadowSpell = ShadowSpell (Attrs Attachment)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
  deriving anyclass IsAttachment
shadowSpells :: [AttachmentCard ShadowSpell]
shadowSpells = map (attachment ShadowSpell) [Cards.street65062,Cards.street65063,Cards.street65064,Cards.street65065]
instance HasModifiersFor ShadowSpell where
  getModifiersFor _ target (ShadowSpell a) = pure [KeywordModifier $ Retaliate 1 | toCardCode a == "65063",Just target == (enemyRef <$> attachmentEnemy a)]
instance HasAbilities ShadowSpell where
  getAbilities (ShadowSpell a) = case toCardCode a of
    "65062" -> [windowAbility a 1 (W.VillainWouldTakeDamageAtLeastFrom AnyVillain AnyAttack 4) Interrupt NoCost (runAbility a 1)]
    "65063" -> [ability a 2 HeroAction corruptionExists NoCost (runAbility a 2),ability a 3 AlterEgoAction NoCriteria (MultiResourceCost $ replicate 3 $ Just Mental) (runAbility a 3)]
    "65064" -> [windowAbility a 4 (W.MakesBasicAttack W.After $ IdentityWithId you) ForcedResponse NoCost (runAbility a 4) | Just you <- [attachmentIdentity a]]
    "65065" -> [windowAbility a 4 (W.MakesBasicThwart W.After (IdentityWithId you)) ForcedResponse NoCost (runAbility a 4) | Just you <- [attachmentIdentity a]]
    _ -> []
   where corruptionExists = UpgradeExists (UpgradeControlledBy You <> UpgradeNamed "Creeping Corruption")
instance RunMessage ShadowSpell where
  runMessage msg e@(ShadowSpell a) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId a -> do
      if toCardCode a `elem` ["65062","65063"] then selectOne ActiveVillain >>= traverse_ (attachTo aid)
        else chooseOne you [labeled "$label.slAcceptSpell" (attachToIdentity aid you),labeled "$label.slResistSpell" (cardSpecific a "rejectSpell" you)]
      pure e
    CardSpecific (isTarget a -> True) "rejectSpell" (fromJSON -> Success you) -> do
      damageIdentity you a (toDamage 4 FromAbility)
      if toCardCode a == "65064" then stunIdentity you else confuseIdentity you
      purify you 1
      removeFromPlay a
      pure e
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      for_ [(vid,source,damage) | W.VillainDamageFrom vid source damage <- windows] $ \(vid,source,damage) -> do
        who <- fromMaybe you <$> getSourceController source
        corrupt who 1
        unless (damageUnpreventable damage) $ cancelEnemyDamage (EnemyVillainId vid)
        removeFromPlay a
      pure e
    RanAbility you (isTarget a -> True) 2 _ _ -> corrupt you 2 >> removeFromPlay a >> pure e
    RanAbility you (isTarget a -> True) 3 _ _ -> removeFromPlay a >> purify you 1 >> pure e
    RanAbility _ (isTarget a -> True) 4 _ _ -> for_ (attachmentIdentity a) (`corrupt` 1) >> pure e
    _ -> ShadowSpell <$> runMessage msg a
