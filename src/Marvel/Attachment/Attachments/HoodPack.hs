module Marvel.Attachment.Attachments.HoodPack where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Attachment.Cards qualified as Cards
import Marvel.Attachment.Types
import Marvel.Boost (boostCount)
import Marvel.Card
import Marvel.Cost
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Hood.Helpers
import Marvel.Id ()
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Deck
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Query
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

newtype HoodAttachment = HoodAttachment (Attrs Attachment)
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

hood24019, hood24020, hood24021, hood24022, hood24037, hood24038, hood24039, hood24040 :: AttachmentCard HoodAttachment
hood24019 = attachment HoodAttachment Cards.hood24019
hood24020 = attachment HoodAttachment Cards.hood24020
hood24021 = attachment HoodAttachment Cards.hood24021
hood24022 = attachment HoodAttachment Cards.hood24022
hood24037 = attachment HoodAttachment Cards.hood24037
hood24038 = attachment HoodAttachment Cards.hood24038
hood24039 = attachment HoodAttachment Cards.hood24039
hood24040 = attachment HoodAttachment Cards.hood24040

instance HasModifiersFor HoodAttachment where
  getModifiersFor _ target (HoodAttachment a) = pure $ case attachmentEnemy a of
    Just enemy | target == enemyRef enemy -> case toCardCode a of
      "24037" -> [AttackModifier 3]
      "24038" -> [HitPointModifier 4, KeywordModifier (Retaliate 2)]
      "24040" -> [HitPointModifier 3, AttackModifier 1, KeywordModifier Overkill]
      _ -> []
    _ -> []

instance HasAbilities HoodAttachment where
  getAbilities (HoodAttachment a) = case attachmentEnemy a of
    Nothing -> []
    Just enemy ->
      [windowAbility a 1 (W.EnemyActivates W.After (EnemyWithId enemy) AnyIdentity) ForcedResponse NoCost (runAbility a 1) | toCardCode a `elem` ["24019","24020","24021","24022"]]
      <> [windowAbility a 2 (W.EnemyWouldBeDealtDamageBy (EnemyWithId enemy) AnyAttack) ForcedInterrupt NoCost (runAbility a 2) | toCardCode a == "24039"]
      <> [windowAbility a 3 (W.EnemyAttacksPlayerOrAlly W.When (EnemyWithId enemy) AnyIdentity) ForcedInterrupt NoCost (runAbility a 3) | toCardCode a == "24037"]

instance RunMessage HoodAttachment where
  runMessage msg e@(HoodAttachment attrs) = runQueueT $ case msg of
    AttachmentMessage aid (RevealAttachment you) | aid == attachmentId attrs -> do
      if toCardCode attrs `elem` ["24019","24020","24021","24022"] then do
        mystics <- selectList (MinionWithTrait Trait.Mystic)
        if null mystics then attachToTheVillain attrs
          else chooseOrRunOne you [targeting mid (attachTo aid mid) | mid <- mystics]
      else attachArmory you True
      pure e
    CardSpecific (isTarget attrs -> True) "attachArmory" (fromJSON -> Success you) -> do
      attachArmory you False
      pure e
    RanAbility resolver (isTarget attrs -> True) 1 windows _ -> do
      let you = fromMaybe resolver $ headMay [who | W.EnemyActivation _ who <- windows]
      removeFromPlay attrs
      case toCardCode attrs of
        "24019" -> discardFor you attrs RandomFromHand 1 1
        "24020" -> dealIndirectDamage you attrs 3
        "24021" -> giveStatus you StunnedStatus
        "24022" -> void $ discardControlledOfTypes you [AllyType, SupportType, UpgradeType]
        _ -> pure ()
      dealEncounterCard you
      pure e
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> do
      discardTopOfEncounterDeckFor 1 attrs
      pure e
    WithDiscarded (isTarget attrs -> True) FromEncounterDeck cards -> do
      for_ (attachmentEnemy attrs) $ \enemy -> preventEnemyDamage enemy (sum $ map (boostCount . cdBoostIcons . getCardDef) cards)
      pure e
    RanAbility _ (isTarget attrs -> True) 3 _ _ -> do
      traverse_ makeAttackIndirect (attachmentEnemy attrs)
      pure e
    _ -> HoodAttachment <$> runMessage msg attrs
   where
    attachArmory you maySearch = do
      minions <- selectList (MinionWithMostRemainingHitPointsAmong AnyMinion)
      if notNull minions then chooseOrRunOne you [targeting mid (attachTo (attachmentId attrs) mid) | mid <- minions]
      else if maySearch && toCardCode attrs `elem` ["24037","24038"] then do
        searchAndPutIntoPlayEncounterCard you (CardWithType MinionType)
        cardSpecific attrs "attachArmory" you
      else do
        removeFromPlay attrs
        when (toCardCode attrs `elem` ["24039","24040"]) $ surge you
