module Marvel.Upgrade.Upgrades.WonderHerculesPack where

import Marvel.Prelude
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Discard
import Marvel.Effect.Types
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hercules.Helpers
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Payment
import Marvel.Projection
import Marvel.Question
import Marvel.Query
import Marvel.Resource.Types
import Marvel.Window qualified as W
import Marvel.Trait qualified as T
import Marvel.WonderMan.Helpers
import Marvel.Ability
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Types
import Marvel.WonderMan.Cards qualified as WM
import Marvel.Hercules.Cards qualified as HC

wm58002 :: UpgradeCard WmHcUpgrade
wm58002 = upgrade (WmHcUpgrade . (`With` WmHcUpgradeMeta 0 Nothing)) WM.wm58002

wm58008 :: UpgradeCard WmHcUpgrade
wm58008 = upgrade (WmHcUpgrade . (`With` WmHcUpgradeMeta 0 Nothing)) WM.wm58008

wm58009 :: UpgradeCard WmHcUpgrade
wm58009 = upgrade (WmHcUpgrade . (`With` WmHcUpgradeMeta 0 Nothing)) WM.wm58009

wm58010 :: UpgradeCard WmHcUpgrade
wm58010 = upgrade (WmHcUpgrade . (`With` WmHcUpgradeMeta 0 Nothing)) WM.wm58010

wm58011 :: UpgradeCard WmHcUpgrade
wm58011 = upgrade (WmHcUpgrade . (`With` WmHcUpgradeMeta 0 Nothing)) WM.wm58011

wm58021 :: UpgradeCard WmHcUpgrade
wm58021 = upgrade (WmHcUpgrade . (`With` WmHcUpgradeMeta 0 Nothing)) WM.wm58021

wm58030 :: UpgradeCard WmHcUpgrade
wm58030 = upgrade (WmHcUpgrade . (`With` WmHcUpgradeMeta 0 Nothing)) WM.wm58030

wm58032 :: UpgradeCard WmHcUpgrade
wm58032 = upgrade (WmHcUpgrade . (`With` WmHcUpgradeMeta 0 Nothing)) WM.wm58032

hc59005 :: UpgradeCard WmHcUpgrade
hc59005 = upgrade (WmHcUpgrade . (`With` WmHcUpgradeMeta 0 Nothing)) HC.hc59005

hc59006 :: UpgradeCard WmHcUpgrade
hc59006 = upgrade (WmHcUpgrade . (`With` WmHcUpgradeMeta 0 Nothing)) HC.hc59006

hc59007 :: UpgradeCard WmHcUpgrade
hc59007 = upgrade (WmHcUpgrade . (`With` WmHcUpgradeMeta 0 Nothing)) HC.hc59007

hc59013 :: UpgradeCard WmHcUpgrade
hc59013 = upgrade (WmHcUpgrade . (`With` WmHcUpgradeMeta 0 Nothing)) HC.hc59013

hc59014 :: UpgradeCard WmHcUpgrade
hc59014 = upgrade (WmHcUpgrade . (`With` WmHcUpgradeMeta 0 Nothing)) HC.hc59014

hc59015 :: UpgradeCard WmHcUpgrade
hc59015 = upgrade (WmHcUpgrade . (`With` WmHcUpgradeMeta 0 Nothing)) HC.hc59015

hc59016 :: UpgradeCard WmHcUpgrade
hc59016 = upgrade (WmHcUpgrade . (`With` WmHcUpgradeMeta 0 Nothing)) HC.hc59016

hc59017 :: UpgradeCard WmHcUpgrade
hc59017 = upgrade (WmHcUpgrade . (`With` WmHcUpgradeMeta 0 Nothing)) HC.hc59017

hc59024 :: UpgradeCard WmHcUpgrade
hc59024 = upgrade (WmHcUpgrade . (`With` WmHcUpgradeMeta 0 Nothing)) HC.hc59024

data WmHcUpgradeMeta = WmHcUpgradeMeta {gauntletRetaliate :: Natural, hollywoodUsedFor :: Maybe CardId}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype WmHcUpgrade = WmHcUpgrade (Attrs Upgrade `With` WmHcUpgradeMeta)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance IsUpgrade WmHcUpgrade where
  toUpgradeAttrs (WmHcUpgrade (attrs `With` _)) = attrs
  upgradeTargets (WmHcUpgrade (attrs `With` _)) = case toCardCode attrs of
    "58030" -> Just . map enemyRef <$> selectList (MinionEnemy <> NotEnemy (EnemyWithTrait T.Elite) <> NotEnemy (EnemyWithUpgrade $ UpgradeIs WM.wm58030))
    "58032" -> do
      targets <- getCardsInPlayTargets
      valid <- filterM (\target -> do
        card <- getsGame (`cardInPlay` target)
        modifiers <- getModifiers target
        pure $ (case card of Just (EncounterCard _) -> True; _ -> False) && AllowsCostContributions `notElem` modifiers) targets
      pure $ Just valid
    "59024" -> Just . map AllyRef <$> selectList (AnyAlly <> NotAlly (AllyWithUpgrade $ UpgradeIs HC.hc59024))
    _ -> controlTargets attrs
instance IsRef WmHcUpgrade where
  toRef = toRef . toUpgradeAttrs

instance HasModifiersFor WmHcUpgrade where
  getModifiersFor _ target (WmHcUpgrade (attrs `With` meta)) = do
    let you = upgradeController attrs
    if toCardCode attrs == "58032" && upgradeAttachedCard attrs == Just target then pure [AllowsCostContributions]
    else if target /= IdentityRef you then pure [] else case toCardCode attrs of
      "58021" -> pure [HitPointModifier 3, ThwartModifier 1]
      "59005" -> pure [HitPointModifier 2, KeywordModifier K.Steady]
      "59006" -> pure [HitPointModifier 1, KeywordModifier $ K.Retaliate 1]
      "59007" -> pure [HitPointModifier 1, BasicAttackKeyword K.Piercing]
      "59016" -> pure [AttackModifier 1, ThwartModifier 1, DefenseModifier 1]
      "59013" -> pure [KeywordModifier $ K.Retaliate $ gauntletRetaliate meta | gauntletRetaliate meta > 0]
      _ -> pure []

instance HasAbilities WmHcUpgrade where
  getAbilities u@(WmHcUpgrade (attrs `With` meta)) = case toCardCode attrs of
    "58002" -> [limitedWindowAbility u 1 (W.PlaysEvent W.After You $ CardWithResource Energy) Response (OwnsThis <> PlayedEventInDiscard <> NotCriteria (UpgradeHeldCardsCount (UpgradeIs WM.wm58002) (AtLeast $ Static 3))) NoCost $ runAbility u 1]
    "58008" -> [ability u 1 HeroResource (OwnsThis <> PayingFor (CardWithType EventType)) ExhaustCost $ Pay (ResourcePayment Energy)
               ,(ability u 2 HeroResource (OwnsThis <> UpgradeHeldCardsCount (UpgradeIs WM.wm58002) (GreaterThan $ Static 0)) ExhaustCost $ runAbility u 2) {abilityChoices = [runAbility u 2, Pay $ ResourcePayment Energy]}]
    "58009" -> [(ability u 1 Resource (OwnsThis <> PayingToOverpay <> maybe NoCriteria PayingForCardOtherThan (hollywoodUsedFor meta)) NoCost $ runAbility u 1) {abilityChoices = [runAbility u 1, Pay $ OverpaymentOnlyPayment Energy]}]
    "58010" -> [limitedWindowAbility u 1 (W.IdentityChangedToForm W.After You) Response (OwnsThis <> AnyCriteria [UpgradeHeldCardsCount (UpgradeIs WM.wm58002) (GreaterThan $ Static 0), NotCriteria (UpgradeHeldCardsCount (UpgradeIs WM.wm58002) (AtLeast $ Static 3)) <> ExtendedCardExists (InDiscardOf You $ BasicCardMatches $ CardWithResource Energy <> CardMatchesAny [CardWithType EventType, CardWithType ResourceType])]) NoCost $ runAbility u 1]
    "58011" -> [limitedWindowAbility u 1 (W.IdentityWouldBeDefeated You) ForcedInterrupt OwnsThis NoCost $ runAbility u 1]
    code | code `elem` ["59005", "59006", "59007"] -> [limitedWindowAbility u 1 (W.PlayerCardEntersPlay W.After You $ CardIs $ getCardDef attrs) Response OwnsThis NoCost $ runAbility u 1]
    "58030" -> [limitedWindowAbility u 1 (W.WindowOneOf [W.IdentityAttacked W.When AnyIdentity AnyEnemy, W.AllyAttacked W.When AnyAlly AnyEnemy]) Interrupt OwnsThis NoCost $ runAbility u 1]
    "59013" -> [limitedWindowAbility u 1 (W.HeroDefended W.When You AnyEnemy) HeroInterrupt (OwnsThis <> HasEffectiveHerculesGift) ExhaustCost $ runAbility u 1]
    "59014" -> [limitedWindowAbility u 1 (W.MakesBasicAttack W.When You) HeroInterrupt OwnsThis ExhaustCost $ runAbility u 1]
    "59015" -> [(limitedWindowAbility u 1 (W.EnemyAttacksPlayerOrAlly W.When VillainEnemy AnyIdentity) HeroInterrupt OwnsThis ExhaustCost $ runAbility u 1) {abilitySubType = Just Defense}]
    "59017" -> [limitedWindowAbility u 1 (W.DealsExcessDamage You) HeroResponse (OwnsThis <> SelfMatches IdentityWithAnyDamage) ExhaustCost $ runAbility u 1]
    "59024" -> [limitedWindowAbility u 1 (W.AllyDefeated W.When $ maybe AnyAlly AllyWithId $ upgradeAttachedAlly attrs) Interrupt OwnsThis NoCost $ runAbility u 1]
    _ -> []

instance RunMessage WmHcUpgrade where
  runMessage msg u@(WmHcUpgrade (attrs `With` meta)) = runQueueT $ case msg of
    UpgradeMessage uid (PlayedUpgrade target) | uid == upgradeId attrs && toCardCode attrs == "58032" -> do
      attrs' <- runMessage msg attrs
      pure $ WmHcUpgrade $ attrs' {upgradeAttachedCard = target} `With` meta
    RanAbility _ (isTarget attrs -> True) 1 _ _ | toCardCode attrs == "58009" -> do
      card <- getActiveCost >>= maybe (pure Nothing) getActiveCostCard
      pure $ WmHcUpgrade $ attrs `With` meta {hollywoodUsedFor = pcCardId <$> card}
    IdentityMessage _ (PlayedCard card _) | Just (pcCardId card) == hollywoodUsedFor meta -> pure $ WmHcUpgrade $ attrs `With` meta {hollywoodUsedFor = Nothing}
    AbortPayment acid -> do
      card <- getActiveCostCard acid
      if (pcCardId <$> card) == hollywoodUsedFor meta then pure $ WmHcUpgrade $ attrs `With` meta {hollywoodUsedFor = Nothing} else pure u
    RanAbility you (isTarget attrs -> True) 1 _ _ | toCardCode attrs == "59013" -> do
      n <- giftCount you
      pure $ WmHcUpgrade $ attrs `With` meta {gauntletRetaliate = n}
    IdentityMessage who (AttackDefended _ _ _) | who == upgradeController attrs && toCardCode attrs == "59013" -> pure $ WmHcUpgrade $ attrs `With` meta {gauntletRetaliate = 0}
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      case toCardCode attrs of
        "58002" -> do
          cards <- projectMap PlayerIdentityDiscard unDiscard you
          let ids = [CardId $ unEventId eid | W.EventPlayed who eid _ <- windows, who == you]
              eligible = [card | card <- cards, pcCardId card `elem` ids]
          host <- ionicCards you
          when (length host < 3) $ chooseOne you [targeting card (tuckIonicCard you card >> heal you 1) | card <- eligible]
        "58009" -> pure ()
        "58030" -> for_ (upgradeAttachedEnemy attrs) $ \enemy -> dealDamage enemy attrs $ toDamage 1 FromAbility
        "58010" -> do
          tucked <- ionicCards you
          discard <- filter (\card -> hasPrintedEnergy card && cardMatch (CardMatchesAny [CardWithType EventType, CardWithType ResourceType]) card) <$> projectMap PlayerIdentityDiscard unDiscard you
          chooseOne you $ [targeting card (tuckIonicCard you card) | length tucked < 3, card <- discard]
            <> [targeting card (ionicPhysiologyFor you >>= traverse_ (\host -> releaseHeldCard host you card)) | card <- tucked]
        "58011" -> do
          cancelMatchingMessage $ \case IdentityMessage who IdentityDefeated -> who == you; _ -> False
          setHitPoints you 4
          changeToAlterEgo you
          cardSpecific attrs "whatAreYouTuck" you
        code | code `elem` ["59005", "59006", "59007"] -> drawCards you 4
        "59014" -> do
          n <- giftCount you
          addToIdentityBasicPower you n
          createEffect HC.hc59014 attrs (IdentityEntity $ IdentityWithId you)
        "59015" -> for_ ([enemy | W.EnemyAttack enemy _who <- windows] <> [enemy | W.EnemyAttackAlly enemy _ally _who <- windows]) $ \enemy -> preventEnemyAttackDamage enemy 1
        "59017" -> for_ ([(source, n) | W.DefeatedMinion _ _ source n <- windows] <> [(source, n) | W.DefeatedVillain _ _ source n <- windows]) $ \(source, n) -> do
          owner <- getHeroSourceController source
          when (owner == Just you) $ heal you n
        "59024" -> drawCards you 2
        _ -> pure ()
      pure u
    RanAbility you (isTarget attrs -> True) 2 _ _ | toCardCode attrs == "58008" -> do
      cards <- ionicCards you
      chooseOne you [targeting card $ ionicPhysiologyFor you >>= traverse_ (\host -> discardHeldPlayerCard you host card) | card <- cards]
      pure u
    CardSpecific (isTarget attrs -> True) "whatAreYouTuck" (fromJSON -> Success (you :: IdentityId)) -> do
      cards <- filter (\card -> hasPrintedEnergy card && cardMatch (CardWithType EventType) card) <$> projectMap PlayerIdentityDiscard unDiscard you
      tucked <- ionicCards you
      if length tucked < 3 && notNull cards then chooseOne you $
        [labeled "$label.done" (removeFromGame attrs)] <> [targeting card (tuckIonicCard you card >> cardSpecific attrs "whatAreYouTuck" you) | card <- cards]
      else removeFromGame attrs
      pure u
    _ -> (\attrs' -> WmHcUpgrade (attrs' `With` meta)) <$> runMessage msg attrs

newtype HcGoldenMaceEffect = HcGoldenMaceEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

hcGoldenMaceEffect :: CardEffect HcGoldenMaceEffect
hcGoldenMaceEffect = effectWith HcGoldenMaceEffect HC.hc59014 ((modifiersL .~ [BasicAttackKeyword K.Overkill]) . (endsL ?~ DisableAtEndOfPhase))

instance RunMessage HcGoldenMaceEffect where
  runMessage msg e@(HcGoldenMaceEffect attrs) = runQueueT $ case msg of
    CheckWindows windows | any (\case W.Window W.After (W.MadeBasicAttack you) -> effectMatcher attrs == IdentityEntity (IdentityWithId you); _ -> False) windows -> e <$ disableEffect (effectId attrs)
    _ -> HcGoldenMaceEffect <$> runMessage msg attrs
