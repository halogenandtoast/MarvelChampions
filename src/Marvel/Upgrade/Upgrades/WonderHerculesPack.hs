module Marvel.Upgrade.Upgrades.WonderHerculesPack where

import Marvel.Prelude
import Marvel.Name qualified as Name
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
  upgradeTargets (WmHcUpgrade (attrs `With` _)) = case Name.title (cdName (upgradeCardDef attrs)) of
    "Caught in the Crossfire" -> Just . map enemyRef <$> selectList (MinionEnemy <> NotEnemy (EnemyWithTrait T.Elite) <> NotEnemy (EnemyWithUpgrade $ UpgradeIs WM.wm58030))
    "Coordinated Effort" -> do
      targets <- getCardsInPlayTargets
      valid <- filterM (\target -> do
        card <- getsGame (`cardInPlay` target)
        modifiers <- getModifiers target
        pure $ (case card of Just (EncounterCard _) -> True; _ -> False) && AllowsCostContributions `notElem` modifiers) targets
      pure $ Just valid
    "\"Avenge Me!\"" -> Just . map AllyRef <$> selectList (AnyAlly <> NotAlly (AllyWithUpgrade $ UpgradeIs HC.hc59024))
    _ -> controlTargets attrs
instance IsRef WmHcUpgrade where
  toRef = toRef . toUpgradeAttrs

instance HasModifiersFor WmHcUpgrade where
  getModifiersFor _ target (WmHcUpgrade (attrs `With` meta)) = do
    let you = upgradeController attrs
    if Name.title (cdName (upgradeCardDef attrs)) == "Coordinated Effort" && upgradeAttachedCard attrs == Just target then pure [AllowsCostContributions]
    else if target /= IdentityRef you then pure [] else case Name.title (cdName (upgradeCardDef attrs)) of
      "Heroic Conditioning" -> pure [HitPointModifier 3, ThwartModifier 1]
      "Nemean Lion Skin" -> pure [HitPointModifier 2, KeywordModifier K.Steady]
      "Shield of Perseus" -> pure [HitPointModifier 1, KeywordModifier $ K.Retaliate 1]
      "Sword of Peleus" -> pure [HitPointModifier 1, BasicAttackKeyword K.Piercing]
      "Lion of Olympus" -> pure [AttackModifier 1, ThwartModifier 1, DefenseModifier 1]
      "Gauntlets of Hercules" -> pure [KeywordModifier $ K.Retaliate $ gauntletRetaliate meta | gauntletRetaliate meta > 0]
      _ -> pure []

instance HasAbilities WmHcUpgrade where
  getAbilities u@(WmHcUpgrade (attrs `With` meta)) = case Name.title (cdName (upgradeCardDef attrs)) of
    "Ionic Physiology" -> [limitedWindowAbility u 1 (W.PlaysEvent W.After You $ CardWithResource Energy) Response (OwnsThis <> PlayedEventInDiscard <> NotCriteria (UpgradeHeldCardsCount (UpgradeIs WM.wm58002) (AtLeast $ Static 3))) NoCost $ runAbility u 1]
    "Jet Belt" -> [ability u 1 HeroResource (OwnsThis <> PayingFor (CardWithType EventType)) ExhaustCost $ Pay (ResourcePayment Energy)
               ,(ability u 2 HeroResource (OwnsThis <> UpgradeHeldCardsCount (UpgradeIs WM.wm58002) (GreaterThan $ Static 0)) ExhaustCost $ runAbility u 2) {abilityChoices = [runAbility u 2, Pay $ ResourcePayment Energy]}]
    "Mr. Hollywood" -> [(ability u 1 Resource (OwnsThis <> PayingToOverpay <> maybe NoCriteria PayingForCardOtherThan (hollywoodUsedFor meta)) NoCost $ runAbility u 1) {abilityChoices = [runAbility u 1, Pay $ OverpaymentOnlyPayment Energy]}]
    "Signature Sunglasses" -> [limitedWindowAbility u 1 (W.IdentityChangedToForm W.After You) Response (OwnsThis <> AnyCriteria [UpgradeHeldCardsCount (UpgradeIs WM.wm58002) (GreaterThan $ Static 0), NotCriteria (UpgradeHeldCardsCount (UpgradeIs WM.wm58002) (AtLeast $ Static 3)) <> ExtendedCardExists (InDiscardOf You $ BasicCardMatches $ CardWithResource Energy <> CardMatchesAny [CardWithType EventType, CardWithType ResourceType])]) NoCost $ runAbility u 1]
    "\"What Are You?\"" -> [limitedWindowAbility u 1 (W.IdentityWouldBeDefeated You) ForcedInterrupt OwnsThis NoCost $ runAbility u 1]
    code | code `elem` ["Nemean Lion Skin", "Shield of Perseus", "Sword of Peleus"] -> [limitedWindowAbility u 1 (W.PlayerCardEntersPlay W.After You $ CardIs $ getCardDef attrs) Response OwnsThis NoCost $ runAbility u 1]
    "Caught in the Crossfire" -> [limitedWindowAbility u 1 (W.WindowOneOf [W.IdentityAttacked W.When AnyIdentity AnyEnemy, W.AllyAttacked W.When AnyAlly AnyEnemy]) Interrupt OwnsThis NoCost $ runAbility u 1]
    "Gauntlets of Hercules" -> [limitedWindowAbility u 1 (W.HeroDefended W.When You AnyEnemy) HeroInterrupt (OwnsThis <> HasEffectiveHerculesGift) ExhaustCost $ runAbility u 1]
    "Golden Mace" -> [limitedWindowAbility u 1 (W.MakesBasicAttack W.When You) HeroInterrupt OwnsThis ExhaustCost $ runAbility u 1]
    "Herc's Helm" -> [(limitedWindowAbility u 1 (W.EnemyAttacksPlayerOrAlly W.When VillainEnemy AnyIdentity) HeroInterrupt OwnsThis ExhaustCost $ runAbility u 1) {abilitySubType = Just Defense}]
    "Prince of Power" -> [limitedWindowAbility u 1 (W.DealsExcessDamage You) HeroResponse (OwnsThis <> SelfMatches IdentityWithAnyDamage) ExhaustCost $ runAbility u 1]
    "\"Avenge Me!\"" -> [limitedWindowAbility u 1 (W.AllyDefeated W.When $ maybe AnyAlly AllyWithId $ upgradeAttachedAlly attrs) Interrupt OwnsThis NoCost $ runAbility u 1]
    _ -> []

instance RunMessage WmHcUpgrade where
  runMessage msg u@(WmHcUpgrade (attrs `With` meta)) = runQueueT $ case msg of
    UpgradeMessage uid (PlayedUpgrade target) | uid == upgradeId attrs && Name.title (cdName (upgradeCardDef attrs)) == "Coordinated Effort" -> do
      attrs' <- runMessage msg attrs
      pure $ WmHcUpgrade $ attrs' {upgradeAttachedCard = target} `With` meta
    RanAbility _ (isTarget attrs -> True) 1 _ _ | Name.title (cdName (upgradeCardDef attrs)) == "Mr. Hollywood" -> do
      card <- getActiveCost >>= maybe (pure Nothing) getActiveCostCard
      pure $ WmHcUpgrade $ attrs `With` meta {hollywoodUsedFor = pcCardId <$> card}
    IdentityMessage _ (PlayedCard card _) | Just (pcCardId card) == hollywoodUsedFor meta -> pure $ WmHcUpgrade $ attrs `With` meta {hollywoodUsedFor = Nothing}
    AbortPayment acid -> do
      card <- getActiveCostCard acid
      if (pcCardId <$> card) == hollywoodUsedFor meta then pure $ WmHcUpgrade $ attrs `With` meta {hollywoodUsedFor = Nothing} else pure u
    RanAbility you (isTarget attrs -> True) 1 windows _ | Name.title (cdName (upgradeCardDef attrs)) == "Gauntlets of Hercules" -> do
      n <- giftCount you
      for_ [enemy | W.HeroDefends who enemy <- windows, who == you] $ \enemy ->
        afterMatching (\case
          VillainMessage vid VillainEndAttack -> enemy == EnemyVillainId vid
          MinionMessage mid MinionEndAttack -> enemy == EnemyMinionId mid
          _ -> False) (cardSpecific attrs "hcGauntletsEnded" ())
      pure $ WmHcUpgrade $ attrs `With` meta {gauntletRetaliate = n}
    CardSpecific (isTarget attrs -> True) "hcGauntletsEnded" _ | Name.title (cdName (upgradeCardDef attrs)) == "Gauntlets of Hercules" -> pure $ WmHcUpgrade $ attrs `With` meta {gauntletRetaliate = 0}
    RanAbility you (isTarget attrs -> True) 1 windows _ -> do
      case Name.title (cdName (upgradeCardDef attrs)) of
        "Ionic Physiology" -> do
          cards <- projectMap PlayerIdentityDiscard unDiscard you
          let ids = [CardId $ unEventId eid | W.EventPlayed who eid _ <- windows, who == you]
              eligible = [card | card <- cards, pcCardId card `elem` ids]
          host <- ionicCards you
          when (length host < 3) $ chooseOne you [targeting card (tuckIonicCard you card >> heal you 1) | card <- eligible]
        "Mr. Hollywood" -> pure ()
        "Caught in the Crossfire" -> for_ (upgradeAttachedEnemy attrs) $ \enemy -> dealDamage enemy attrs $ toDamage 1 FromAbility
        "Signature Sunglasses" -> do
          tucked <- ionicCards you
          discard <- filter (\card -> hasPrintedEnergy card && cardMatch (CardMatchesAny [CardWithType EventType, CardWithType ResourceType]) card) <$> projectMap PlayerIdentityDiscard unDiscard you
          chooseOne you $ [targeting card (tuckIonicCard you card) | length tucked < 3, card <- discard]
            <> [targeting card (ionicPhysiologyFor you >>= traverse_ (\host -> releaseHeldCard host you card)) | card <- tucked]
        "\"What Are You?\"" -> do
          cancelMatchingMessage $ \case IdentityMessage who IdentityDefeated -> who == you; _ -> False
          setHitPoints you 4
          changeToAlterEgo you
          cardSpecific attrs "whatAreYouTuck" you
        code | code `elem` ["Nemean Lion Skin", "Shield of Perseus", "Sword of Peleus"] -> drawCards you 4
        "Golden Mace" -> do
          n <- giftCount you
          addToIdentityBasicPower you n
          createEffect HC.hc59014 attrs (IdentityEntity $ IdentityWithId you)
        "Herc's Helm" -> for_ ([enemy | W.EnemyAttack enemy _who <- windows] <> [enemy | W.EnemyAttackAlly enemy _ally _who <- windows]) $ \enemy -> preventEnemyAttackDamage enemy 1
        "Prince of Power" -> for_ ([(source, n) | W.DefeatedMinion _ _ source n <- windows] <> [(source, n) | W.DefeatedVillain _ _ source n <- windows]) $ \(source, n) -> do
          owner <- getHeroSourceController source
          when (owner == Just you) $ heal you n
        "\"Avenge Me!\"" -> drawCards you 2
        _ -> pure ()
      pure u
    RanAbility you (isTarget attrs -> True) 2 _ _ | Name.title (cdName (upgradeCardDef attrs)) == "Jet Belt" -> do
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
