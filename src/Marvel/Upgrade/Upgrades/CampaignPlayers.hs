{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Upgrade.Upgrades.CampaignPlayers where
import Marvel.Prelude
import Marvel.Name qualified as Name
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Ability.Types qualified as A
import Marvel.Aspect qualified as Aspect
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Deck
import Marvel.Discard
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Phase
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource.Types qualified as R
import Marvel.Stats
import Marvel.Status
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.CampaignPlayers.Cards qualified as Cards
import Marvel.CampaignPlayers.Helpers
import Marvel.FalconWinter.Helpers (nextCardReduction)
import Marvel.Upgrade.Types

cw56004 :: UpgradeCard CampaignUpgrade
cw56004 = upgrade CampaignUpgrade Cards.cw56004

cw56005 :: UpgradeCard CampaignUpgrade
cw56005 = upgrade CampaignUpgrade Cards.cw56005

cw56006 :: UpgradeCard CampaignUpgrade
cw56006 = upgrade CampaignUpgrade Cards.cw56006

cw56007 :: UpgradeCard CampaignUpgrade
cw56007 = upgrade CampaignUpgrade Cards.cw56007

cw56013 :: UpgradeCard CampaignUpgrade
cw56013 = upgrade CampaignUpgrade Cards.cw56013

cw56032 :: UpgradeCard CampaignUpgrade
cw56032 = upgrade CampaignUpgrade Cards.cw56032

cw56033 :: UpgradeCard CampaignUpgrade
cw56033 = upgrade CampaignUpgrade Cards.cw56033

cw56034 :: UpgradeCard CampaignUpgrade
cw56034 = upgrade CampaignUpgrade Cards.cw56034

cw56035 :: UpgradeCard CampaignUpgrade
cw56035 = upgrade CampaignUpgrade Cards.cw56035

cw56036 :: UpgradeCard CampaignUpgrade
cw56036 = upgrade CampaignUpgrade Cards.cw56036

cw56045 :: UpgradeCard CampaignUpgrade
cw56045 = upgrade CampaignUpgrade Cards.cw56045

cw56046 :: UpgradeCard CampaignUpgrade
cw56046 = upgrade CampaignUpgrade Cards.cw56046

fne60002 :: UpgradeCard CampaignUpgrade
fne60002 = upgrade CampaignUpgrade Cards.fne60002

fne60003 :: UpgradeCard CampaignUpgrade
fne60003 = upgrade CampaignUpgrade Cards.fne60003

fne60004 :: UpgradeCard CampaignUpgrade
fne60004 = upgrade CampaignUpgrade Cards.fne60004

fne60005 :: UpgradeCard CampaignUpgrade
fne60005 = upgrade CampaignUpgrade Cards.fne60005

fne60006 :: UpgradeCard CampaignUpgrade
fne60006 = upgrade CampaignUpgrade Cards.fne60006

fne60017 :: UpgradeCard CampaignUpgrade
fne60017 = upgrade CampaignUpgrade Cards.fne60017

fne60018 :: UpgradeCard CampaignUpgrade
fne60018 = upgrade CampaignUpgrade Cards.fne60018

fne60026 :: UpgradeCard CampaignUpgrade
fne60026 = upgrade CampaignUpgrade Cards.fne60026

fne60027 :: UpgradeCard CampaignUpgrade
fne60027 = upgrade CampaignUpgrade Cards.fne60027

fne60028 :: UpgradeCard CampaignUpgrade
fne60028 = upgrade CampaignUpgrade Cards.fne60028

fne60030 :: UpgradeCard CampaignUpgrade
fne60030 = upgrade CampaignUpgrade Cards.fne60030

fne60031 :: UpgradeCard CampaignUpgrade
fne60031 = upgrade CampaignUpgrade Cards.fne60031

fne60038 :: UpgradeCard CampaignUpgrade
fne60038 = upgrade CampaignUpgrade Cards.fne60038

fne60043 :: UpgradeCard CampaignUpgrade
fne60043 = upgrade CampaignUpgrade Cards.fne60043

fne60044 :: UpgradeCard CampaignUpgrade
fne60044 = upgrade CampaignUpgrade Cards.fne60044

fne60045 :: UpgradeCard CampaignUpgrade
fne60045 = upgrade CampaignUpgrade Cards.fne60045

fne60046 :: UpgradeCard CampaignUpgrade
fne60046 = upgrade CampaignUpgrade Cards.fne60046

fne60047 :: UpgradeCard CampaignUpgrade
fne60047 = upgrade CampaignUpgrade Cards.fne60047

fne60052 :: UpgradeCard CampaignUpgrade
fne60052 = upgrade CampaignUpgrade Cards.fne60052

fne60053 :: UpgradeCard CampaignUpgrade
fne60053 = upgrade CampaignUpgrade Cards.fne60053

fne60054 :: UpgradeCard CampaignUpgrade
fne60054 = upgrade CampaignUpgrade Cards.fne60054

newtype CampaignUpgrade = CampaignUpgrade (Attrs Upgrade)

  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade CampaignUpgrade where
  upgradeTargets (CampaignUpgrade attrs) = case Name.title (cdName (upgradeCardDef attrs)) of
    code | code `elem` ["Acute Tactility","Enhanced Olfaction"] -> do
      enemies <- selectList AnyEnemy
      schemes <- selectList AnyScheme
      pure $ Just $ map enemyRef enemies <> map schemeTarget schemes
    code | code `elem` ["Heightened Hearing","Radar Sense","Dance with the Devil"] -> Just . map enemyRef <$> selectList AnyEnemy
    "Superior Taste" -> Just . map schemeTarget <$> selectList AnyScheme
    "Legal Trouble" -> Just . map enemyRef <$> selectList (MinionEnemy <> NotEnemy (EnemyWithUpgrade $ UpgradeIs Cards.fne60026))
    _ -> controlTargets attrs

instance HasModifiersFor CampaignUpgrade where
  getModifiersFor _ target (CampaignUpgrade attrs) = do
    let you = upgradeController attrs
        own = target == IdentityRef you
        code = Name.title (cdName (upgradeCardDef attrs))
    hero <- identityMatches HeroIdentity you
    allies <- selectListCount $ AllyControlledBy $ IdentityWithId you
    pure $ case code of
      "Aggressive Conditioning" | own -> [HitPointModifier 3] <> [AttackModifier 1 | hero]
      "Winged Shape" | own && hero -> [ThwartModifier 1, AttackModifier 1, DefenseModifier 1, TraitModifier T.Aerial]
      "Hulk Shape" | own && hero -> [AttackModifier 2, DefenseModifier 1]
      "Armored Shape" | own && hero -> [AttackModifier 1, DefenseModifier 2, KeywordModifier $ K.Retaliate 1]
      "Imitation Shape" | own && hero -> [ThwartModifier 2]
      "Defensive Conditioning" | own -> [HitPointModifier 3] <> [DefenseModifier 1 | hero]
      "Daredevil's Billy Club" | own && hero -> [AttackModifier 1]
      "Legal Trouble" | Just enemy <- upgradeAttachedEnemy attrs, target == enemyRef enemy || target == toRef enemy -> [SchemePenalty 2]
      "Innate Reflexes" | own && hero -> [DefenseModifier 1]
      "The Best Offense..." | own && hero -> [DefenseModifier 1, UseDefenseForAttackAndThwart]
      "Ronin" | own && hero && allies == 0 -> [DefenseModifier 1, KeywordModifier $ K.Retaliate 1]
      _ -> []

instance HasAbilities CampaignUpgrade where
  getAbilities u@(CampaignUpgrade attrs) = case Name.title (cdName (upgradeCardDef attrs)) of
    "Cat's Head Amulet" -> [ability u 1 Resource (OwnsThis <> MinionExists (MinionEngagedWith you)) ExhaustCost $ runAbility u 1]
    "Sharp Claws" -> [limitedWindowAbility u 1 (W.MakesBasicAttack W.When you) Interrupt OwnsThis ExhaustCost $ runAbility u 1]
    "Cat-Like Reflexes" -> [limitedWindowAbility u 1 (W.WouldTakeDamage you FromAnyDamageSource AnyValue) HeroInterrupt OwnsThis (DiscardCost $ toTarget attrs) $ runAbility u 1]
    "Hunted" -> [limitedWindowAbility u 1 (W.EnemyDefeated W.When (MinionEnemy <> EnemyWithUpgrade (UpgradeWithId $ upgradeId attrs)) AnyDamage) Interrupt OwnsThis NoCost $ runAbility u 1]
    "Armored Shape" -> [limitedWindowAbility u 1 (W.PlaysEvent W.After you AnyCard) HeroResponse
      (OwnsThis <> SelfMatches (IdentityWithAnyDamage <> IdentityCanBeHealed)) ExhaustCost $ runAbility u 1]
    code | code `elem` ["Winged Shape","Hulk Shape","Imitation Shape"] -> shapeResponse : [limitedWindowAbility u 2 (W.EnemyWouldAttack AnyEnemy you) ForcedInterrupt OwnsThis NoCost (runAbility u 2) | code == "Imitation Shape"]
    "Alien Physiology" -> [ability u 1 Resource OwnsThis ExhaustCost $ runAbility u 1]
    "Excelsior" -> [limitedWindowAbility u 1 (W.HeroDefended W.After you AnyEnemy) Response OwnsThis (ResourceCost $ Just R.Energy) $ runAbility u 1]
    "Acute Tactility" -> [senseDefeat]
    "Enhanced Olfaction" -> [senseDefeat]
    "Heightened Hearing" -> [subtype A.Defense $ limitedWindowAbility u 1 (W.EnemyAttacked W.When attached AnyIdentity) HeroInterrupt OwnsThis (DiscardCost $ toTarget attrs) $ runAbility u 1]
    "Radar Sense" -> [subtype A.Attack $ limitedWindowAbility u 1 (W.IdentityAttacked W.After you attached) Response OwnsThis (DiscardCost $ toTarget attrs) $ runAbility u 1]
    "Superior Taste" -> [subtype A.Thwart $ limitedWindowAbility u 1 (W.ThreatRemovedByThwart W.After (SourceExtendsIdentity you) (maybe (NotScheme AnyScheme) SchemeWithId $ attachedScheme attrs)) Response OwnsThis (DiscardCost $ toTarget attrs) $ runAbility u 1]
    "Daredevil's Billy Club" -> [ability u 1 HeroAction OwnsThis (ReturnUpgradeToHandCost $ UpgradeWithId $ upgradeId attrs) $ runAbility u 1]
    "The Man Without Fear" -> [ability u 1 HeroAction OwnsThis (ExhaustCost <> DamageCharacterCost (CharacterWithId $ IdentityCharacter $ upgradeController attrs) 1) $ runAbility u 1]
    "Move in Shadow" -> [subtype A.Thwart $ limitedWindowAbility u 1 (W.PlaysCard W.After you AnyCard) Response OwnsThis NoCost $ runAbility u 1]
    "Stealth Training" -> [limitedWindowAbility u 1 (W.SideSchemeExactlyDefeatedBy you) Response (OwnsThis <> EnemyExists EnemyCanBeStunned) ExhaustCost $ runAbility u 1]
    "Contingency Planning" -> [ability u 1 Action (OwnsThis <> ExtendedCardExists (InHandOf You $ BasicCardMatches contingencyAttachmentMatcher)) NoCost $ runAbility u 1 | null $ upgradeHeldCards attrs]
    "Dance with the Devil" -> [subtype A.Attack $ ability u 1 HeroAction OwnsThis (DiscardCost $ toTarget attrs) $ runAbility u 1]
    "American Sign Language" -> [ability u 1 HeroResource (OwnsThis <> ForAnyPlayer <> PayingFor (CardWithType EventType)) ExhaustCost $ runAbility u 1]
    "Choreography" -> [ability u 1 Action OwnsThis ExhaustCost $ runAbility u 1]
    "Echo's Katana" -> [subtype A.Attack $ limitedWindowAbility u 1 (W.PlaysEvent W.After you AnyCard) HeroResponse OwnsThis ExhaustCost $ runAbility u 1]
    "Improvisation" -> [limitedWindowAbility u 1 (W.PlaysEvent W.After you $ CardMatchesAny [CardWithTrait T.Attack, CardWithTrait T.Defense, CardWithTrait T.Thwart]) HeroResponse OwnsThis NoCost $ runAbility u 1]
    "Muscle Memory" -> [ability u 1 HeroAction (OwnsThis <> HasEchoTuckedEvent) ExhaustCost $ runAbility u 1]
    "Stand Alone" -> [limitedWindowAbility u 1 (W.EnemyAttacked W.When AnyEnemy you) HeroInterrupt (OwnsThis <> SelfMatches IdentityCanReady <> NotCriteria (AllyExists $ AllyControlledBy you)) ExhaustCost $ runAbility u 1]
    _ -> []
   where
    you = IdentityWithId $ upgradeController attrs
    attached = maybe (NotEnemy AnyEnemy) EnemyWithId $ upgradeAttachedEnemy attrs
    shapeResponse = limitedWindowAbility u 1 (W.PlaysEvent W.After you AnyCard) HeroResponse OwnsThis ExhaustCost $ runAbility u 1
    senseDefeat = limitedWindowAbility u 1
      (W.WindowOneOf [W.EnemyDefeated W.When attached $ DamageByHero you, W.RemovesLastThreatFrom you (maybe (NotScheme AnyScheme) SchemeWithId $ attachedScheme attrs)]) Interrupt OwnsThis (DiscardCost $ toTarget attrs) $ runAbility u 1

instance RunMessage CampaignUpgrade where
  runMessage msg u@(CampaignUpgrade attrs) = runQueueT $ case msg of
    UpgradeMessage uid (PlayedUpgrade target) | uid == upgradeId attrs, Name.title (cdName (upgradeCardDef attrs)) == "Hunted" -> do
      updated <- runMessage msg attrs
      for_ target $ \case
        MinionRef mid -> giveStatus mid StunnedStatus
        EnemyRef (EnemyMinionId mid) -> giveStatus mid StunnedStatus
        _ -> pure ()
      pure $ CampaignUpgrade updated
    RanAbility you (isTarget attrs -> True) n windows _ -> do
      case Name.title (cdName (upgradeCardDef attrs)) of
        "Cat's Head Amulet" -> do
          count <- selectListCount $ MinionEngagedWith $ IdentityWithId you
          gainResources $ replicate (fromIntegral $ min 3 count) R.Physical
        "Sharp Claws" -> do
          addToBasicPower you 1
          chooseOne you [labeled "$label.overkill" (addKeywordToAttack you attrs K.Overkill), labeled "$label.piercing" (addKeywordToAttack you attrs K.Piercing)]
        "Cat-Like Reflexes" -> do
          reduceIdentityDamage you 3
          enemies <- selectList $ MinionEngagedWith $ IdentityWithId you
          chooseOne you $ labeled "$label.done" (pure ()) : [targeting mid (giveStatus mid ConfusedStatus) | mid <- enemies]
        "Hunted" -> ready you
        "Winged Shape" -> removeThreat you attrs 2 ThwartableScheme
        "Hulk Shape" -> chooseDamage you attrs (toDamage 2 FromAbility) AnyEnemy
        "Armored Shape" -> heal you 2
        "Imitation Shape" -> if n == 1 then drawCards you 1 else for_ (windowEnemyAttacks windows) $ \enemy -> replacePendingAttackWithScheme enemy you
        "Alien Physiology" -> do
          specific <- payingForIdentitySpecific you
          gainResources $ replicate (if specific then 2 else 1) R.Wild
        "Excelsior" -> chooseDamage you attrs (toDamage 2 FromAbility) AnyEnemy
        "Acute Tactility" -> ready you
        "Enhanced Olfaction" -> nextCardReduction you attrs AnyCard 2
        "Heightened Hearing" -> for_ (upgradeAttachedEnemy attrs) $ \enemy -> do
          becomeDefender you enemy
          changeAttack enemy (-3)
        "Radar Sense" -> for_ (upgradeAttachedEnemy attrs) $ \enemy -> dealDamage enemy attrs (toDamage 3 $ FromPlayerAttack you)
        "Superior Taste" -> for_ (attachedScheme attrs) $ \scheme -> removeThreatFrom scheme attrs 2
        "Daredevil's Billy Club" -> chooseOne you
          [labeled "$label.dealDamage" (chooseDamage you attrs (toDamage 1 FromAbility) AnyEnemy)
          ,labeled "$label.gainAerial" (lastingModifiers attrs (IdentityEntity $ IdentityWithId you) [TraitModifier T.Aerial] True)]
        "The Man Without Fear" -> chooseOne you
          [labeled "$label.playSense" (chooseSense you attrs True), labeled "$label.ready" (ready you)]
        "Move in Shadow" -> removeThreat you attrs 1 ThwartableScheme
        "Stealth Training" -> chooseEnemyStatus you StunnedStatus AnyEnemy
        "Contingency Planning" -> do
          hand <- projectMap PlayerIdentityHand unHand you
          let allowed = filter canTuckUnderContingency hand
          chooseOne you [targeting card (holdFacedownCard attrs card) | card <- allowed]
        "Dance with the Devil" -> for_ (upgradeAttachedEnemy attrs) $ \enemy -> dealDamage enemy attrs (toDamage 3 $ FromPlayerAttack you)
        "American Sign Language" -> gainResources [R.Wild]
        "Choreography" -> do
          alter <- identityMatches AlterEgoIdentity you
          shuffleDiscardMatching you attrs (CardWithType EventType <> CardMatchesAny [CardWithAspect a | a <- [Aspect.Leadership,Aspect.Aggression,Aspect.Protection,Aspect.Justice,Aspect.Pool,Aspect.Neutral]]) alter
        "Echo's Katana" -> for_ [def | W.EventPlayed _ _ def <- windows] $ \printedDef -> do
          def <- resolvePlayerCardDef printedDef
          for_ (cdCost def) $ \cost -> chooseDamage you attrs (withPiercing $ toDamage (fromIntegral $ max 0 cost) $ FromPlayerAttack you) AttackableEnemy
        "Improvisation" -> for_ [def | W.EventPlayed _ _ def <- windows] $ \def -> do
          when (T.Attack `member` cdTraits def) $ heal you 1
          when (T.Defense `member` cdTraits def) $ cardSpecific attrs "improvisationThwart" you
          when (T.Thwart `member` cdTraits def) $ cardSpecific attrs "improvisationDamage" you
        "Muscle Memory" -> do
          cards <- tuckedPlayerCards you
          chooseOne you [targeting card (addTuckedPlayerCardToHand you card) | card <- cards]
        "Stand Alone" -> ready you
        _ -> pure ()
      pure u
    CardSpecific (isTarget attrs -> True) "improvisationThwart" (fromJSON -> Success you) -> u <$ removeThreat you attrs 1 ThwartableScheme
    CardSpecific (isTarget attrs -> True) "improvisationDamage" (fromJSON -> Success you) -> u <$ chooseDamage you attrs (toDamage 1 FromAbility) AnyEnemy
    CardSpecific (isTarget attrs -> True) "holdFacedownCard" _ | Name.title (cdName (upgradeCardDef attrs)) == "Contingency Planning" -> do
      updated <- runMessage msg attrs
      pure $ CampaignUpgrade updated {upgradeHeldCardsFacedown = False}
    _ -> CampaignUpgrade <$> runMessage msg attrs
