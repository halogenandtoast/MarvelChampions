module Marvel.Event.Events.WonderHerculesPack where

import Marvel.Prelude
import Marvel.Name qualified as Name
import Marvel.Card
import Marvel.Effect.Types
import Marvel.Damage
import Marvel.Discard
import Marvel.Game.Source
import Marvel.Hero.Runner qualified as Hero
import Marvel.Hercules.Helpers
import Marvel.Identity.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Resource.Types
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Event.Types
import Marvel.WonderMan.Cards qualified as WM
import Marvel.Hercules.Cards qualified as HC

wm58003 :: EventCard WmHcEvent
wm58003 = event WmHcEvent WM.wm58003

wm58004 :: EventCard WmHcEvent
wm58004 = event WmHcEvent WM.wm58004

wm58005 :: EventCard WmHcEvent
wm58005 = event WmHcEvent WM.wm58005

wm58016 :: EventCard WmHcEvent
wm58016 = event WmHcEvent WM.wm58016

wm58018 :: EventCard WmHcEvent
wm58018 = event WmHcEvent WM.wm58018

wm58019 :: EventCard WmHcEvent
wm58019 = event WmHcEvent WM.wm58019

wm58020 :: EventCard WmHcEvent
wm58020 = event WmHcEvent WM.wm58020

wm58033 :: EventCard WmHcEvent
wm58033 = event WmHcEvent WM.wm58033

hc59009 :: EventCard WmHcEvent
hc59009 = event WmHcEvent HC.hc59009

hc59010 :: EventCard WmHcEvent
hc59010 = event WmHcEvent HC.hc59010

hc59011 :: EventCard WmHcEvent
hc59011 = event WmHcEvent HC.hc59011

hc59026 :: EventCard WmHcEvent
hc59026 = event WmHcEvent HC.hc59026

newtype WmHcEvent = WmHcEvent (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage WmHcEvent where
  runMessage msg e@(WmHcEvent attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you payment window) | eid == eventId attrs -> do
      let excess = overpaidResources payment
          energy = fromIntegral $ length $ filter (== Energy) excess
      case Name.title (cdName (eventCardDef attrs)) of
        "Active Altruism" -> removeThreat you attrs (1 + 2 * min 2 energy) ThwartableScheme
        "Ionic Blast" -> chooseDamage you attrs (toDamage (3 + 2 * min 3 energy) $ FromPlayerAttack you) AttackableEnemy
        "Starstruck" -> do
          n <- characterAttackValue $ IdentityCharacter you
          enemies <- selectList AttackableEnemy
          chooseOne you [targeting (enemyRef enemy) do
            dealDamage enemy attrs (toDamage n $ FromPlayerAttack you)
            when (energy > 0) $ stun enemy attrs
            | enemy <- enemies]
        "Battlefield Benevolence" -> for_ (paymentTarget payment) $ \case
          VillainRef vid -> confuse vid attrs
          MinionRef mid -> confuse mid attrs
          AttachmentRef aid -> confuse (EnemyAttachmentId aid) attrs
          _ -> pure ()
        "Stronger Together" -> do
          n <- heroDefenseValue you
          for_ window $ \case
            W.AllyTakeDamage aid _ -> preventCharacterDamage aid n
            W.IdentityTakeDamage who _ -> preventCharacterDamage who n
            W.EnemyDamageWouldBeDealt enemy _ _ -> preventCharacterDamage (enemyCharacter enemy) n
            _ -> pure ()
        "Unified Strike" -> for_ window $ \case
          W.AllyAttack aid _ -> do
            n <- characterAttackValue $ IdentityCharacter you
            addToAllyBasicPower aid n
            createEffect WM.wm58020 attrs $ AllyEntity $ AllyWithId aid
          W.AllyThwart aid _ -> do
            n <- characterThwartValue $ IdentityCharacter you
            addToAllyBasicPower aid n
            createEffect WM.wm58020 attrs $ AllyEntity $ AllyWithId aid
          _ -> pure ()
        "Disarming Defense" -> do
          for_ window $ \case
            W.HeroDefends who _enemy | who == you -> do
              createEffect WM.wm58033 attrs $ IdentityEntity $ IdentityWithId you
            _ -> pure ()
        "Everywhere All at Once" -> cardSpecific attrs "everywhere" (paymentX payment, not $ null excess, [] :: [SchemeId])
        "The Gift of Battle" -> chooseDamage you attrs (toDamage 5 $ FromPlayerAttack you) AttackableEnemy
        "Son of Zeus" -> do
          ready you
          n <- giftCount you
          when (n >= 1) $ cardSpecific attrs "readySignature" you
          when (n >= 2) $ giveStatus you ToughStatus
          when (n >= 3) $ drawCards you 1
        "Wisdom of Athena" -> removeThreat you attrs 4 ThwartableScheme
        "Ancient Rivalry" -> do
          cards <- filter (cardMatch $ HeroSpecificCard <> CardWithType UpgradeType) <$> projectMap PlayerIdentityDiscard unDiscard you
          chooseOne you [targeting card (addToHand you card) | card <- cards]
          identities <- filterM (\p -> (||) <$> identityMatches (IdentityWithTitle "Hercules") p <*> identityMatches (IdentityWithTitle "Thor") p) =<< selectList AnyIdentity
          allies <- filterM (\a -> (||) <$> allyMatches (AllyWithTitle "Hercules") a <*> allyMatches (AllyWithTitle "Thor") a) =<< selectList AnyAlly
          traverse_ ready identities
          traverse_ ready allies
        _ -> pure ()
      pure e
    CardSpecific (isTarget attrs -> True) "everywhere" (fromJSON -> Success (remaining :: Natural, overpaid :: Bool, chosen :: [SchemeId])) -> do
      schemes <- filter (`notElem` chosen) <$> selectList ThwartableScheme
      if remaining == 0 || null schemes
        then traverse_ (\scheme -> thwartScheme scheme attrs (if overpaid then 3 else 2)) chosen
        else chooseOne (eventController attrs) [targeting (schemeRef scheme) $ cardSpecific attrs "everywhere" (remaining - 1, overpaid, scheme : chosen) | scheme <- schemes]
      pure e
    CardSpecific (isTarget attrs -> True) "readySignature" (fromJSON -> Success (you :: IdentityId)) -> do
      upgrades <- selectList $ UpgradeControlledBy (IdentityWithId you) <> UpgradeWithCard HeroSpecificCard <> NotUpgrade UnexhaustedUpgrade
      chooseOne you [targeting uid (ready uid) | uid <- upgrades]
      pure e
    _ -> WmHcEvent <$> runMessage msg attrs

instance HasAbilities WmHcEvent where
  getAbilities _ = []

-- Enemies retain their character identity when healed as a cost.
enemyCharacter :: EnemyId -> CharacterId
enemyCharacter = \case
  EnemyVillainId vid -> VillainCharacter vid
  EnemyMinionId mid -> MinionCharacter mid
  EnemyAttachmentId aid -> AttachmentCharacter aid

heroDefenseValue :: HasGame m => IdentityId -> m Natural
heroDefenseValue you = do
  side <- project PlayerIdentitySide you
  sides <- project PlayerIdentitySides you
  case lookup side sides of
    Just (HeroSide hero) -> Hero.getModifiedDefense $ toAttrs hero
    _ -> pure 0

newtype WmUnifiedStrikeEffect = WmUnifiedStrikeEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

wmUnifiedStrikeEffect :: CardEffect WmUnifiedStrikeEffect
wmUnifiedStrikeEffect = effectWith WmUnifiedStrikeEffect WM.wm58020 ((modifiersL .~ [NoAttackConsequentialDamage, NoThwartConsequentialDamage]) . (endsL ?~ DisableAtEndOfPhase))

instance RunMessage WmUnifiedStrikeEffect where
  runMessage msg e@(WmUnifiedStrikeEffect attrs) = runQueueT $ case msg of
    AllyMessage aid step | effectMatcher attrs == AllyEntity (AllyWithId aid), (case step of AllyAttackConsequential -> True; AllyAttackConsequentialAgainst _ -> True; AllyThwartConsequential -> True; AllyThwartConsequentialOn _ -> True; _ -> False) -> e <$ disableEffect (effectId attrs)
    _ -> WmUnifiedStrikeEffect <$> runMessage msg attrs

newtype WmDisarmingDefenseEffect = WmDisarmingDefenseEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

wmDisarmingDefenseEffect :: CardEffect WmDisarmingDefenseEffect
wmDisarmingDefenseEffect = effectWith WmDisarmingDefenseEffect WM.wm58033 ((modifiersL .~ [DefenseModifier 2]) . (endsL ?~ DisableAtEndOfPhase))

instance RunMessage WmDisarmingDefenseEffect where
  runMessage msg e@(WmDisarmingDefenseEffect attrs) = runQueueT $ case msg of
    CheckWindows windows -> do
      for_ [ (you, enemy) | W.Window W.After (W.DefendedWithoutDamage you enemy) <- windows, effectMatcher attrs == IdentityEntity (IdentityWithId you)] $ \(you, enemy) -> do
        attachments <- selectList $ AttachmentOnEnemy enemy <> AttachmentWithPrintedHeroActionOrResponse
        chooseOne you [targeting aid $ removeFromPlay aid | aid <- attachments]
        disableEffect $ effectId attrs
      when (any (\case W.Window W.After (W.HeroDefends you _) -> effectMatcher attrs == IdentityEntity (IdentityWithId you); _ -> False) windows) $ disableEffect $ effectId attrs
      pure e
    _ -> WmDisarmingDefenseEffect <$> runMessage msg attrs
