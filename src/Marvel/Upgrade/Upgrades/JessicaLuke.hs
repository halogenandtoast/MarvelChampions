{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Upgrade.Upgrades.JessicaLuke where
import Marvel.Prelude
import Marvel.Name qualified as Name
import Control.Monad (replicateM_)
import Data.Maybe (maybeToList)
import Data.HashMap.Strict qualified as HashMap
import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question hiding (Attack, Thwart)
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Status
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.JessicaJones.Cards qualified as Cards
import Marvel.JessicaJones.Helpers
import Marvel.Phase
import Marvel.Upgrade.Types

data JLUpgrade = JLUpgrade (Attrs Upgrade) (Maybe Source)
  deriving stock (Show, Eq, Generic)

-- Preserve the original attrs-object save format; the pending thwart is optional
-- so games saved before Stakeout began tracking its outcome continue to load.
instance ToJSON JLUpgrade where
  toJSON (JLUpgrade attrs pending) = case toJSON attrs of
    Object object -> Object $ KeyMap.insert "stakeoutThwart" (toJSON pending) object
    other -> other

instance FromJSON JLUpgrade where
  parseJSON value = withObject "JLUpgrade" (\object ->
    JLUpgrade <$> parseJSON value <*> object .:? "stakeoutThwart") value

instance HasCardCode JLUpgrade where
  toCardCode (JLUpgrade a _) = toCardCode a

instance IsRef JLUpgrade where
  toRef (JLUpgrade a _) = toRef a


jl61009 :: UpgradeCard JLUpgrade
jl61009 = upgrade (\a -> JLUpgrade a Nothing) Cards.jl61009

jl61010 :: UpgradeCard JLUpgrade
jl61010 = upgrade (\a -> JLUpgrade a Nothing) Cards.jl61010

jl61011 :: UpgradeCard JLUpgrade
jl61011 = upgrade (\a -> JLUpgrade a Nothing) Cards.jl61011

jl61012 :: UpgradeCard JLUpgrade
jl61012 = upgrade (\a -> JLUpgrade a Nothing) Cards.jl61012

jl61013 :: UpgradeCard JLUpgrade
jl61013 = upgrade (\a -> JLUpgrade a Nothing) Cards.jl61013

jl61014 :: UpgradeCard JLUpgrade
jl61014 = upgrade (\a -> JLUpgrade a Nothing) Cards.jl61014

jl61023 :: UpgradeCard JLUpgrade
jl61023 = upgrade (\a -> JLUpgrade a Nothing) Cards.jl61023

jl61024 :: UpgradeCard JLUpgrade
jl61024 = upgrade (\a -> JLUpgrade a Nothing) Cards.jl61024

jl61034 :: UpgradeCard JLUpgrade
jl61034 = upgrade (\a -> JLUpgrade a Nothing) Cards.jl61034

jl61035 :: UpgradeCard JLUpgrade
jl61035 = upgrade (\a -> JLUpgrade a Nothing) Cards.jl61035

jl61036 :: UpgradeCard JLUpgrade
jl61036 = upgrade (\a -> JLUpgrade a Nothing) Cards.jl61036

jl61037 :: UpgradeCard JLUpgrade
jl61037 = upgrade (\a -> JLUpgrade a Nothing) Cards.jl61037

jl61040 :: UpgradeCard JLUpgrade
jl61040 = upgrade (\a -> JLUpgrade a Nothing) Cards.jl61040

jl62008 :: UpgradeCard JLUpgrade
jl62008 = upgrade (\a -> JLUpgrade a Nothing) Cards.jl62008

jl62009 :: UpgradeCard JLUpgrade
jl62009 = upgrade (\a -> JLUpgrade a Nothing) Cards.jl62009

jl62010 :: UpgradeCard JLUpgrade
jl62010 = upgrade (\a -> JLUpgrade a Nothing) Cards.jl62010

jl62011 :: UpgradeCard JLUpgrade
jl62011 = upgrade (\a -> JLUpgrade a Nothing) Cards.jl62011

jl62036 :: UpgradeCard JLUpgrade
jl62036 = upgrade (\a -> JLUpgrade a Nothing) Cards.jl62036

jl62037 :: UpgradeCard JLUpgrade
jl62037 = upgrade (\a -> JLUpgrade a Nothing) Cards.jl62037

instance IsUpgrade JLUpgrade where
  toUpgradeAttrs (JLUpgrade a _) = a
  upgradeTargets (JLUpgrade a _) = case Name.title (cdName (upgradeCardDef a)) of
    "Cruisin' for a Bruisin'" -> Just . map enemyRef <$> selectList (MinionEnemy <> NotEnemy (EnemyWithUpgrade $ UpgradeIs Cards.jl62009))
    "Black Belt" -> Just . map hostRef <$> selectList (CharacterControlledBy AnyIdentity)
    "Mitigated Threat" -> Just <$> getCardsInPlayTargets
    _ -> controlTargets a

instance HasModifiersFor JLUpgrade where
  getModifiersFor _ target (JLUpgrade a _) = do
    let you = upgradeController a
        self = target == IdentityRef you
        attached = Just target == upgradeAttachedCard a || target `elem` (map enemyRef (maybeToList $ upgradeAttachedEnemy a) <> map AllyRef (maybeToList $ upgradeAttachedAlly a) <> map IdentityRef (maybeToList $ upgradeAttachedIdentity a))
    controlled <- selectList (AllyControlledBy $ IdentityWithId you)
    pure $ case Name.title (cdName (upgradeCardDef a)) of
      "Leather Jacket" | self -> [DefenseModifier 2]
      "Innate Aggression" | self -> [AttackModifier 1]
      "Innate Perception" | self -> [ThwartModifier 1]
      "Innate Inspiration" | target `elem` map AllyRef controlled -> [HitPointModifier 1]
      "Cruisin' for a Bruisin'" | attached -> [AttackDamageTakenModifier 2, AttacksAgainstGainKeyword K.Overkill]
      "Metal Bracer" | self -> [KeywordModifier $ K.Retaliate 1]
      "Black Belt" | attached -> [HitPointModifier 1, TraitModifier T.MartialArtist]
      "Mitigated Threat" | attached || Just target == upgradeAttachedCard a -> [SuppressSchemeIcons]
      _ -> []

instance HasAbilities JLUpgrade where
  getAbilities (JLUpgrade a _) = case Name.title (cdName (upgradeCardDef a)) of
    "4K Digital Camcorder" -> [ability a 1 AlterEgoAction (OwnsThis <> CanDiscardMinionWithSupportCounters (SupportNamed "Alias Investigations" <> SupportControlledBy self) 2) ExhaustCost (runAbility a 1)]
    "Circumstantial Evidence" -> [limitedWindowAbility a 1 (W.EnemyDefeated W.After AnyEnemy AnyDamage) Response OwnsThis ExhaustCost (runAbility a 1)]
    "Leather Jacket" -> [limitedWindowAbility a 1 (W.HeroDefended W.After self AnyEnemy) Response OwnsThis NoCost (runAbility a 1)]
    "\"Now I'm Ticked Off!\"" -> [subtype Attack $ limitedWindowAbility a 1 (W.EnemyActivates W.After VillainEnemy AnyIdentity) ForcedResponse OwnsThis NoCost (runAbility a 1)]
    "Reluctant Flier" -> [subtype Defense $ limitedWindowAbility a 1 (W.WouldTakeDamage self FromAttack (AtLeast $ Static 3)) HeroInterrupt (OwnsThis <> PendingDamagePreventable) (DiscardCost $ toTarget a) (runAbility a 1)]
    "Stakeout" ->
      [ability a 1 Action (OwnsThis <> SideSchemeExists (SideSchemeWithCard (CardWithType SideSchemeType) <> NotSideScheme (SideSchemeWithKeyword K.Permanent))) NoCost (runAbility a 1) | isNothing (upgradeAttachedSideScheme a)] <>
      [limitedWindowAbility a 2 (W.CharacterThwartsScheme W.When AnySource (SchemeWithId $ SchemeSideSchemeId sid)) Interrupt OwnsThis NoCost (runAbility a 2) | sid <- maybeToList (upgradeAttachedSideScheme a)]
    "Grapnel Launcher" -> [ability a 1 HeroAction OwnsThis (DiscardCost $ toTarget a) (runAbility a 1)]
    "Entrapment" -> [subtype Attack $ limitedWindowAbility a 1 (W.VillainSchemedPlacingThreat AnyVillain) AlterEgoResponse
      (OwnsThis <> SelfMatches IdentityCanChangeForm) (DiscardCost $ toTarget a) (runAbility a 1)]
    "Shakedown" -> [limitedWindowAbility a 1 (W.EnemyExactlyDefeatedByAttack self) Response OwnsThis ExhaustCost (runAbility a 1)]
    "Mitigated Threat" -> [limitedWindowAbility a 1 W.RoundEnds ForcedResponse OwnsThis NoCost (runAbility a 1)]
    "Burstein Process" -> [ability a 1 AlterEgoAction OwnsThis ExhaustCost (runAbility a 1)]
    "Cruisin' for a Bruisin'" -> [limitedWindowAbility a 1 (W.PlayThis W.After) Response OwnsThis NoCost (runAbility a 1)]
    "Metal Bracer" -> [limitedWindowAbility a 1 (W.WouldTakeDamage self FromAnyDamageSource AnyValue) HeroInterrupt (OwnsThis <> PendingDamagePreventable) ExhaustCost (runAbility a 1)]
    "Power Man" -> [limitedWindowAbility a 1 (W.IdentityDiscardsTough W.After self) HeroResponse OwnsThis ExhaustCost (runAbility a 1)]
    "Toughen Up" -> [limitedWindowAbility a 1 (W.UsesRecovery self) Response (OwnsThis <> SelfMatches IdentityWithAtLeastBaseHitPoints <> CharacterExists (CharacterWithId (IdentityCharacter $ upgradeController a) <> CharacterCanGainTough)) ExhaustCost (runAbility a 1)]
    _ -> []
   where self = IdentityWithId $ upgradeController a

instance RunMessage JLUpgrade where
  runMessage msg e@(JLUpgrade a stakeoutThwart) = runQueueT $ case msg of
    UpgradeMessage uid (PlayedUpgrade (Just host)) | uid == upgradeId a, Name.title (cdName (upgradeCardDef a)) `elem` ["Black Belt", "Mitigated Threat"] -> do
      case host of
        IdentityRef owner -> do
          upgradeStep uid (UpgradeControlledByPlayer owner)
          upgradeStep uid (UpgradeAttachedToIdentity owner)
        AllyRef aid -> upgradeStep uid (UpgradeAttachedToAlly aid)
        _ -> upgradeCreated (upgradeController a) uid
      attachUpgradeToTarget uid host
      pure e
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      case Name.title (cdName (upgradeCardDef a)) of
        "4K Digital Camcorder" -> do
          evidence <- evidenceCount you
          minions <- selectList (NotMinion $ MinionWithTrait T.Elite)
          eligible <- filterM (\mid -> (<= evidence + 2) <$> getEnemyRemainingHitPoints (EnemyMinionId mid)) minions
          chooseOne you [targeting mid $ do
            hp <- getEnemyRemainingHitPoints (EnemyMinionId mid)
            spendEvidence you (if hp <= 2 then 0 else hp - 2)
            removeFromPlay mid | mid <- eligible]
        "Circumstantial Evidence" -> addEvidence you 1
        "Leather Jacket" -> addEvidence you 1
        "\"Now I'm Ticked Off!\"" -> do
          ae <- identityMatches AlterEgoIdentity you
          when ae $ changeToHeroForm you
          for_ [(vid,p) | W.EnemyActivation (EnemyVillainId vid) p <- windows] $ \(vid,p) -> do
            minions <- selectList (MinionEngagedWith $ IdentityWithId p)
            dealDamageToEach (EnemyVillainId vid : map EnemyMinionId minions) a (toDamage 5 $ FromPlayerAttack you)
          removeFromPlay a
        "Reluctant Flier" -> cancelDamage you >> damageIdentity you a (toDamage 1 FromAbility)
        "Stakeout" -> do
          schemes <- selectList (SideSchemeWithCard (CardWithType SideSchemeType) <> NotSideScheme (SideSchemeWithKeyword K.Permanent))
          chooseOne you [targeting sid (upgradeStep (upgradeId a) $ UpgradeAttachedToSideScheme sid) | sid <- schemes]
        "Grapnel Launcher" -> do
          basicThwartIgnoringPatrolAndCrisis you 1
        "Entrapment" -> do
          changeToHeroForm you
          for_ [(vid,n) | W.VillainSchemeResolved vid n <- windows] $ \(vid,n) -> do
            dealDamage vid a (toDamage n $ FromPlayerAttack you)
        "Shakedown" -> for_ [(mid,n) | W.DefeatedMinion mid _ _ n <- windows, n == 0] $ \(mid,_) -> do
          sch <- getEnemyBaseScheme (EnemyMinionId mid)
          removeThreat you a sch ThwartableScheme
        "Mitigated Threat" -> getPlayers >>= \players -> cardSpecific a "maintainMitigation" (players, True)
        "Burstein Process" -> do
          n <- toughCount you
          replicateM_ (if n == 0 then 2 else 1) $ becomeTough you
        "Cruisin' for a Bruisin'" -> for_ (upgradeAttachedEnemy a) $ \case
          EnemyMinionId mid -> engageMinion mid you
          _ -> pure ()
        "Metal Bracer" -> preventDamage you 1
        "Power Man" -> drawCards you 1
        "Toughen Up" -> do
          (remaining,_,base) <- getRemainingHitPoints you
          when (remaining >= base) $ becomeTough you
        _ -> pure ()
      pure e
    RanAbility _ (isTarget a -> True) 2 windows _ | Name.title (cdName (upgradeCardDef a)) == "Stakeout" -> do
      let thwarts = [(source,sid) | W.ThwartRemovedThreat source (SchemeSideSchemeId sid) _ _ <- windows]
      for_ thwarts $ \(source,sid) -> do
        increaseCurrentThwart source (SchemeSideSchemeId sid) 2
        afterMatching (isThwartResolution source sid) (cardSpecific a "clearStakeoutThwart" ())
      pure $ JLUpgrade a (fst <$> headMay thwarts)
    CheckWindows windows | Name.title (cdName (upgradeCardDef a)) == "Stakeout", Just source <- stakeoutThwart -> do
      for_ [sid | W.Window W.After (W.ThwartRemovedThreat from (SchemeSideSchemeId sid) before removed) <- windows,
        from == source, Just sid == upgradeAttachedSideScheme a, before > 0, removed == before] $ \sid ->
          afterMatching (== SideSchemeMessage sid DefeatSideScheme) (addEvidence (upgradeController a) 2)
      pure e
    CardSpecific (isTarget a -> True) "clearStakeoutThwart" _ -> pure $ JLUpgrade a Nothing
    CardSpecific (isTarget a -> True) "maintainMitigation" (fromJSON -> Success (players :: [IdentityId], allPaid :: Bool)) -> do
      case players of
        [] -> unless allPaid (removeFromPlay a)
        p:rest -> do
          canPay <- asYou p $ canAffordCost p (toSource a) (ResourceCost Nothing)
          chooseOne p $ [labeled "$label.payResource" (payForTreachery p (ResourceCost Nothing) >> cardSpecific a "maintainMitigation" (rest,allPaid)) | canPay]
            <> [labeled "$label.doNotPay" (cardSpecific a "maintainMitigation" (rest,False))]
      pure e
    _ -> (\a' -> JLUpgrade a' stakeoutThwart) <$> runMessage msg a

hostRef :: CharacterId -> Target
hostRef = \case
  IdentityCharacter p -> IdentityRef p
  AllyCharacter a -> AllyRef a
  VillainCharacter v -> VillainRef v
  MinionCharacter m -> MinionRef m
  AttachmentCharacter a -> AttachmentRef a

-- The marker follows this one thwart, including all its removal/defeat windows.
-- It prevents a canceled thwart from leaving a later use of the source armed.
isThwartResolution :: Source -> SideSchemeId -> Message -> Bool
isThwartResolution source sid = \case
  CardSpecific (SideSchemeRef target) "resolveThwart" (fromJSON -> Success (from :: Source, _amount :: Natural)) -> target == sid && from == source
  _ -> False
