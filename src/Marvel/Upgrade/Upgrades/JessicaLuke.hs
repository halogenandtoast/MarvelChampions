{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Upgrade.Upgrades.JessicaLuke where
import Marvel.Prelude
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
  upgradeTargets (JLUpgrade a _) = case toCardCode a of
    "62009" -> Just . map enemyRef <$> selectList (MinionEnemy <> NotEnemy (EnemyWithUpgrade $ UpgradeIs Cards.jl62009))
    "62037" -> Just . map hostRef <$> selectList (CharacterControlledBy AnyIdentity)
    "61040" -> Just <$> getCardsInPlayTargets
    _ -> controlTargets a

instance HasModifiersFor JLUpgrade where
  getModifiersFor _ target (JLUpgrade a _) = do
    let you = upgradeController a
        self = target == IdentityRef you
        attached = Just target == upgradeAttachedCard a || target `elem` (map enemyRef (maybeToList $ upgradeAttachedEnemy a) <> map AllyRef (maybeToList $ upgradeAttachedAlly a) <> map IdentityRef (maybeToList $ upgradeAttachedIdentity a))
    controlled <- selectList (AllyControlledBy $ IdentityWithId you)
    pure $ case toCardCode a of
      "61011" | self -> [DefenseModifier 2]
      "61034" | self -> [AttackModifier 1]
      "61036" | self -> [ThwartModifier 1]
      "61037" | target `elem` map AllyRef controlled -> [HitPointModifier 1]
      "62009" | attached -> [AttackDamageTakenModifier 2, AttacksAgainstGainKeyword K.Overkill]
      "62010" | self -> [KeywordModifier $ K.Retaliate 1]
      "62037" | attached -> [HitPointModifier 1, TraitModifier T.MartialArtist]
      "61040" | attached || Just target == upgradeAttachedCard a -> [SuppressSchemeIcons]
      _ -> []

instance HasAbilities JLUpgrade where
  getAbilities (JLUpgrade a _) = case toCardCode a of
    "61009" -> [ability a 1 AlterEgoAction (OwnsThis <> CanDiscardMinionWithSupportCounters (SupportNamed "Alias Investigations" <> SupportControlledBy self) 2) ExhaustCost (runAbility a 1)]
    "61010" -> [limitedWindowAbility a 1 (W.EnemyDefeated W.After AnyEnemy AnyDamage) Response OwnsThis ExhaustCost (runAbility a 1)]
    "61011" -> [limitedWindowAbility a 1 (W.HeroDefended W.After self AnyEnemy) Response OwnsThis NoCost (runAbility a 1)]
    "61012" -> [subtype Attack $ limitedWindowAbility a 1 (W.EnemyActivates W.After VillainEnemy AnyIdentity) ForcedResponse OwnsThis NoCost (runAbility a 1)]
    "61013" -> [subtype Defense $ limitedWindowAbility a 1 (W.WouldTakeDamage self FromAttack (AtLeast $ Static 3)) HeroInterrupt (OwnsThis <> PendingDamagePreventable) (DiscardCost $ toTarget a) (runAbility a 1)]
    "61014" ->
      [ability a 1 Action (OwnsThis <> SideSchemeExists (SideSchemeWithCard (CardWithType SideSchemeType) <> NotSideScheme (SideSchemeWithKeyword K.Permanent))) NoCost (runAbility a 1) | isNothing (upgradeAttachedSideScheme a)] <>
      [limitedWindowAbility a 2 (W.CharacterThwartsScheme W.When AnySource (SchemeWithId $ SchemeSideSchemeId sid)) Interrupt OwnsThis NoCost (runAbility a 2) | sid <- maybeToList (upgradeAttachedSideScheme a)]
    "61023" -> [ability a 1 HeroAction OwnsThis (DiscardCost $ toTarget a) (runAbility a 1)]
    "61024" -> [subtype Attack $ limitedWindowAbility a 1 (W.VillainSchemedPlacingThreat AnyVillain) AlterEgoResponse
      (OwnsThis <> SelfMatches IdentityCanChangeForm) (DiscardCost $ toTarget a) (runAbility a 1)]
    "61035" -> [limitedWindowAbility a 1 (W.EnemyExactlyDefeatedByAttack self) Response OwnsThis ExhaustCost (runAbility a 1)]
    "61040" -> [limitedWindowAbility a 1 W.RoundEnds ForcedResponse OwnsThis NoCost (runAbility a 1)]
    "62008" -> [ability a 1 AlterEgoAction OwnsThis ExhaustCost (runAbility a 1)]
    "62009" -> [limitedWindowAbility a 1 (W.PlayThis W.After) Response OwnsThis NoCost (runAbility a 1)]
    "62010" -> [limitedWindowAbility a 1 (W.WouldTakeDamage self FromAnyDamageSource AnyValue) HeroInterrupt (OwnsThis <> PendingDamagePreventable) ExhaustCost (runAbility a 1)]
    "62011" -> [limitedWindowAbility a 1 (W.IdentityDiscardsTough W.After self) HeroResponse OwnsThis ExhaustCost (runAbility a 1)]
    "62036" -> [limitedWindowAbility a 1 (W.UsesRecovery self) Response (OwnsThis <> SelfMatches IdentityWithAtLeastBaseHitPoints <> CharacterExists (CharacterWithId (IdentityCharacter $ upgradeController a) <> CharacterCanGainTough)) ExhaustCost (runAbility a 1)]
    _ -> []
   where self = IdentityWithId $ upgradeController a

instance RunMessage JLUpgrade where
  runMessage msg e@(JLUpgrade a stakeoutThwart) = runQueueT $ case msg of
    UpgradeMessage uid (PlayedUpgrade (Just host)) | uid == upgradeId a, toCardCode a `elem` ["62037", "61040"] -> do
      case host of
        IdentityRef owner -> do
          upgradeStep uid (UpgradeControlledByPlayer owner)
          upgradeStep uid (UpgradeAttachedToIdentity owner)
        AllyRef aid -> upgradeStep uid (UpgradeAttachedToAlly aid)
        _ -> upgradeCreated (upgradeController a) uid
      attachUpgradeToTarget uid host
      pure e
    RanAbility you (isTarget a -> True) 1 windows _ -> do
      case toCardCode a of
        "61009" -> do
          evidence <- evidenceCount you
          minions <- selectList (NotMinion $ MinionWithTrait T.Elite)
          eligible <- filterM (\mid -> (<= evidence + 2) <$> getEnemyRemainingHitPoints (EnemyMinionId mid)) minions
          chooseOne you [targeting mid $ do
            hp <- getEnemyRemainingHitPoints (EnemyMinionId mid)
            spendEvidence you (if hp <= 2 then 0 else hp - 2)
            removeFromPlay mid | mid <- eligible]
        "61010" -> addEvidence you 1
        "61011" -> addEvidence you 1
        "61012" -> do
          ae <- identityMatches AlterEgoIdentity you
          when ae $ changeToHeroForm you
          for_ [(vid,p) | W.EnemyActivation (EnemyVillainId vid) p <- windows] $ \(vid,p) -> do
            minions <- selectList (MinionEngagedWith $ IdentityWithId p)
            dealDamageToEach (EnemyVillainId vid : map EnemyMinionId minions) a (toDamage 5 $ FromPlayerAttack you)
          removeFromPlay a
        "61013" -> cancelDamage you >> damageIdentity you a (toDamage 1 FromAbility)
        "61014" -> do
          schemes <- selectList (SideSchemeWithCard (CardWithType SideSchemeType) <> NotSideScheme (SideSchemeWithKeyword K.Permanent))
          chooseOne you [targeting sid (upgradeStep (upgradeId a) $ UpgradeAttachedToSideScheme sid) | sid <- schemes]
        "61023" -> do
          basicThwartIgnoringPatrolAndCrisis you 1
        "61024" -> do
          changeToHeroForm you
          for_ [(vid,n) | W.VillainSchemeResolved vid n <- windows] $ \(vid,n) -> do
            dealDamage vid a (toDamage n $ FromPlayerAttack you)
        "61035" -> for_ [(mid,n) | W.DefeatedMinion mid _ _ n <- windows, n == 0] $ \(mid,_) -> do
          sch <- getEnemyBaseScheme (EnemyMinionId mid)
          removeThreat you a sch ThwartableScheme
        "61040" -> getPlayers >>= \players -> cardSpecific a "maintainMitigation" (players, True)
        "62008" -> do
          n <- toughCount you
          replicateM_ (if n == 0 then 2 else 1) $ becomeTough you
        "62009" -> for_ (upgradeAttachedEnemy a) $ \case
          EnemyMinionId mid -> engageMinion mid you
          _ -> pure ()
        "62010" -> preventDamage you 1
        "62011" -> drawCards you 1
        "62036" -> do
          (remaining,_,base) <- getRemainingHitPoints you
          when (remaining >= base) $ becomeTough you
        _ -> pure ()
      pure e
    RanAbility _ (isTarget a -> True) 2 windows _ | toCardCode a == "61014" -> do
      let thwarts = [(source,sid) | W.ThwartRemovedThreat source (SchemeSideSchemeId sid) _ _ <- windows]
      for_ thwarts $ \(source,sid) -> do
        increaseCurrentThwart source (SchemeSideSchemeId sid) 2
        afterMatching (isThwartResolution source sid) (cardSpecific a "clearStakeoutThwart" ())
      pure $ JLUpgrade a (fst <$> headMay thwarts)
    CheckWindows windows | toCardCode a == "61014", Just source <- stakeoutThwart -> do
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
