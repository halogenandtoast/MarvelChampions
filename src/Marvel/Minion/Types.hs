module Marvel.Minion.Types where

import Marvel.Prelude

import Data.HashSet qualified as HashSet ()
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Typeable
import Marvel.Ability (windowAbility)
import Marvel.Ability.Types
import Marvel.Criteria (Criteria (MinionExists))
import Marvel.Cost.Types (Cost (NoCost))
import Marvel.Matchers.Types (MinionMatcher (..))
import Marvel.Question (Choice (RunAbility))
import Marvel.Window.Types qualified as W
import Marvel.Damage
import Marvel.Attack
import Marvel.Card
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Hp
import Marvel.Id hiding (MinionId)
import Marvel.Id as X (MinionId)
import Marvel.Keyword
import Marvel.Message hiding (
  MinionConfused,
  MinionEngagedIdentity,
  MinionStunned,
 )
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats
import Marvel.Trait

data Minion = forall a. (IsMinion a) => Minion a

instance Show Minion where
  show (Minion a) = show a

instance ToJSON Minion where
  toJSON (Minion a) = toJSON a

instance Eq Minion where
  Minion (a :: a) == Minion (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

data SomeMinionCard = forall a. (IsMinion a) => SomeMinionCard (MinionCard a)

liftMinionCard :: (forall a. MinionCard a -> b) -> SomeMinionCard -> b
liftMinionCard f (SomeMinionCard a) = f a

someMinionCardCode :: SomeMinionCard -> CardCode
someMinionCardCode = liftMinionCard toCardCode

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, HasModifiersFor a, RunMessage a, HasAbilities a) => IsMinion a where
  toMinionAttrs :: a -> Attrs Minion
  default toMinionAttrs :: (Coercible a (Attrs Minion)) => a -> Attrs Minion
  toMinionAttrs = coerce
  minionDisplayCounters :: a -> Natural
  minionDisplayCounters = minionCounters . toMinionAttrs
  minionBaseHitPoints :: HasGame m => a -> m Natural
  minionBaseHitPoints = pure . unHp . minionHitPoints . toMinionAttrs
  minionAccelerationTokens :: a -> Natural
  minionAccelerationTokens _ = 0
  -- Whether pausing to acknowledge this minion's reveal would show an effect.
  -- The reveal itself still resolves when this is false.
  minionWhenRevealedHasEffect :: HasGame m => a -> m Bool
  minionWhenRevealedHasEffect _ = pure True
  overMinionAttrs :: (Attrs Minion -> Attrs Minion) -> a -> a
  default overMinionAttrs :: (Coercible a (Attrs Minion)) => (Attrs Minion -> Attrs Minion) -> a -> a
  overMinionAttrs = coerce

overMinion :: (Attrs Minion -> Attrs Minion) -> Minion -> Minion
overMinion f (Minion a) = Minion (overMinionAttrs f a)

getMinionFacedownCard :: Minion -> Maybe PlayerCard
getMinionFacedownCard = minionFacedownCard . toAttrs

type MinionCard a = CardBuilder (IdentityId, MinionId) a

getMinionDamage :: Minion -> Natural
getMinionDamage = minionDamage . toAttrs

getMinionEngagedIdentity :: Minion -> IdentityId
getMinionEngagedIdentity = minionEngagedIdentity . toAttrs

getMinionPrintedHitPoints :: Minion -> HP Natural
getMinionPrintedHitPoints = minionHitPoints . toAttrs

getMinionBaseHitPoints :: HasGame m => Minion -> m Natural
getMinionBaseHitPoints (Minion a) = minionBaseHitPoints a

perPlayerMinionBaseHitPoints :: HasGame m => Natural -> m Natural
perPlayerMinionBaseHitPoints n = (n *) . fromIntegral <$> getStartingPlayerCount

minionAttackDetails :: Minion -> Maybe Attack
minionAttackDetails = minionAttacking . toAttrs

instance Entity Minion where
  type Id Minion = MinionId
  data Attrs Minion = MinionAttrs
    { minionId :: MinionId
    , minionCardDef :: CardDef
    , minionOriginalCardDef :: CardDef
    , minionExhausted :: Bool
    -- ^ physical state retained when an ally is temporarily treated as a minion
    , minionCounters :: Natural
    , minionDamage :: Natural
    , minionIgnoreWhenDefeated :: Bool
    , minionDamageThisPhase :: Natural
    , minionHitPoints :: HP Natural
    , minionSchemeAvailable :: Bool
    , minionAttackAvailable :: Bool
    , minionScheme :: Sch
    , minionDefeatedBaseScheme :: Maybe Natural
    -- ^ defined base SCH preserved for after-defeat responses
    , minionAttack :: Atk
    , minionEngagedIdentity :: IdentityId
    , minionStunned :: Bool
    , minionConfused :: Bool
    , minionAdditionalStun :: Bool
    , minionAdditionalConfuse :: Bool
    , minionTough :: Bool
    , minionAdditionalTough :: Natural
    , minionAttacking :: Maybe Attack
    , minionUpgrades :: HashSet UpgradeId
    , minionAttachments :: HashSet AttachmentId
    , minionDefensePriority :: DefensePriority
    , minionFacedownCard :: Maybe PlayerCard
    , minionOriginalAlly :: Maybe Value
    , minionOriginalCard :: Maybe Card
    -- ^ the player card this minion is, put into play facedown (as Ultron's drones are)
    , minionAttachedTo :: Maybe MinionId
    , minionSchemeReplacesAttack :: Bool
    , minionSchemeRemoval :: Maybe IdentityId
    , minionActivationPlayer :: Maybe IdentityId
    , minionAttackTarget :: Maybe CharacterId
    -- ^ the character its coming attack is against, when not the player's identity
    , minionBoostCards :: [EncounterCard]
    -- ^ dealt facedown for its activation, as a villainous minion's are
    , minionHeldCards :: [PlayerCard]
    , minionHeldTreacheries :: [EncounterCard]
    -- ^ facedown attachments kept when an ally is treated as a minion
    , minionBoost :: Natural
    -- ^ the boost icons its activation under way has
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON)
  data Field Minion :: Type -> Type where
    MinionId :: Field Minion MinionId
    MinionHeldCards :: Field Minion [PlayerCard]
    MinionHeldTreacheries :: Field Minion [EncounterCard]
    MinionCardDef :: Field Minion CardDef
    MinionDamage :: Field Minion Natural
    MinionHitPoints :: Field Minion (HP Natural)
    MinionScheme :: Field Minion Sch
    MinionAttack :: Field Minion Atk
    MinionEngagedIdentity :: Field Minion IdentityId
    MinionStunned :: Field Minion Bool
    MinionConfused :: Field Minion Bool
    MinionAdditionalStun :: Field Minion Bool
    MinionAdditionalConfuse :: Field Minion Bool
    MinionTough :: Field Minion Bool
    MinionAdditionalTough :: Field Minion Natural
    MinionAttacking :: Field Minion (Maybe Attack)
    MinionUpgrades :: Field Minion (HashSet UpgradeId)
    MinionAttachments :: Field Minion (HashSet AttachmentId)
    MinionDefensePriority :: Field Minion DefensePriority
    MinionFacedownCard :: Field Minion (Maybe PlayerCard)
  field fld m =
    let MinionAttrs {..} = toAttrs m
     in case fld of
          MinionId -> minionId
          MinionHeldCards -> minionHeldCards
          MinionHeldTreacheries -> minionHeldTreacheries
          MinionCardDef -> minionCardDef
          MinionDamage -> minionDamage
          MinionHitPoints -> minionHitPoints
          MinionScheme -> minionScheme
          MinionAttack -> minionAttack
          MinionEngagedIdentity -> minionEngagedIdentity
          MinionStunned -> minionStunned
          MinionConfused -> minionConfused
          MinionAdditionalStun -> minionAdditionalStun
          MinionAdditionalConfuse -> minionAdditionalConfuse
          MinionTough -> minionTough
          MinionAdditionalTough -> minionAdditionalTough
          MinionAttacking -> minionAttacking
          MinionUpgrades -> minionUpgrades
          MinionAttachments -> minionAttachments
          MinionDefensePriority -> minionDefensePriority
          MinionFacedownCard -> minionFacedownCard
  toId = minionId . toAttrs
  toAttrs (Minion a) = toMinionAttrs a

instance RunMessage Minion where
  runMessage msg (Minion a) = Minion <$> runMessage msg a

instance HasAbilities Minion where
  getAbilities (Minion a) = getAbilities a <>
    [ (windowAbility attrs 9901 (W.MinionEntersPlay W.After $ MinionWithId $ minionId attrs)
        ForcedResponse NoCost (RunAbility (toTarget attrs) 9901)) {abilityCriteria = MinionExists $ MinionWithTrait trait <> NotMinion (MinionWithId $ minionId attrs)}
    | let attrs = toMinionAttrs a, Teamwork trait <- toList $ cdKeywords $ getCardDef attrs
    ]

instance HasModifiersFor Minion where
  getModifiersFor source target (Minion a) = do
    printed <- getModifiersFor source target a
    let attrs = toMinionAttrs a
    pure $ printed <> [modifier | target == toRef attrs, isJust (minionAttachedTo attrs), modifier <- [CannotActivate, CannotBeDefeated]]

instance HasTraits Minion where
  getTraits m = (cdTraits (getCardDef m) <>) <$> getGainedTraits m

instance IsRef Minion where
  toRef = MinionRef . toId

instance IsCard Minion where
  toCard = toCard . toAttrs

instance HasCardDef Minion where
  getCardDef = getCardDef . toAttrs

minionRemainingHitPoints :: Attrs Minion -> Natural
minionRemainingHitPoints attrs =
  subtractNatural (minionDamage attrs) (unHp $ minionHitPoints attrs)

defensePriorityL :: Lens' (Attrs Minion) DefensePriority
defensePriorityL =
  lens minionDefensePriority $ \m x -> m {minionDefensePriority = x}

damageL :: Lens' (Attrs Minion) Natural
damageL = lens minionDamage $ \m x -> m {minionDamage = x}

attackingL :: Lens' (Attrs Minion) (Maybe Attack)
attackingL = lens minionAttacking $ \m x -> m {minionAttacking = x}

stunnedL :: Lens' (Attrs Minion) Bool
stunnedL = lens minionStunned $ \m x -> m {minionStunned = x, minionAdditionalStun = x && minionAdditionalStun m}

confusedL :: Lens' (Attrs Minion) Bool
confusedL = lens minionConfused $ \m x -> m {minionConfused = x, minionAdditionalConfuse = x && minionAdditionalConfuse m}

toughL :: Lens' (Attrs Minion) Bool
toughL = lens minionTough $ \m x -> m {minionTough = x, minionAdditionalTough = if x then minionAdditionalTough m else 0}

upgradesL :: Lens' (Attrs Minion) (HashSet UpgradeId)
upgradesL = lens minionUpgrades $ \m x -> m {minionUpgrades = x}

facedownCardL :: Lens' (Attrs Minion) (Maybe PlayerCard)
facedownCardL = lens minionFacedownCard $ \m x -> m {minionFacedownCard = x}

boostCardsL :: Lens' (Attrs Minion) [EncounterCard]
boostCardsL = lens minionBoostCards $ \m x -> m {minionBoostCards = x}

boostL :: Lens' (Attrs Minion) Natural
boostL = lens minionBoost $ \m x -> m {minionBoost = x}

attachmentsL :: Lens' (Attrs Minion) (HashSet AttachmentId)
attachmentsL = lens minionAttachments $ \m x -> m {minionAttachments = x}

instance HasCardCode (Attrs Minion) where
  toCardCode = toCardCode . minionCardDef

minionWith ::
  (Attrs Minion -> a) ->
  CardDef ->
  Sch ->
  Atk ->
  HP Natural ->
  (Attrs Minion -> Attrs Minion) ->
  CardBuilder (IdentityId, MinionId) a
minionWith f cardDef sch atk hp g = minion (f . g) cardDef sch atk hp

minion ::
  (Attrs Minion -> a) ->
  CardDef ->
  Sch ->
  Atk ->
  HP Natural ->
  CardBuilder (IdentityId, MinionId) a
minion f cardDef sch atk hp =
  CardBuilder
    { cbCardDef = cardDef
    , cbCardBuilder = \cardDef (ident, mid) ->
        f $
          MinionAttrs
            { minionId = mid
            , minionCardDef = cardDef
            , minionOriginalCardDef = cardDef
            , minionExhausted = False
            , minionCounters = 0
            , minionDamage = 0
            , minionIgnoreWhenDefeated = False
            , minionDamageThisPhase = 0
            , minionAttack = atk
            , minionSchemeAvailable = True
            , minionAttackAvailable = True
            , minionScheme = sch
            , minionDefeatedBaseScheme = Nothing
            , minionHitPoints = hp
            , minionEngagedIdentity = ident
            , minionStunned = False
            , minionConfused = False
            , minionAdditionalStun = False
            , minionAdditionalConfuse = False
            , minionTough = False
            , minionAdditionalTough = 0
            , minionAttacking = Nothing
            , minionUpgrades = mempty
            , minionAttachments = mempty
            , minionDefensePriority = AnyDefense
            , minionFacedownCard = Nothing
            , minionOriginalAlly = Nothing
            , minionOriginalCard = Nothing
            , minionAttachedTo = Nothing
            , minionSchemeReplacesAttack = False
            , minionSchemeRemoval = Nothing
            , minionActivationPlayer = Nothing
            , minionAttackTarget = Nothing
            , minionBoostCards = []
            , minionHeldCards = []
            , minionHeldTreacheries = []
            , minionBoost = 0
            }
    }

instance IsRef (Attrs Minion) where
  toRef = MinionRef . minionId

-- | A facedown minion is its owner's player card, so it goes to their discard pile when
-- it leaves play.
instance IsCard (Attrs Minion) where
  toCard a | Just card <- minionOriginalCard a = card
  toCard a | Just card <- minionFacedownCard a = PlayerCard card
  toCard a =
    EncounterCard $
      MkEncounterCard
        { ecCardId = CardId $ unMinionId $ minionId a
        , ecCardDef = getCardDef a
        }

instance HasCardDef (Attrs Minion) where
  getCardDef = minionCardDef

getModifiedKeywords :: (HasGame m) => Attrs Minion -> m [Keyword]
getModifiedKeywords attrs = do
  modifiers <- getModifiers attrs
  blanked <- isBlanked attrs
  pure $ foldr applyModifier (if blanked then [] else toList . cdKeywords $ getCardDef attrs) modifiers
 where
  applyModifier (LoseKeyword k) = filter (/= k)
  applyModifier (KeywordModifier k) = (k :)
  applyModifier _ = id

-- | Older saves predate the second status cards a Steady minion can hold.
instance FromJSON (Attrs Minion) where
  parseJSON = withObject "MinionAttrs" \o ->
    genericParseJSON defaultOptions $ Object $
      KeyMap.insertWith (\_ old -> old) "minionSchemeReplacesAttack" (Bool False) $ KeyMap.insertWith (\_ old -> old) "minionSchemeRemoval" (toJSON (Nothing :: Maybe IdentityId)) $ KeyMap.insertWith (\_ old -> old) "minionActivationPlayer" (toJSON (Nothing :: Maybe IdentityId)) $ KeyMap.union o $ KeyMap.fromList
      [("minionOriginalAlly", Null), ("minionOriginalCard", Null), ("minionCounters", Number 0), ("minionSchemeAvailable", Bool True), ("minionAttackAvailable", Bool True), ("minionDefeatedBaseScheme", toJSON (Nothing :: Maybe Natural)), ("minionIgnoreWhenDefeated", Bool False), ("minionAttachedTo", toJSON (Nothing :: Maybe MinionId)), ("minionExhausted", Bool False), ("minionDamageThisPhase", Number 0), ("minionHeldTreacheries", toJSON ([] :: [EncounterCard])), ("minionHeldCards", toJSON ([] :: [PlayerCard])), ("minionAdditionalTough", Number 0), ("minionAdditionalStun", Bool False), ("minionAdditionalConfuse", Bool False)]

-- | Having a status card is distinct from being stunned or confused (RR Steady).
isStunned, isConfused :: (HasGame m) => Attrs Minion -> m Bool
isStunned attrs = do
  steady <- elem Steady <$> getModifiedKeywords attrs
  pure $ minionStunned attrs && (not steady || minionAdditionalStun attrs)
isConfused attrs = do
  steady <- elem Steady <$> getModifiedKeywords attrs
  pure $ minionConfused attrs && (not steady || minionAdditionalConfuse attrs)

getModifiedHitPoints :: (HasGame m) => Attrs Minion -> m Natural
getModifiedHitPoints attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unHp $ minionHitPoints attrs) modifiers
 where
  applyModifier (HitPointModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

getModifiedAttack :: (HasGame m) => Attrs Minion -> m Natural
getModifiedAttack attrs = (fromInteger . max 0) <$> getModifiedAttackSigned attrs

-- | Keep the signed intermediate while an activation still has boosts and
-- attack-specific modifiers to add (RR Modifiers).
getModifiedAttackSigned :: HasGame m => Attrs Minion -> m Integer
getModifiedAttackSigned attrs = do
  modifiers <- getModifiers attrs
  let base = fromMaybe (unAtk $ minionAttack attrs) (headMay [n | BaseAttack n <- modifiers])
  pure $ toInteger base + sum (map change modifiers)
 where
  change (AttackModifier n) = toInteger n
  change (AttackPenalty n) = negate (toInteger n)
  change (AttackReductionWhileAttacking you n)
    | Just (IdentityCharacter you) == (attackCharacter <$> minionAttacking attrs) = negate (toInteger n)
  change _ = 0

toEnemyId :: Attrs Minion -> EnemyId
toEnemyId = EnemyMinionId . minionId

-- | A star defined by the card establishes its base before additive modifiers.
minionBaseScheme :: HasGame m => Attrs Minion -> m Natural
minionBaseScheme attrs = do
  modifiers <- getModifiers attrs
  pure $ fromMaybe (unSch $ minionScheme attrs) (headMay [n | BaseScheme n <- modifiers])

getModifiedScheme :: (HasGame m) => Attrs Minion -> m Natural
getModifiedScheme attrs = do
  modifiers <- getModifiers attrs
  let base = fromMaybe (unSch $ minionScheme attrs) (headMay [n | BaseScheme n <- modifiers])
  pure $ fromInteger $ max 0 $ toInteger base + sum (map change modifiers)
 where
  change (SchemeModifier n) = toInteger n
  change (SchemePenalty n) = negate (toInteger n)
  change _ = 0

getMinionDisplayCounters :: Minion -> Natural
getMinionDisplayCounters (Minion a) = minionDisplayCounters a

incomingMinionDamage :: HasGame m => Attrs Minion -> Source -> Damage -> m Damage
incomingMinionDamage attrs source originalDamage = do
  modifiers <- getModifiers attrs
  sourceDef <- getsGame (`abilityCardDef` refocus source)
  attackerTraits <- if any (\case AttackDamageReductionUnlessAttackerHasTrait _ _ -> True; _ -> False) modifiers
    then case damageSource originalDamage of
      FromPlayerAttack you -> getCharacterTraits (IdentityCharacter you)
      FromAllyAttack ally -> getCharacterTraits (AllyCharacter ally)
      _ -> pure mempty
    else pure mempty
  let resources = maybe [] printedResources sourceDef
      multipliers = [n | DamageMultiplierFromResource resource n <- modifiers, resource `elem` resources]
        <> [n | AttackDamageMultiplierWithKeyword Piercing n <- modifiers, damagePiercing originalDamage]
      bonus = sum [n | DamageBonusFromResource resource n <- modifiers, resource `elem` resources]
      barred = any (`notElem` resources) [resource | CannotTakeDamageExceptFromResource resource <- modifiers]
      adjusted = if barred then 0 else damageAmount originalDamage * product multipliers + if damageAmount originalDamage > 0 then bonus else 0
      attacking = case damageSource originalDamage of
        FromPlayerAttack _ -> True
        FromAllyAttack _ -> True
        FromAttack -> True
        _ -> False
      taken = sum [n | DamageTakenModifier n <- modifiers]
        + if attacking then sum [n | AttackDamageTakenModifier n <- modifiers] else 0
      incoming = if adjusted == 0 then 0 else fromIntegral (max 0 (fromIntegral adjusted + taken))
      reduction = if attacking then sum [n | AttackDamageReduction n <- modifiers] + sum [n | AttackDamageReductionUnlessAttackerHasTrait trait n <- modifiers, not (member trait attackerTraits)] else 0
      reduced = subtractNatural reduction incoming
      caps = [cap | attacking, AttackDamageCap cap <- modifiers]
      capped = minimum (reduced : caps)
      room = minimum (capped : [subtractNatural (minionDamageThisPhase attrs) cap | MaximumDamagePerPhase cap <- modifiers])
      damage = if damageUnpreventable originalDamage
        then originalDamage {damageAmount = max (damageAmount originalDamage) incoming}
        else if minionTough attrs && preventedByTough originalDamage then originalDamage else originalDamage {damageAmount = subtractNatural (damagePrevention originalDamage) room}
  pure damage

getMinionAccelerationTokens :: Minion -> Natural
getMinionAccelerationTokens (Minion a) = minionAccelerationTokens a
