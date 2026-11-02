module Marvel.Villain.Types (
  module Marvel.Villain.Types,
  module X,
) where

import Marvel.Prelude
import Marvel.Message ()
import Control.Monad (replicateM_)

import Data.HashSet qualified as HashSet
import Data.Aeson ()
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Typeable
import Marvel.Ability.Types
import Marvel.Attack
import Marvel.Boost ()
import Marvel.Card
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hp
import Marvel.Id hiding (VillainId)
import Marvel.Id as X (VillainId)
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted hiding (VillainConfused, VillainStunned)
import Marvel.Message qualified as Msg
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Stats
import Marvel.Trait
import Marvel.Window qualified as W

data Villain = forall a. (IsVillain a) => Villain a

instance Show Villain where
  show (Villain a) = show a

instance ToJSON Villain where
  toJSON (Villain a) = toJSON a

instance Eq Villain where
  Villain (a :: a) == Villain (b :: b) = case eqT @a @b of
    Just Refl -> a == b
    Nothing -> False

data SomeVillainCard
  = forall a.
    (IsVillain a) =>
    SomeVillainCard
      (VillainCard a)

liftVillainCard :: (forall a. VillainCard a -> b) -> SomeVillainCard -> b
liftVillainCard f (SomeVillainCard a) = f a

someVillainCardCode :: SomeVillainCard -> CardCode
someVillainCardCode = liftVillainCard toCardCode

villainDamage :: Villain -> Natural
villainDamage v =
  fromIntegral . max 0 $
    unHp (villainMaxHp attrs)
      - unHp
        (villainHp attrs)
 where
  attrs = toAttrs v

villainIsTough :: Villain -> Bool
villainIsTough = villainTough . toAttrs

villainAttackDetails :: Villain -> Maybe Attack
villainAttackDetails = villainAttacking . toAttrs

instance HasAbilities Villain where
  getAbilities (Villain a) = getAbilities a

instance HasModifiersFor Villain where
  getBlankingModifiersFor source target (Villain a) = getBlankingModifiersFor source target a
  getModifiersFor source target (Villain a) = getModifiersFor source target a

instance RunMessage Villain where
  runMessage msg v@(Villain a) = runQueueT $ case msg of
    -- Resume the original declaration through its concrete villain, so that
    -- scenario-specific replacements and interrupts still see Attacks/Schemes.
    CardSpecific target "activationAfterWould" (fromJSON -> Success declaration)
      | target == VillainRef vid -> Villain <$> runMessage (VillainMessage vid declaration) a
    VillainMessage target declaration
      | target == vid, Just recipient <- activationRecipient declaration -> do
          you <- maybe getActivePlayerId pure recipient
          checkWindows [W.Window W.Would $ W.VillainActivationWouldBegin vid you]
          resolveVillainActivationAfterWould vid declaration
          pure v
    _ -> Villain <$> runMessage msg a
   where
    vid = toId v
    activationRecipient = \case
      VillainAttacks you -> Just (Just you)
      VillainAttacksWithoutBoost you -> Just (Just you)
      VillainAttacksGaining you _ -> Just (Just you)
      VillainAttacksWithAdditionalBoostCards you _ -> Just (Just you)
      VillainAttacksAlly you _ -> Just (Just you)
      VillainAttacksCharacter you _ -> Just (Just you)
      VillainAttacksCharacterGaining you _ _ -> Just (Just you)
      VillainSchemesAgainst you -> Just (Just you)
      VillainSchemes -> Just Nothing
      _ -> Nothing

instance HasCardDef Villain where
  getCardDef = getCardDef . toAttrs

instance Entity Villain where
  type Id Villain = VillainId
  data Attrs Villain = VillainAttrs
    { villainId :: VillainId
    , villainCardDef :: CardDef
    , villainCounters :: Natural
    , villainHp :: HP Int
    , villainStartingHp :: HP GameValue
    , villainMaxHp :: HP Int
    , villainScheme :: Sch
    , villainAttack :: Atk
    , villainStunned :: Bool
    , villainConfused :: Bool
    , villainAdditionalStun :: Bool
    , villainAdditionalConfuse :: Bool
    , villainTough :: Bool
    , villainBoostCards :: [EncounterCard]
    , villainBoost :: Natural
    , villainPendingAttackBoostCards :: Natural
    , villainSchemeReplacesAttack :: Bool
    , villainSchemeWithoutBoost :: Bool
    , villainPendingSchemeBoostCards :: Natural
    , villainAttackAdditionalBoostCards :: Natural
    , villainAttackGrantedKeywords :: [Keyword]
    , villainAttacking :: Maybe Attack
    , villainAttachments :: HashSet AttachmentId
    , villainUpgrades :: HashSet UpgradeId
    , villainStage :: Natural
    , villainAttackTarget :: Maybe CharacterId
    -- ^ the character its coming attack is against, when not the player's identity
    , villainActivationPlayer :: Maybe IdentityId
    -- ^ the player its activation under way is against, who resolves its boost cards
    , villainSchemeRemoval :: Maybe (IdentityId, Source)
    , villainPreventedAttack :: Maybe Source
    -- ^ the attack under way, by what makes it, all of whose damage to him is prevented
    -- until it ends (Master of Illusions)
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass ToJSON
  data Field Villain :: Type -> Type where
    VillainId :: Field Villain VillainId
    VillainCardDef :: Field Villain CardDef
    VillainCounters :: Field Villain Natural
    VillainHp :: Field Villain (HP Int)
    VillainStartingHp :: Field Villain (HP GameValue)
    VillainMaxHp :: Field Villain (HP Int)
    VillainScheme :: Field Villain Sch
    VillainAttack :: Field Villain Atk
    VillainStunned :: Field Villain Bool
    VillainConfused :: Field Villain Bool
    VillainTough :: Field Villain Bool
    VillainBoostCards :: Field Villain [EncounterCard]
    VillainBoost :: Field Villain Natural
    VillainAttacking :: Field Villain (Maybe Attack)
    VillainAttachments :: Field Villain (HashSet AttachmentId)
    VillainUpgrades :: Field Villain (HashSet UpgradeId)
    VillainStage :: Field Villain Natural
  field fld v =
    let VillainAttrs {..} = toAttrs v
     in case fld of
          VillainId -> villainId
          VillainCardDef -> villainCardDef
          VillainCounters -> villainCounters
          VillainHp -> villainHp
          VillainStartingHp -> villainStartingHp
          VillainMaxHp -> villainMaxHp
          VillainScheme -> villainScheme
          VillainAttack -> villainAttack
          VillainStunned -> villainStunned
          VillainConfused -> villainConfused
          VillainTough -> villainTough
          VillainBoostCards -> villainBoostCards
          VillainBoost -> villainBoost
          VillainAttacking -> villainAttacking
          VillainAttachments -> villainAttachments
          VillainUpgrades -> villainUpgrades
          VillainStage -> villainStage
  toId = villainId . toAttrs
  toAttrs (Villain a) = toVillainAttrs a

class (Typeable a, Show a, Eq a, ToJSON a, FromJSON a, RunMessage a, HasAbilities a, HasModifiersFor a) => IsVillain a where
  toVillainAttrs :: a -> Attrs Villain
  default toVillainAttrs :: (Coercible a (Attrs Villain)) => a -> Attrs Villain
  toVillainAttrs = coerce
  overVillainAttrs :: (Attrs Villain -> Attrs Villain) -> a -> a
  default overVillainAttrs :: (Coercible a (Attrs Villain)) => (Attrs Villain -> Attrs Villain) -> a -> a
  overVillainAttrs f = coerce f

type VillainCard a = CardBuilder VillainId a

villainWith ::
  (Attrs Villain -> a) ->
  CardDef ->
  Sch ->
  Atk ->
  HP GameValue ->
  (Attrs Villain -> Attrs Villain) ->
  VillainCard a
villainWith f cardDef sch atk startingHp g =
  villain (f . g) cardDef sch atk startingHp

villain ::
  (Attrs Villain -> a) ->
  CardDef ->
  Sch ->
  Atk ->
  HP GameValue ->
  VillainCard a
villain f cardDef sch atk startingHp =
  CardBuilder
    { cbCardDef = cardDef
    , cbCardBuilder = \cardDef ident ->
        f $
          VillainAttrs
            { villainId = ident
            , villainCardDef = cardDef
            , villainStartingHp = startingHp
            , villainMaxHp = HP 1
            , villainCounters = 0
            , villainHp = HP 1
            , villainScheme = sch
            , villainAttack = atk
            , villainStunned = False
            , villainConfused = False
            , villainAdditionalStun = False
            , villainAdditionalConfuse = False
            , villainTough = False
            , villainBoostCards = mempty
            , villainBoost = 0
            , villainPendingAttackBoostCards = 0
            , villainSchemeReplacesAttack = False
            , villainSchemeWithoutBoost = False
            , villainPendingSchemeBoostCards = 0
            , villainAttackAdditionalBoostCards = 0
            , villainAttackGrantedKeywords = []
            , villainAttacking = Nothing
            , villainAttachments = mempty
            , villainUpgrades = mempty
            , villainStage = 1
            , villainAttackTarget = Nothing
            , villainActivationPlayer = Nothing
            , villainSchemeRemoval = Nothing
            , villainPreventedAttack = Nothing
            }
    }

-- makeLensesWith suffixedFields ''VillainAttrs

stageL :: Lens' (Attrs Villain) Natural
stageL = lens villainStage $ \m x -> m {villainStage = x}

hpL :: Lens' (Attrs Villain) (HP Int)
hpL = lens villainHp $ \m x -> m {villainHp = x}

maxHpL :: Lens' (Attrs Villain) (HP Int)
maxHpL = lens villainMaxHp $ \m x -> m {villainMaxHp = x}

upgradesL :: Lens' (Attrs Villain) (HashSet UpgradeId)
upgradesL = lens villainUpgrades $ \m x -> m {villainUpgrades = x}

attachmentsL :: Lens' (Attrs Villain) (HashSet AttachmentId)
attachmentsL = lens villainAttachments $ \m x -> m {villainAttachments = x}

toughL :: Lens' (Attrs Villain) Bool
toughL = lens villainTough $ \m x -> m {villainTough = x}

stunnedL :: Lens' (Attrs Villain) Bool
stunnedL = lens villainStunned $ \m x -> m {villainStunned = x, villainAdditionalStun = x && villainAdditionalStun m}

confusedL :: Lens' (Attrs Villain) Bool
confusedL = lens villainConfused $ \m x -> m {villainConfused = x, villainAdditionalConfuse = x && villainAdditionalConfuse m}

attackingL :: Lens' (Attrs Villain) (Maybe Attack)
attackingL = lens villainAttacking $ \m x -> m {villainAttacking = x}

boostL :: Lens' (Attrs Villain) Natural
boostL = lens villainBoost $ \m x -> m {villainBoost = x}

boostCardsL :: Lens' (Attrs Villain) [EncounterCard]
boostCardsL = lens villainBoostCards $ \m x -> m {villainBoostCards = x}

instance HasCardDef (Attrs Villain) where
  getCardDef = villainCardDef

statusL :: StatusCard -> Lens' (Attrs Villain) Bool
statusL = \case
  StunnedStatus -> stunnedL
  ConfusedStatus -> confusedL
  ToughStatus -> toughL

toEnemyId :: Attrs Villain -> EnemyId
toEnemyId = EnemyVillainId . villainId

-- | Its printed traits and those it gains, as Absorbing Man gains each environment's.
instance HasTraits Villain where
  getTraits v = (cdTraits (getCardDef v) <>) <$> getGainedTraits (VillainRef $ toId v)

-- | The status card is given, a stunned or confused one not to a stalwart villain (RR
-- Stalwart).
statusUnlessStalwart :: (HasGame m) => Attrs Villain -> StatusCard -> m (Attrs Villain)
statusUnlessStalwart attrs s = do
  keywords <- getModifiedKeywords attrs
  modifiers <- getModifiers attrs
  pure $ if (Stalwart `elem` keywords && s /= ToughStatus) || (s == StunnedStatus && CannotBeStunned `elem` modifiers) || (s == ConfusedStatus && CannotBeConfused `elem` modifiers) then attrs else
    case s of
      StunnedStatus | Steady `elem` keywords && villainStunned attrs -> attrs {villainAdditionalStun = True}
      ConfusedStatus | Steady `elem` keywords && villainConfused attrs -> attrs {villainAdditionalConfuse = True}
      _ -> attrs & statusL s .~ True

isStunned, isConfused :: (HasGame m) => Attrs Villain -> m Bool
isStunned a = do
  steady <- elem Steady <$> getModifiedKeywords a
  pure $ villainStunned a && (not steady || villainAdditionalStun a)
isConfused a = do
  steady <- elem Steady <$> getModifiedKeywords a
  pure $ villainConfused a && (not steady || villainAdditionalConfuse a)


getModifiedKeywords :: (HasGame m) => Attrs Villain -> m [Keyword]
getModifiedKeywords attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (toList . cdKeywords $ getCardDef attrs) modifiers
 where
  applyModifier (LoseKeyword k) = filter (/= k)
  applyModifier (KeywordModifier k) = (k :)
  applyModifier _ = id

runVillainMessage :: (ReverseQueue m) => VillainMessage -> Attrs Villain -> m (Attrs Villain)
runVillainMessage msg attrs = case msg of
  VillainAdvanced -> do
    villainStep vid SetVillainHp
    checkWindows [W.Window W.When $ W.RevealVillain vid W.RevealedFromVillain]
    pure attrs
  SetVillainHp -> do
    hp <- fromGameValue (unHp $ villainStartingHp attrs)
    modifiers <- getModifiers attrs
    let hp' = hp + sum [fromIntegral n | HitPointModifier n <- modifiers]
    -- a stage with toughness enters play with a tough status card (RR Toughness)
    let toughness = Toughness `elem` cdKeywords (getCardDef attrs)
    pure $ attrs & hpL .~ HP hp' & maxHpL .~ HP hp' & toughL %~ (|| toughness)
  VillainHitPointsChanged n -> do
    let remaining = unHp (villainHp attrs) + n
    modifiers <- getModifiers attrs
    let unbeaten = InfiniteHitPoints `elem` modifiers || CannotBeDefeated `elem` modifiers
    when (remaining <= 0 && not unbeaten) $ villainStep vid (VillainDefeatPending Nothing)
    pure $ attrs & hpL .~ HP (max 0 remaining) & maxHpL %~ HP . max 0 . (+ n) . unHp
  VillainHealed n -> do
    pure $
      attrs
        & hpL
          %~ HP
            . min (unHp $ villainMaxHp attrs)
            . (+ fromIntegral n)
            . unHp
  -- retaliate answers the attack whether or not a tough status card took its damage
  VillainRetaliates attacker -> do
    keywords <- getModifiedKeywords attrs
    let amount = sum [n | Retaliate n <- keywords]
    when (amount > 0) $ retaliate attrs attacker amount
    pure attrs
  VillainDamaged source dealt -> do
    modifiers <- getModifiers attrs
    -- a tough status card prevents the damage before anything else that answers "would
    -- take damage" (RR Damage); what gets past it is reduced (Thanos's Armor)
    let reduced = reducedDamage modifiers dealt
        sustained = subtractNatural (fromIntegral $ unHp $ villainHp attrs) (fromIntegral $ unHp $ villainMaxHp attrs)
        room = minimum (damageAmount reduced : [subtractNatural sustained cap | MaximumSustainedDamage cap <- modifiers])
        damage = if damageUnpreventable dealt then reduced else if villainTough attrs && preventedByTough dealt then dealt else reduced {damageAmount = subtractNatural (damagePrevention dealt) room}
    -- none at all (The Red House), or none from a player card (Sanctuary)
    cannotTakeDamage <- (||) <$> cannotTakeDamageFrom (toRef attrs) source <*> (not <$> canDamageThroughAerialRestriction (toRef attrs) source dealt)
    let -- infinite hit points: the damage is dealt, but he is not defeated by it
        infinite = InfiniteHitPoints `elem` modifiers
        -- "cannot be defeated" (Tower Defense): the damage is dealt, to 0 hit points, but
        -- he is not defeated, nor is any of it excess
        held = CannotBeDefeated `elem` modifiers
    if
      | cannotTakeDamage && not (damageUnpreventable dealt) -> pure attrs
      | preventedAttackDamage attrs source damage && not (damageUnpreventable dealt) -> pure attrs
      | villainTough attrs && preventedByTough dealt -> pure $ attrs & toughL .~ False
      | otherwise -> do
        when (damageAmount damage > 0) $ do
          recordPlayerAttackDamage (toEnemyId attrs) source damage
          attackDealtDamageTo source damage vid
          checkWindows [W.Window W.After $ W.EnemyTookDamageFrom (toEnemyId attrs) damage source]
        when
          ( not infinite && not held && subtractNatural
              (damageAmount damage)
              (fromIntegral . unHp $ villainHp attrs)
              == 0
          )
          do
            let excess = subtractNatural (fromIntegral . unHp $ villainHp attrs) (damageAmount damage)
            -- "when your hero's attack deals excess damage, increase that amount": it
            -- raises the excess the window after the defeat counts ('increaseExcessDamage')
            hero <- heroAttackBy damage source
            for_ hero \ident ->
              when (excess > 0) $ checkWindows [W.Window W.When $ W.ExcessDamageDealt ident (EnemyVillainId vid) excess]
            villainStep vid (VillainDefeatPending $ Just (source, damage, excess))
        pure $
          attrs
            & toughL %~ (&& not (piercesTough dealt))
            & hpL
              %~ HP
                . max 0
                . subtract (if infinite then 0 else fromIntegral $ damageAmount damage)
                . unHp
  -- the last stage defeated: the scenario says what follows, by default the players win
  -- with no villain left
  VillainDefeated -> do
    resolveVillainDefeat vid
    pure attrs
  VillainDefeatPending details -> do
    checkWindows [W.Window W.Would $ W.VillainDefeatWouldOccur vid]
    villainStep vid (VillainResolveDefeat (toCardCode $ getCardDef attrs) details)
    pure attrs
  VillainResolveDefeat original details | original == toCardCode (getCardDef attrs) -> do
    for_ details $ \(source, damage, excess) -> do
      attached <- enemyDefeatUpgrades (toEnemyId attrs)
      checkWindows [W.Window W.When $ W.DefeatedVillain vid damage source excess, W.Window W.When $ W.DefeatedEnemyWithUpgrades (toEnemyId attrs) attached damage source]
    villainStep vid VillainWouldBeDefeated
    for_ details $ \(source, damage, excess) -> do
      attached <- enemyDefeatUpgrades (toEnemyId attrs)
      checkWindows [W.Window W.After $ W.DefeatedVillain vid damage source excess, W.Window W.After $ W.DefeatedEnemyWithUpgrades (toEnemyId attrs) attached damage source]
    pure attrs
  VillainResolveDefeat _ _ -> pure attrs
  VillainWouldBeDefeated -> attrs <$ defeatVillain vid
  VillainBecomeTough -> pure $ attrs & toughL .~ True
  VillainPreventAttackDamage source -> pure attrs {villainPreventedAttack = Just source}
  -- a stalwart villain cannot be stunned or confused (RR Stalwart)
  Msg.VillainStunned _ -> beginVillainStatus attrs StunnedStatus
  Msg.VillainConfused _ -> beginVillainStatus attrs ConfusedStatus
  VillainSchemes -> do
    confused <- isConfused attrs
    if confused
      then pure $ attrs {villainActivationPlayer = Nothing} & confusedL .~ False & boostL .~ 0
      else do
        player <- maybe getActivePlayerId pure (villainActivationPlayer attrs)
        checkWindows [W.Window W.Would $ W.EnemySchemeBegins (toEnemyId attrs) player]
        villainStep vid VillainBeginScheme
        pure attrs
  -- queued after the scheme, everything it leads to runs before the player is forgotten,
  -- whether the scheme resolved, was replaced or was cancelled
  VillainSchemesWithBonus ident n -> do
    villainStep vid (VillainSchemesAgainst ident)
    cardSpecific attrs "clearSchemeBonus" n
    pure attrs {villainScheme = Sch (unSch (villainScheme attrs) + n)}
  VillainSchemesAgainst ident -> do
    resolveVillainActivationAfterWould vid VillainSchemes
    villainStep vid VillainSchemeAgainstEnded
    pure attrs {villainActivationPlayer = Just ident}
  VillainSchemeAgainstEnded -> pure attrs {villainActivationPlayer = Nothing, villainSchemeReplacesAttack = False, villainSchemeRemoval = Nothing, villainSchemeWithoutBoost = False, villainPendingSchemeBoostCards = 0, villainPendingAttackBoostCards = 0}
  VillainBeginScheme -> do
    -- against the player it was said to be against, or the active player
    player <- maybe getActivePlayerId pure (villainActivationPlayer attrs)
    -- the scheme is his activation against the player whose alter-ego activates him
    unless (villainSchemeReplacesAttack attrs) $ checkWindows [W.Window W.When $ W.EnemyActivation (toEnemyId attrs) player]
    checkWindows [W.Window W.When $ W.Schemed vid]
    modifiers <- getModifiers attrs
    unless (villainSchemeWithoutBoost attrs || SchemesWithoutBoost `elem` modifiers) dealActivationBoost
    replicateM_ (fromIntegral $ villainPendingSchemeBoostCards attrs) (dealBoost attrs)
    villainStep vid VillainFlipBoostCards
    villainStep vid VillainSchemed
    clearBoosts
    checkWindows [W.Window W.After $ W.EnemyActivation (toEnemyId attrs) player]
    pure attrs {villainActivationPlayer = Just player}
  -- "attacks an ally": the player who controls it is attacked
  VillainAttacksCharacterGaining ident character keywords -> do
    resolveVillainActivationAfterWould vid (VillainAttacksGaining ident keywords)
    pure attrs {villainAttackTarget = Just character}
  VillainAttacksCharacter ident character -> do
    resolveVillainActivationAfterWould vid (VillainAttacks ident)
    pure attrs {villainAttackTarget = Just character}
  VillainAttacksAlly ident allyId -> do
    resolveVillainActivationAfterWould vid (VillainAttacks ident)
    pure attrs {villainAttackTarget = Just (AllyCharacter allyId)}
  VillainAttacksWithAdditionalBoostCards ident n -> do
    -- Dispatch the ordinary declaration so card-specific activation interrupts still run.
    resolveVillainActivationAfterWould vid (VillainAttacks ident)
    pure attrs {villainPendingAttackBoostCards = n}
  VillainSetAttackAdditionalBoostCards n -> pure attrs {villainAttackAdditionalBoostCards = n}
  VillainAttacks ident -> attacks (VillainBeginAttack ident) ident []
  VillainAttacksWithoutBoost ident -> attacks (VillainBeginAttackWithoutBoost ident) ident []
  VillainAttacksGaining ident keywords -> attacks (VillainBeginAttack ident) ident keywords
  VillainBeginAttack ident -> beginAttack True ident
  VillainBeginAttackWithoutBoost ident -> beginAttack False ident
  VillainEndAttack -> pure $ attrs {villainActivationPlayer = Nothing, villainAttackGrantedKeywords = [], villainAttackTarget = Nothing, villainPendingAttackBoostCards = 0, villainPendingSchemeBoostCards = 0, villainAttackAdditionalBoostCards = 0} & attackingL .~ Nothing
  VillainAttackGains k -> pure $ attrs {villainAttackGrantedKeywords = villainAttackGrantedKeywords attrs <> [k]} & attackingL . _Just %~ gainKeyword k
  VillainAttackChanged n -> pure $ attrs & attackingL . _Just %~ \a -> a {attackChange = attackChange a + n}
  VillainSchemeRemovesThreat you source -> pure attrs {villainSchemeRemoval = Just (you, source)}
  VillainAttackDamageRedirected enemy -> pure $ attrs & attackingL . _Just %~ (\a -> a {attackDamageRedirect = Just enemy})
  VillainAttackBecomesIndirect -> pure $ attrs & attackingL . _Just %~ (\attack -> attack {attackIndirect = True})
  VillainAttackDamagePrevented -> pure $ attrs & attackingL . _Just %~ \a -> a {attackPrevented = True}
  VillainDefendedBy characterId -> do
    forbidden <- elem AttacksCannotBeDefended <$> getModifiers attrs
    pure $ if forbidden then attrs else attrs & attackingL . _Just %~ defendedBy characterId
  -- onto his own side scheme, when he has one, or his own main scheme (Tower Defense),
  -- or else the main scheme
  VillainSchemed -> do
    own <- selectOne (SignatureSchemeOf $ VillainWithId vid)
    paired <- getPairedMainScheme vid
    scheme <- maybe (selectOne EncounterMainScheme) (pure . Just) $ (SchemeSideSchemeId <$> own) <|> (SchemeMainSchemeId <$> paired)
    sch <- getModifiedScheme attrs
    for_ scheme \s -> case villainSchemeRemoval attrs of
      Nothing -> placeVillainSchemeThreat vid s (sch + villainBoost attrs)
      Just (you, source) -> thwartSchemeAs you s source (sch + villainBoost attrs)
    cardSpecific attrs "villainSchemeResult" (0 :: Natural)
    pure $ attrs {villainActivationPlayer = Nothing, villainSchemeRemoval = Nothing} & boostL .~ 0
  VillainAttacked -> do
    case villainAttacking attrs of
      Nothing -> error "No current attack"
      Just attack0 -> do
        actual <- undefendedIfDefenderGone attack0
        base <- getModifiedAttackSigned (attrs {villainAttacking = Just actual})
        keywords <- getModifiedKeywords attrs
        let current = keywords <> villainAttackGrantedKeywords attrs
            refreshed = (withAttackBase (base + toInteger (villainBoost attrs)) actual) {
              attackOverkill = Overkill `elem` current, attackPiercing = Piercing `elem` current,
              attackRanged = Ranged `elem` current}
        attack' <- undefendedIfDefenderGone refreshed
        attackedYou (attackEnemy attack') (attackCharacter attack')
        wasAttacked attack'
        pure $ attrs & boostL .~ 0 & attackingL ?~ attack'
  VillainDealtBoost c -> do
    checkWindows [W.Window W.After $ W.FacedownBoostGiven (EnemyVillainId $ villainId attrs) c]
    pure $ attrs & boostCardsL %~ (c :)
  AttachedToVillain attachmentId -> do
    pure $ attrs & attachmentsL %~ HashSet.insert attachmentId
  AttachedUpgradeToVillain upgradeId -> do
    pure $ attrs & upgradesL %~ HashSet.insert upgradeId
  -- each boost card is turned face up in turn, and its icons counted and added, which
  -- an interrupt to its turning face up may cancel
  VillainFlipBoostCards -> do
    for_ (villainBoostCards attrs) \card -> do
      checkWindows [W.Window W.When $ W.FlippedBoostCard card (toEnemyId attrs)]
      ignored <- elem IgnoreBoostCards <$> getModifiers attrs
      if ignored
        then revealBoostCardWithoutAbility card vid
        else do
          revealBoostCard card vid
          countActivationBoostIcons attrs [card]
    villainStep vid VillainCheckAdditionalBoosts
    pure $ attrs & (boostCardsL .~ mempty)
  VillainActivationBonus n -> pure $ attrs & boostL +~ n
  VillainAddBoost n ->
    pure $
      attrs
        & (boostL +~ n)
        & (attackingL . _Just . attackDamageL +~ n)
  VillainCheckAdditionalBoosts -> do
    unless (null $ villainBoostCards attrs) $ villainStep vid VillainFlipBoostCards
    pure attrs
  -- a double-sided villain answers this itself, with flipVillainTo
  FlipVillain -> error $ "No other face for villain " <> show (toCardCode $ villainCardDef attrs)
 where
  vid = villainId attrs
  attacks begin ident keywords = do
    stunned <- isStunned attrs
    if stunned
      then pure $ attrs & stunnedL .~ False & boostL .~ 0 & \a -> a {villainAttackTarget = Nothing, villainPendingAttackBoostCards = 0, villainPendingSchemeBoostCards = 0, villainAttackAdditionalBoostCards = 0}
      else do
        -- the attack is his activation against the player, from start to end
        checkWindows [W.Window W.When $ W.EnemyActivation (toEnemyId attrs) ident]
        checkWindows [attackWindow W.Would ident]
        villainStep vid (VillainSetAttackAdditionalBoostCards $ villainPendingAttackBoostCards attrs)
        villainStep vid begin
        villainStep vid VillainEndAttack
        clearBoosts
        checkWindows [W.Window W.After $ W.EnemyActivation (toEnemyId attrs) ident]
        -- the attack is under way from when it is declared, as its would windows open,
        -- with the keywords it gains as it is declared
        attack' <- flip (foldr gainKeyword) keywords <$> newAttack ident
        pure $ attrs {villainActivationPlayer = Just ident, villainAttackGrantedKeywords = keywords, villainPendingAttackBoostCards = 0} & attackingL ?~ attack'
  -- an attack "that does not get a boost card" is dealt none
  beginAttack boosted ident = do
    -- with the keywords it gained as it was declared, and what its activation already
    -- got for itself
    attack' <- (attackDamageL +~ villainBoost attrs) . keepingGains (villainAttacking attrs) <$> newAttack ident
    modifiers <- getModifiers attrs
    checkWindows [attackWindow W.When ident]
    when boosted do
      dealActivationBoost
      unless (NoActivationBoostCard `elem` modifiers) $
        replicateM_ (fromIntegral $ villainAttackAdditionalBoostCards attrs) (dealBoost attrs)
    case attackTarget ident of
      VillainCharacter _ -> pure ()
      MinionCharacter _ -> pure ()
      _ -> declareDefense ident vid (if DefendedByAllyIfAble `elem` modifiers then AllyIfAble else AnyDefense)
    villainStep vid VillainFlipBoostCards
    villainStep vid VillainAttacked
    checkWindows [attackWindow W.After ident]
    pure $
      attrs {villainAttackTarget = Nothing, villainAttackAdditionalBoostCards = 0} & attackingL ?~ attack'
  dealActivationBoost = do
    modifiers <- getModifiers attrs
    unless (NoActivationBoostCard `elem` modifiers) $ dealBoost attrs
  attackWindow timing ident = W.Window timing $ case villainAttackTarget attrs of
    Just (AllyCharacter aid) -> W.EnemyAttackAlly (toEnemyId attrs) aid ident
    Just (VillainCharacter target) -> W.EnemyAttackOnEnemy (toEnemyId attrs) (EnemyVillainId target)
    Just (MinionCharacter target) -> W.EnemyAttackOnEnemy (toEnemyId attrs) (EnemyMinionId target)
    _ -> W.EnemyAttack (toEnemyId attrs) ident
  attackTarget ident = fromMaybe (IdentityCharacter ident) (villainAttackTarget attrs)
  newAttack ident = do
    base <- getModifiedAttackSigned attrs
    keywords <- getModifiedKeywords attrs
    prepareEnemyAttack (toEnemyId attrs) $ withAttackBase base $ attackWith keywords (toEnemyId attrs) (attackTarget ident) 0

instance RunMessage (Attrs Villain) where
  runMessage msg attrs = runQueueT $ case msg of
    CardSpecific target "recordPendingSchemeThreat" (fromJSON -> Success scheme) | isTarget attrs target -> attrs <$ recordPendingVillainSchemeThreat (villainId attrs) scheme
    CardSpecific target "villainSchemeResult" (fromJSON -> Success n) | isTarget attrs target -> do
      checkWindows [W.Window W.After $ W.VillainSchemeResolved (villainId attrs) n]
      pure attrs
    CardSpecific target "fwReplaceBoost" (fromJSON -> Success (old, replacement)) | isTarget attrs target ->
      pure attrs {villainBoostCards = map (\card -> if card == old then replacement else card) (villainBoostCards attrs)}
    CardSpecific target "schemeReplacesAttack" (fromJSON -> Success flag) | isTarget attrs target -> pure attrs {villainSchemeReplacesAttack = flag}
    CardSpecific target "schemeWithoutBoost" (fromJSON -> Success flag) | target == toRef attrs ->
      pure attrs {villainSchemeWithoutBoost = flag}
    CardSpecific target "additionalActivationBoost" (fromJSON -> Success n) | target == toRef attrs ->
      pure attrs {villainPendingAttackBoostCards = n, villainPendingSchemeBoostCards = n}
    CardSpecific target "clearSchemeBonus" (fromJSON -> Success n) | target == toRef attrs ->
      pure attrs {villainScheme = Sch (subtractNatural n $ unSch $ villainScheme attrs)}
    CardSpecific target "neRedirectAttackToAlly" (fromJSON -> Success (you,aid)) | target == toRef attrs ->
      pure attrs {villainActivationPlayer = Just you, villainAttackTarget = Just (AllyCharacter aid), villainAttacking = fmap (\a -> a {attackCharacter = AllyCharacter aid, attackTarget = Nothing}) (villainAttacking attrs)}
    PlaceCounters target n | target == toRef attrs -> pure attrs {villainCounters = villainCounters attrs + n}
    RemoveCounters target n | target == toRef attrs -> pure attrs {villainCounters = subtractNatural n (villainCounters attrs)}
    GiveStatus (VillainRef vid) s | vid == villainId attrs -> beginVillainStatus attrs s
    CardSpecific target "gainVillainStatus" (fromJSON -> Success status) | isTarget attrs target -> applySolidSoundStatus attrs status
    RemoveStatus (VillainRef vid) StunnedStatus | vid == villainId attrs, villainAdditionalStun attrs -> pure attrs {villainAdditionalStun = False}
    RemoveStatus (VillainRef vid) ConfusedStatus | vid == villainId attrs, villainAdditionalConfuse attrs -> pure attrs {villainAdditionalConfuse = False}
    RemoveStatus (VillainRef vid) s | vid == villainId attrs -> pure $ attrs & statusL s .~ False
    AttachmentRemoved attachmentId -> do
      pure $ attrs & attachmentsL %~ HashSet.delete attachmentId
    UpgradeMessage uid UpgradeDetached -> pure $ attrs & upgradesL %~ HashSet.delete uid
    UpgradeRemoved upgradeId -> do
      pure $ attrs & upgradesL %~ HashSet.delete upgradeId
    VillainMessage ident msg'
      | ident == villainId attrs ->
          runVillainMessage msg' attrs
    -- the attack whose damage he is spared has ended: what made it, an ability or an
    -- event, has resolved all it does (or, at the latest, the phase has ended)
    AbilityResolved source | villainPreventedAttack attrs == Just source -> pure attrs {villainPreventedAttack = Nothing}
    EventMessage eid ResolvedEvent | Just (_, EventRef source) <- villainSchemeRemoval attrs, eid == source -> pure attrs {villainSchemeRemoval = Nothing}
    EventMessage eid ResolvedEvent | villainPreventedAttack attrs == Just (EventRef eid) -> pure attrs {villainPreventedAttack = Nothing}
    EndPhase _ -> pure attrs {villainPreventedAttack = Nothing}
    WithBoostIcons (VillainRef vid) _ counts | vid == villainId attrs -> do
      villainStep vid (VillainAddBoost $ sum $ map snd counts)
      pure attrs
    _ -> pure attrs

-- | The damage is the attack's whose damage to him is prevented ('villainPreventedAttack'):
-- by what makes it, an attack's own or its overkill (RR Overkill: damage from an attack).
preventedAttackDamage :: Attrs Villain -> Source -> Damage -> Bool
preventedAttackDamage attrs source damage =
  villainPreventedAttack attrs == Just source && case damageSource damage of
    FromPlayerAttack _ -> True
    FromAllyAttack _ -> True
    FromOverkill -> True
    _ -> False

instance IsRef (Attrs Villain) where
  toRef = VillainRef . villainId

-- | The next stage, of the same title: what was on the villain carries over, and an
-- activation it was defeated during resumes with the new stage, its attack at the new
-- stage's ATK.
advanceVillainTo :: (IsVillain a) => VillainCard a -> Attrs Villain -> a
advanceVillainTo newVillain old = overVillainAttrs (carryOver old) $ buildCard newVillain (villainId old)

-- | What carries over from the villain card to the next of his title (an advance, a
-- flip, a swap): status cards, attachments, upgrades, boosts and an activation under
-- way, its attack at the new card's ATK.
carryOver :: Attrs Villain -> Attrs Villain -> Attrs Villain
carryOver VillainAttrs {..} = update
 where
  update new =
    new
      & (confusedL .~ villainConfused)
      & (stunnedL .~ villainStunned)
      & (\a -> a {villainAdditionalStun = villainAdditionalStun, villainAdditionalConfuse = villainAdditionalConfuse})
      & (toughL %~ (|| villainTough))
      & (attachmentsL .~ villainAttachments)
      & (upgradesL .~ villainUpgrades)
      & (boostCardsL .~ villainBoostCards)
      & (boostL .~ villainBoost)
      & (\a -> a {villainCounters = villainCounters, villainSchemeReplacesAttack = villainSchemeReplacesAttack, villainSchemeWithoutBoost = villainSchemeWithoutBoost, villainPendingSchemeBoostCards = villainPendingSchemeBoostCards, villainAttackGrantedKeywords = villainAttackGrantedKeywords, villainPendingAttackBoostCards = villainPendingAttackBoostCards, villainAttackAdditionalBoostCards = villainAttackAdditionalBoostCards})
      & (attackingL .~ (villainAttacking <&> attackDamageL %~ restage new))
      & \a -> a {villainAttackTarget = villainAttackTarget, villainActivationPlayer = villainActivationPlayer, villainPreventedAttack = villainPreventedAttack}
  restage VillainAttrs {villainAttack = Atk new} dmg = subtractNatural (unAtk villainAttack) (dmg + new)

-- | The villain's other face, as a flip turns it: unlike advancing, its damage and stage
-- stay as they were, with all else that advancing keeps.
flipVillainTo :: (IsVillain a) => VillainCard a -> Attrs Villain -> a
flipVillainTo newVillain attrs = overVillainAttrs (keepDial attrs) $ advanceVillainTo newVillain attrs

-- | The hit point dial, stage and tough status card of the villain card replaced, kept
-- by the one replacing it (a flip, a swap).
keepDial :: Attrs Villain -> Attrs Villain -> Attrs Villain
keepDial attrs a = a {villainCounters = villainCounters attrs} & (hpL .~ villainHp attrs) & (maxHpL .~ villainMaxHp attrs) & (stageL .~ villainStage attrs) & (toughL .~ villainTough attrs)

-- | The damage less what reduces "any amount of damage" the villain would take (Thanos's
-- Armor), once a tough status card has had its say.
reducedDamage :: [Modifier] -> Damage -> Damage
reducedDamage modifiers d
  | damageUnpreventable d = d
  | otherwise = d {damageAmount = maybe reduced (min reduced) cap}
 where
  attack = case damageSource d of
    FromPlayerAttack _ -> True
    FromAllyAttack _ -> True
    FromAttack -> True
    FromOverkill -> True
    _ -> False
  reduction = sum [n | DamageReduction n <- modifiers] + sum [n | AttackDamageReduction n <- modifiers, attack]
  reduced = subtractNatural reduction (damageAmount d)
  cap = case [n | AttackDamageCap n <- modifiers, attack] of
    [] -> Nothing
    ns -> Just (minimum ns)

getModifiedAttack :: (HasGame m) => Attrs Villain -> m Natural
getModifiedAttack attrs = (fromInteger . max 0) <$> getModifiedAttackSigned attrs

-- | Keep the signed intermediate while an activation still has boosts and
-- attack-specific modifiers to add (RR Modifiers).
getModifiedAttackSigned :: HasGame m => Attrs Villain -> m Integer
getModifiedAttackSigned attrs = ((+ toInteger (unAtk $ villainAttack attrs)) . sum . map change) <$> getModifiers attrs
 where
  change (AttackModifier n) = toInteger n
  change (AttackPenalty n) = negate (toInteger n)
  change (AttackReductionWhileAttacking you n)
    | Just (IdentityCharacter you) == (attackCharacter <$> villainAttacking attrs) = negate (toInteger n)
  change _ = 0

getModifiedScheme :: (HasGame m) => Attrs Villain -> m Natural
getModifiedScheme attrs = do
  modifiers <- getModifiers attrs
  pure $ fromInteger $ max 0 $ toInteger (unSch $ villainScheme attrs) + sum (map change modifiers)
 where
  change (SchemeModifier n) = toInteger n
  change (SchemePenalty n) = negate (toInteger n)
  change _ = 0

-- | The Wrecking Crew's extreme variant (TWC insert p.7, "Adjustable Difficulty"): each
-- villain starts as version A with version B under it, and when version A is defeated
-- version B enters play. Played as the scenario set up by Breakout's A side.
versionBEnters :: (HasGame m) => Attrs Villain -> m Bool
versionBEnters attrs
  | villainStage attrs /= 1 = pure False
  | otherwise = (== "07001a") <$> getScenarioCode

-- Older saved games predate a second status card.
instance FromJSON (Attrs Villain) where
  parseJSON = withObject "VillainAttrs" $ \o ->
    genericParseJSON defaultOptions $ Object $
      KeyMap.insertWith (\_ old -> old) "villainCounters" (toJSON (0 :: Natural)) $
      KeyMap.insertWith (\_ old -> old) "villainSchemeRemoval" (toJSON (Nothing :: Maybe (IdentityId, Source))) $
      KeyMap.insertWith (\_ old -> old) "villainPendingAttackBoostCards" (toJSON (0 :: Natural)) $
      KeyMap.insertWith (\_ old -> old) "villainSchemeReplacesAttack" (Bool False) $ KeyMap.insertWith (\_ old -> old) "villainSchemeWithoutBoost" (Bool False) $
      KeyMap.insertWith (\_ old -> old) "villainPendingSchemeBoostCards" (toJSON (0 :: Natural)) $
      KeyMap.insertWith (\_ old -> old) "villainAttackAdditionalBoostCards" (toJSON (0 :: Natural)) $
      KeyMap.insertWith (\_ old -> old) "villainAttackGrantedKeywords" (toJSON ([] :: [Keyword])) $
      KeyMap.insertWith (\_ old -> old) "villainAdditionalStun" (Bool False) $
      KeyMap.insertWith (\_ old -> old) "villainAdditionalConfuse" (Bool False) o

applySolidSoundStatus :: ReverseQueue m => Attrs Villain -> StatusCard -> m (Attrs Villain)
applySolidSoundStatus attrs status = do
  replaced <- if status == ToughStatus then pure False else absorbStatusWithSolidSound (EnemyVillainId $ villainId attrs)
  if replaced then pure attrs else statusUnlessStalwart attrs status

-- | An impossible or redundant status does not open an interrupt window.
beginVillainStatus :: ReverseQueue m => Attrs Villain -> StatusCard -> m (Attrs Villain)
beginVillainStatus attrs status = do
  updated <- statusUnlessStalwart attrs status
  when (updated /= attrs) $ do
    checkWindows [W.Window W.When $ W.VillainGainingStatus (villainId attrs) status]
    cardSpecific attrs "gainVillainStatus" status
  pure attrs
