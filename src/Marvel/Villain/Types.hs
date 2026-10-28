module Marvel.Villain.Types (
  module Marvel.Villain.Types,
  module X,
) where

import Marvel.Prelude

import Data.HashSet qualified as HashSet
import Data.Typeable
import Marvel.Ability.Types
import Marvel.Attack
import Marvel.Boost
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
  getModifiersFor source target (Villain a) = getModifiersFor source target a

instance RunMessage Villain where
  runMessage msg (Villain a) = Villain <$> runMessage msg a

instance HasCardDef Villain where
  getCardDef = getCardDef . toAttrs

instance Entity Villain where
  type Id Villain = VillainId
  data Attrs Villain = VillainAttrs
    { villainId :: VillainId
    , villainCardDef :: CardDef
    , villainHp :: HP Int
    , villainStartingHp :: HP GameValue
    , villainMaxHp :: HP Int
    , villainScheme :: Sch
    , villainAttack :: Atk
    , villainStunned :: Bool
    , villainConfused :: Bool
    , villainTough :: Bool
    , villainBoostCards :: [EncounterCard]
    , villainBoost :: Natural
    , villainAttacking :: Maybe Attack
    , villainAttachments :: HashSet AttachmentId
    , villainUpgrades :: HashSet UpgradeId
    , villainStage :: Natural
    , villainAttackTarget :: Maybe CharacterId
    -- ^ the character its coming attack is against, when not the player's identity
    , villainActivationPlayer :: Maybe IdentityId
    -- ^ the player its activation under way is against, who resolves its boost cards
    , villainPreventedAttack :: Maybe Source
    -- ^ the attack under way, by what makes it, all of whose damage to him is prevented
    -- until it ends (Master of Illusions)
    }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
  data Field Villain :: Type -> Type where
    VillainId :: Field Villain VillainId
    VillainCardDef :: Field Villain CardDef
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
            , villainHp = HP 1
            , villainScheme = sch
            , villainAttack = atk
            , villainStunned = False
            , villainConfused = False
            , villainTough = False
            , villainBoostCards = mempty
            , villainBoost = 0
            , villainAttacking = Nothing
            , villainAttachments = mempty
            , villainUpgrades = mempty
            , villainStage = 1
            , villainAttackTarget = Nothing
            , villainActivationPlayer = Nothing
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
stunnedL = lens villainStunned $ \m x -> m {villainStunned = x}

confusedL :: Lens' (Attrs Villain) Bool
confusedL = lens villainConfused $ \m x -> m {villainConfused = x}

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
  stalwart <- elem Stalwart <$> getModifiedKeywords attrs
  pure $ if stalwart && s /= ToughStatus then attrs else attrs & statusL s .~ True

getModifiedKeywords :: (HasGame m) => Attrs Villain -> m [Keyword]
getModifiedKeywords attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (toList . cdKeywords $ getCardDef attrs) modifiers
 where
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
    when (remaining <= 0 && not unbeaten) $ villainStep vid VillainWouldBeDefeated
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
    for_ [n | Retaliate n <- toList keywords] $ retaliate attrs attacker
    pure attrs
  VillainDamaged source dealt -> do
    modifiers <- getModifiers attrs
    -- a tough status card prevents the damage before anything else that answers "would
    -- take damage" (RR Damage); what gets past it is reduced (Thanos's Armor)
    let damage = if villainTough attrs && preventedByTough dealt then dealt else reducedDamage modifiers dealt
    -- none at all (The Red House), or none from a player card (Sanctuary)
    cannotTakeDamage <- cannotTakeDamageFrom (toRef attrs) source
    let -- infinite hit points: the damage is dealt, but he is not defeated by it
        infinite = InfiniteHitPoints `elem` modifiers
        -- "cannot be defeated" (Tower Defense): the damage is dealt, to 0 hit points, but
        -- he is not defeated, nor is any of it excess
        held = CannotBeDefeated `elem` modifiers
    if
      | cannotTakeDamage -> pure attrs
      | preventedAttackDamage attrs source damage -> pure attrs
      | villainTough attrs && preventedByTough dealt -> pure $ attrs & toughL .~ False
      | otherwise -> do
        when (damageAmount damage > 0) $
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
            villainStep vid VillainWouldBeDefeated
            -- a stage defeated is the villain defeated: "after you attack and defeat an enemy"
            checkWindows [W.Window W.After $ W.DefeatedVillain vid damage source excess]
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
  VillainWouldBeDefeated -> attrs <$ defeatVillain vid
  VillainBecomeTough -> pure $ attrs & toughL .~ True
  VillainPreventAttackDamage source -> pure attrs {villainPreventedAttack = Just source}
  -- a stalwart villain cannot be stunned or confused (RR Stalwart)
  Msg.VillainStunned _ -> statusUnlessStalwart attrs StunnedStatus
  Msg.VillainConfused _ -> statusUnlessStalwart attrs ConfusedStatus
  VillainSchemes ->
    if villainConfused attrs
      then pure $ attrs {villainActivationPlayer = Nothing} & confusedL .~ False & boostL .~ 0
      else attrs <$ villainStep vid VillainBeginScheme
  -- queued after the scheme, everything it leads to runs before the player is forgotten,
  -- whether the scheme resolved, was replaced or was cancelled
  VillainSchemesAgainst ident -> do
    villainStep vid VillainSchemes
    villainStep vid VillainSchemeAgainstEnded
    pure attrs {villainActivationPlayer = Just ident}
  VillainSchemeAgainstEnded -> pure attrs {villainActivationPlayer = Nothing}
  VillainBeginScheme -> do
    -- against the player it was said to be against, or the active player
    player <- maybe getActivePlayerId pure (villainActivationPlayer attrs)
    -- the scheme is his activation against the player whose alter-ego activates him
    checkWindows [W.Window W.When $ W.EnemyActivation (toEnemyId attrs) player]
    dealActivationBoost
    villainStep vid VillainFlipBoostCards
    villainStep vid VillainSchemed
    clearBoosts
    checkWindows [W.Window W.After $ W.EnemyActivation (toEnemyId attrs) player]
    pure attrs {villainActivationPlayer = Just player}
  -- "attacks an ally": the player who controls it is attacked
  VillainAttacksAlly ident allyId -> do
    enemyAttacks vid ident
    pure attrs {villainAttackTarget = Just (AllyCharacter allyId)}
  VillainAttacks ident -> attacks (VillainBeginAttack ident) ident []
  VillainAttacksWithoutBoost ident -> attacks (VillainBeginAttackWithoutBoost ident) ident []
  VillainAttacksGaining ident keywords -> attacks (VillainBeginAttack ident) ident keywords
  VillainBeginAttack ident -> beginAttack True ident
  VillainBeginAttackWithoutBoost ident -> beginAttack False ident
  VillainEndAttack -> pure $ attrs {villainActivationPlayer = Nothing} & attackingL .~ Nothing
  VillainAttackGains k -> pure $ attrs & attackingL . _Just %~ gainKeyword k
  VillainAttackChanged n -> pure $ attrs & attackingL . _Just %~ \a -> a {attackChange = attackChange a + n}
  VillainAttackDamagePrevented -> pure $ attrs & attackingL . _Just %~ \a -> a {attackPrevented = True}
  VillainDefendedBy characterId -> pure $ attrs & attackingL . _Just %~ defendedBy characterId
  -- onto his own side scheme, when he has one, or his own main scheme (Tower Defense),
  -- or else the main scheme
  VillainSchemed -> do
    own <- selectOne (SignatureSchemeOf $ VillainWithId vid)
    paired <- getPairedMainScheme vid
    scheme <- maybe (selectOne MainScheme) (pure . Just) $ (SchemeSideSchemeId <$> own) <|> (SchemeMainSchemeId <$> paired)
    sch <- getModifiedScheme attrs
    for_ scheme \s -> placeThreatFrom W.ThreatFromVillain s (sch + villainBoost attrs)
    checkWindows [W.Window W.After $ W.Schemed vid]
    pure $ attrs {villainActivationPlayer = Nothing} & boostL .~ 0
  VillainAttacked -> do
    case villainAttacking attrs of
      Nothing -> error "No current attack"
      Just attack0 -> do
        attack' <- undefendedIfDefenderGone attack0
        attackedYou (attackEnemy attack') (attackCharacter attack')
        wasAttacked attack'
        pure $ attrs & boostL .~ 0 & attackingL ?~ attack'
  VillainDealtBoost c -> pure $ attrs & boostCardsL %~ (c :)
  AttachedToVillain attachmentId -> do
    pure $ attrs & attachmentsL %~ HashSet.insert attachmentId
  AttachedUpgradeToVillain upgradeId -> do
    pure $ attrs & upgradesL %~ HashSet.insert upgradeId
  -- each boost card is turned face up in turn, and its icons counted and added, which
  -- an interrupt to its turning face up may cancel
  VillainFlipBoostCards -> do
    for_ (villainBoostCards attrs) \card -> do
      checkWindows [W.Window W.When $ W.FlippedBoostCard card (toEnemyId attrs)]
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
  attacks begin ident keywords =
    if villainStunned attrs
      then pure $ attrs & stunnedL .~ False & boostL .~ 0 & \a -> a {villainAttackTarget = Nothing}
      else do
        -- the attack is his activation against the player, from start to end
        checkWindows [W.Window W.When $ W.EnemyActivation (toEnemyId attrs) ident]
        checkWindows [W.Window W.Would $ W.EnemyAttack (toEnemyId attrs) ident]
        villainStep vid begin
        villainStep vid VillainEndAttack
        clearBoosts
        checkWindows [W.Window W.After $ W.EnemyActivation (toEnemyId attrs) ident]
        -- the attack is under way from when it is declared, as its would windows open,
        -- with the keywords it gains as it is declared
        attack' <- flip (foldr gainKeyword) keywords <$> newAttack ident
        pure $ attrs {villainActivationPlayer = Just ident} & attackingL ?~ attack'
  -- an attack "that does not get a boost card" is dealt none
  beginAttack boosted ident = do
    -- with the keywords it gained as it was declared, and what its activation already
    -- got for itself
    attack' <- (attackDamageL +~ villainBoost attrs) . keepingGains (villainAttacking attrs) <$> newAttack ident
    modifiers <- getModifiers attrs
    checkWindows [W.Window W.When $ W.EnemyAttack (toEnemyId attrs) ident]
    when boosted dealActivationBoost
    declareDefense ident vid (if DefendedByAllyIfAble `elem` modifiers then AllyIfAble else AnyDefense)
    villainStep vid VillainFlipBoostCards
    villainStep vid VillainAttacked
    checkWindows [W.Window W.After $ W.EnemyAttack (toEnemyId attrs) ident]
    pure $
      attrs {villainAttackTarget = Nothing} & attackingL ?~ attack'
  dealActivationBoost = do
    modifiers <- getModifiers attrs
    unless (NoActivationBoostCard `elem` modifiers) $ dealBoost attrs
  attackTarget ident = fromMaybe (IdentityCharacter ident) (villainAttackTarget attrs)
  newAttack ident = attackWith <$> getModifiedKeywords attrs <*> pure (toEnemyId attrs) <*> pure (attackTarget ident) <*> getModifiedAttack attrs

instance RunMessage (Attrs Villain) where
  runMessage msg attrs = runQueueT $ case msg of
    GiveStatus (VillainRef vid) s | vid == villainId attrs -> statusUnlessStalwart attrs s
    RemoveStatus (VillainRef vid) s | vid == villainId attrs -> pure $ attrs & statusL s .~ False
    AttachmentRemoved attachmentId -> do
      pure $ attrs & attachmentsL %~ HashSet.delete attachmentId
    UpgradeRemoved upgradeId -> do
      pure $ attrs & upgradesL %~ HashSet.delete upgradeId
    VillainMessage ident msg'
      | ident == villainId attrs ->
          runVillainMessage msg' attrs
    -- the attack whose damage he is spared has ended: what made it, an ability or an
    -- event, has resolved all it does (or, at the latest, the phase has ended)
    AbilityResolved source | villainPreventedAttack attrs == Just source -> pure attrs {villainPreventedAttack = Nothing}
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
      & (toughL %~ (|| villainTough))
      & (attachmentsL .~ villainAttachments)
      & (upgradesL .~ villainUpgrades)
      & (boostCardsL .~ villainBoostCards)
      & (boostL .~ villainBoost)
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
keepDial attrs a = a & (hpL .~ villainHp attrs) & (maxHpL .~ villainMaxHp attrs) & (stageL .~ villainStage attrs) & (toughL .~ villainTough attrs)

-- | The damage less what reduces "any amount of damage" the villain would take (Thanos's
-- Armor), once a tough status card has had its say.
reducedDamage :: [Modifier] -> Damage -> Damage
reducedDamage modifiers d = d {damageAmount = subtractNatural (sum [n | DamageReduction n <- modifiers]) (damageAmount d)}

getModifiedAttack :: (HasGame m) => Attrs Villain -> m Natural
getModifiedAttack attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unAtk $ villainAttack attrs) modifiers
 where
  applyModifier (AttackModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

getModifiedScheme :: (HasGame m) => Attrs Villain -> m Natural
getModifiedScheme attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (unSch $ villainScheme attrs) modifiers
 where
  applyModifier (SchemeModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id

-- | The Wrecking Crew's extreme variant (TWC insert p.7, "Adjustable Difficulty"): each
-- villain starts as version A with version B under it, and when version A is defeated
-- version B enters play. Played as the scenario set up by Breakout's A side.
versionBEnters :: (HasGame m) => Attrs Villain -> m Bool
versionBEnters attrs
  | villainStage attrs /= 1 = pure False
  | otherwise = (== "07001a") <$> getScenarioCode
