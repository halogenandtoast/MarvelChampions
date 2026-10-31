module Marvel.Ability (
  module Marvel.Ability,
  module X,
) where

import Marvel.Prelude

import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability.Types as X
import Marvel.Card.PlayerCard.Types
import Marvel.Discard (unDiscard)
import Marvel.Damage (damageAmount)
import Marvel.Message qualified as Msg
import Marvel.Card.Id (CardId (..))
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.Id
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Modifier (Modifier (CannotTriggerPrintedHeroAbilities, CannotBeCanceled, LoseKeyword, KeywordModifier))
import Marvel.Card.Def (cdKeywords)
import Marvel.Projection
import Marvel.Query
import {-# SOURCE #-} Marvel.Question
import Marvel.Queue
import Marvel.Ref
import Marvel.Window

ability ::
  (IsRef a) =>
  a ->
  Natural ->
  AbilityType ->
  Criteria ->
  Cost ->
  Choice ->
  Ability
ability a idx aType =
  limitedAbility (toSource a) idx (defaultAbilityLimit aType) aType

windowAbility ::
  (IsRef a) =>
  a ->
  Natural ->
  WindowMatcher ->
  AbilityType ->
  Cost ->
  Choice ->
  Ability
windowAbility a idx window aType cost choice =
  limitedAbility
    (toSource a)
    idx
    (defaultAbilityLimit aType)
    aType
    NoCriteria
    cost
    choice
    & windowL
      ?~ window

limitedWindowAbility ::
  (IsRef a) =>
  a ->
  Natural ->
  WindowMatcher ->
  AbilityType ->
  Criteria ->
  Cost ->
  Choice ->
  Ability
limitedWindowAbility a idx window aType criteria cost choice =
  limitedAbility
    (toSource a)
    idx
    (defaultAbilityLimit aType)
    aType
    criteria
    cost
    choice
    & windowL
      ?~ window

limitedAbility ::
  (IsRef a) =>
  a ->
  Natural ->
  Limit ->
  AbilityType ->
  Criteria ->
  Cost ->
  Choice ->
  Ability
limitedAbility a idx limit aType criteria cost choice =
  Ability
    { abilitySource = toSource a
    , abilityIndex = idx
    , abilityCriteria = criteria
    , abilityLimit = limit
    , abilityTiming = pure DuringOwnTurn
    , abilityCost = cost
    , abilityChoices = [choice]
    , abilityType = aType
    , abilitySubType = Nothing
    , abilityLabel = Nothing
    , abilityWindow = Nothing
    }

label :: Text -> Ability -> Ability
label l a = a {abilityLabel = Just l}

limited :: Limit -> Ability -> Ability
limited l a = a {abilityLimit = l}

subtype :: AbilitySubType -> Ability -> Ability
subtype s a = a {abilitySubType = Just s}

passesUseLimit :: IdentityId -> HashMap IdentityId [Ability] -> Ability -> Bool
passesUseLimit x aMap a = case baseLimit (abilityLimit a) of
  NoLimit -> True
  PerTurn n -> count (== a) usedAbilities < n
  PerRound n -> count (== a) usedAbilities < n
  PerWindow n -> count (== a) usedAbilities < n
  SharedPerWindow name n -> count (\used -> case baseLimit (abilityLimit used) of
    SharedPerWindow other _ -> name == other
    _ -> False) usedAbilities < n
  PerPhase n -> count (== a) usedAbilities < n
 where
  usedAbilities = HashMap.findWithDefault [] x aMap

passesCriteria :: (Projection m PlayerIdentity, MonadThrow m, HasGame m, HasQueue m) => IdentityId -> Ability -> m Bool
passesCriteria x a =
  go
    ( abilityTypeCriteria (abilityType a)
        <> maybe NoCriteria abilitySubTypeCriteria (abilitySubType a)
        <> abilityCriteria a
    )
 where
  go = \case
    IsSelf -> pure $ toSource x == source
    SelfMatches identityMatcher ->
      member x <$> select (IdentityWithId x <> identityMatcher)
    NoCriteria -> pure True
    Never -> pure False
    InHeroForm -> member x <$> select HeroIdentity
    InAlterEgoForm -> member x <$> select AlterEgoIdentity
    Exhausted -> member x <$> select ExhaustedIdentity
    Unexhausted -> member x <$> select UnexhaustedIdentity
    OwnsThis -> case abilitySource a of
      AllyRef aid ->
        member aid <$> select (AllyControlledBy $ IdentityWithId x)
      SupportRef aid ->
        member aid <$> select (SupportControlledBy $ IdentityWithId x)
      UpgradeRef aid ->
        member aid <$> select (UpgradeControlledBy $ IdentityWithId x)
      _ -> error $ "Unhandled " <> show (abilitySource a)
    Criteria xs -> allM go xs
    AnyCriteria xs -> anyM go xs
    NotCriteria x -> not <$> go x
    MinionExists m -> selectAny m
    CharacterExists m -> selectAny m
    EnemyExists m -> selectAny m
    SchemeExists m -> selectAny m
    AllyExists m -> selectAny m
    AttachmentExists m -> selectAny m
    SideSchemeExists m -> selectAny m
    SupportExists m -> selectAny m
    CanPlayAttackEventWithClaws -> notNull <$> playableClawsEvents x (abilitySource a)
    ExtendedCardExists m -> selectAny m
    -- what is paid for is checked as it is paid (see 'getResourceAbilities')
    SpendOnly _ -> pure True
    UpgradeExists m -> selectAny m
    EnvironmentExists m -> selectAny m
    -- the card paid for is checked by 'getResourceAbilities'
    PayingFor _ -> pure True
    PayingForAbility _ _ -> pure True
    SourceHasKeyword keyword -> do
      blanked <- isBlanked source
      modifiers <- getModifiers source
      definition <- getsGame (`abilityCardDef` refocus source)
      pure $ not blanked && LoseKeyword keyword `notElem` modifiers
        && (maybe False (elem keyword . cdKeywords) definition || KeywordModifier keyword `elem` modifiers)
    -- peril is checked by 'getAbilitiesFor'
    ForAnyPlayer -> pure True
    PlayedEventInDiscard -> playedEventInDiscard
    CollectionHasCards -> notNull <$> getCollection
    CardPlayAfterExhaustingHasTarget _ -> pure False
    PlayCardWithReductionHasTarget matcher amount ->
      notNull <$> playableCardsWithReductionAfterExhausting x (toTarget source) matcher amount
    ControlsCardsWithTrait trait n -> (>= n) <$> countControlledCardsWithTrait x trait
    CanCancelRevealedEncounterCard -> canCancelRevealedEncounterCard
    CanCancelRevealedTreachery -> canCancelRevealedTreachery
    CanResolvePrintedHeroAbilities -> notElem CannotTriggerPrintedHeroAbilities <$> getModifiers x
    PendingDamageAtLeast n -> pendingDamageAtLeast x n
    HasOtherSchemeForThreatMovement -> hasOtherSchemeForThreatMovement
    HasOtherSchemeForThreatRemoval -> hasOtherSchemeForThreatRemoval
  source = abilitySource a

-- | Whether a card's criteria hold for the player who would play it.
passesCardCriteria :: (Projection m PlayerIdentity, MonadThrow m, HasGame m, HasQueue m) => IdentityId -> PlayerCard -> Criteria -> m Bool
passesCardCriteria ident card = go
 where
  go = \case
    IsSelf -> error "Irrelevant"
    OwnsThis -> error "Irrelevant"
    NoCriteria -> pure True
    Never -> pure False
    SelfMatches identityMatcher ->
      member ident <$> select (IdentityWithId ident <> identityMatcher)
    InHeroForm -> member ident <$> select HeroIdentity
    InAlterEgoForm -> member ident <$> select AlterEgoIdentity
    Unexhausted -> member ident <$> select UnexhaustedIdentity
    Exhausted -> member ident <$> select ExhaustedIdentity
    Criteria xs -> allM go xs
    AnyCriteria xs -> anyM go xs
    NotCriteria x -> not <$> go x
    MinionExists m -> selectAny m
    EnemyExists m -> selectAny m
    CharacterExists m -> selectAny m
    AllyExists m -> selectAny m
    AttachmentExists m -> selectAny m
    SideSchemeExists m -> selectAny m
    SupportExists m -> selectAny m
    SchemeExists m -> selectAny m
    -- the card itself, about to leave the hand, is not one of them
    CanPlayAttackEventWithClaws -> notNull <$> playableClawsEvents ident (toSource card)
    ExtendedCardExists m -> selectAny (NotCard card <> m)
    UpgradeExists m -> selectAny m
    EnvironmentExists m -> selectAny m
    -- how it is paid for is checked as it is paid
    SpendOnly _ -> pure True
    PayingFor _ -> pure True
    PayingForAbility _ _ -> pure True
    SourceHasKeyword _ -> pure True
    ForAnyPlayer -> pure True
    PlayedEventInDiscard -> playedEventInDiscard
    CollectionHasCards -> notNull <$> getCollection
    CardPlayAfterExhaustingHasTarget n -> hasCardToPlayWithReductionAfterExhausting ident card n
    PlayCardWithReductionHasTarget _ _ -> pure False
    ControlsCardsWithTrait trait n -> (>= n) <$> countControlledCardsWithTrait ident trait
    CanCancelRevealedEncounterCard -> canCancelRevealedEncounterCard
    CanCancelRevealedTreachery -> canCancelRevealedTreachery
    CanResolvePrintedHeroAbilities -> notElem CannotTriggerPrintedHeroAbilities <$> getModifiers ident
    PendingDamageAtLeast n -> pendingDamageAtLeast ident n
    HasOtherSchemeForThreatMovement -> hasOtherSchemeForThreatMovement
    HasOtherSchemeForThreatRemoval -> hasOtherSchemeForThreatRemoval

partitionMaybes :: [Maybe a] -> ([()], [a])
partitionMaybes = foldr (maybe nothing just) ([], [])
 where
  nothing ~(l, r) = (() : l, r)
  just a ~(l, r) = (l, a : r)

passesTiming :: IdentityId -> Ability -> Bool
passesTiming _ a = all passes (toList $ abilityTiming a)
 where
  passes x = case x of
    DuringOwnTurn -> True

passesTypeIsRelevant :: (HasGame m) => IdentityId -> Ability -> m Bool
passesTypeIsRelevant ident a = case abilityType a of
  Interrupt -> pure False
  AlterEgoInterrupt -> pure False
  HeroInterrupt -> pure False
  ForcedInterrupt -> pure False
  Resource -> pure False
  HeroResource -> pure False
  AlterEgoResource -> pure False
  Response -> pure False
  AlterEgoResponse -> pure False
  HeroResponse -> pure False
  ForcedResponse -> pure False
  Action -> pure True
  Basic -> pure True
  HeroAction -> member ident <$> select HeroIdentity
  AlterEgoAction -> member ident <$> select AlterEgoIdentity
  Special -> pure False
  Setup -> pure False
  WhenRevealed -> pure False

class PerformAbility a where
  performAbility :: (HasGame m) => a -> Natural -> m ()

isForcedAbility :: Ability -> Bool
isForcedAbility a = case abilityType a of
  Interrupt -> False
  AlterEgoInterrupt -> False
  HeroInterrupt -> False
  ForcedInterrupt -> True
  Resource -> False
  HeroResource -> False
  AlterEgoResource -> False
  Response -> False
  AlterEgoResponse -> False
  HeroResponse -> False
  ForcedResponse -> True
  Action -> False
  HeroAction -> False
  AlterEgoAction -> False
  Special -> False
  Basic -> False
  Setup -> False
  WhenRevealed -> True

defaultAbilityLimit :: AbilityType -> Limit
defaultAbilityLimit = \case
  Interrupt -> PerWindow 1
  AlterEgoInterrupt -> PerWindow 1
  HeroInterrupt -> PerWindow 1
  ForcedInterrupt -> PerWindow 1
  Resource -> NoLimit
  HeroResource -> NoLimit
  AlterEgoResource -> NoLimit
  Response -> PerWindow 1
  AlterEgoResponse -> PerWindow 1
  HeroResponse -> PerWindow 1
  ForcedResponse -> PerWindow 1
  Action -> NoLimit
  HeroAction -> NoLimit
  AlterEgoAction -> NoLimit
  Special -> NoLimit
  Basic -> NoLimit
  Setup -> NoLimit
  WhenRevealed -> PerWindow 1

-- A pure cancel must have a pending reveal whose abilities may be canceled.
canCancelRevealedTreachery :: (HasGame m) => m Bool
canCancelRevealedTreachery = do
  windows <- getCurrentWindows
  anyM (\case
    Window _ (RevealTreachery tid _) -> notElem CannotBeCanceled <$> getModifiers tid
    _ -> pure False) windows

canCancelRevealedEncounterCard :: (HasGame m) => m Bool
canCancelRevealedEncounterCard = do
  windows <- getCurrentWindows
  anyM (\case
    Window _ (EncounterCardRevealed _ _ card) -> encounterCardCanBeCanceled card
    _ -> pure False) windows

-- | The physical event that opened this play window is still in its owner's discard.
playedEventInDiscard :: (HasGame m, Projection m PlayerIdentity) => m Bool
playedEventInDiscard = do
  windows <- getCurrentWindows
  orM $ flip map windows $ \w -> case windowType w of
    EventPlayed who eid _ -> projectP PlayerIdentityDiscard
      (any ((== CardId (unEventId eid)) . pcCardId) . unDiscard) who
    _ -> pure False

-- | A copied removal needs another scheme with threat that the player may remove.
-- ThwartableScheme checks removal restrictions and crisis, without imposing patrol.
hasOtherSchemeForThreatRemoval :: (HasGame m) => m Bool
hasOtherSchemeForThreatRemoval = do
  windows <- getCurrentWindows
  anyM (\case
    Window _ (ThwartRemovedThreat _ scheme _ _) ->
      selectAny (NotScheme (SchemeWithId scheme) <> ThwartableScheme)
    _ -> pure False) windows

-- | Check what remains on the queue after earlier prevention interrupts, rather
-- than the original damage amount snapshotted by the window.
pendingDamageAtLeast :: (HasQueue m) => IdentityId -> Natural -> m Bool
pendingDamageAtLeast ident n = withQueue $ \q ->
  (q, maybe False ((>= n) . damageAmount) $ headMay
    [damage | Msg.IdentityMessage who (Msg.IdentityDamaged _ damage) <- q, who == ident])

hasOtherSchemeForThreatMovement :: HasGame m => m Bool
hasOtherSchemeForThreatMovement = do
  windows <- getCurrentWindows
  anyM (\case
    Window _ (AllyThwart aid scheme) -> (&&) <$> canMoveSchemeThreatFrom aid scheme <*> selectAny (NotScheme $ SchemeWithId scheme)
    _ -> pure False) windows
