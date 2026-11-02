{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.CampaignPlayers.Helpers where

import Marvel.Prelude
import Data.HashMap.Strict qualified as HashMap
import Marvel.Hand
import System.Random.Shuffle (shuffleM)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Key qualified as Key
import Data.Maybe (maybeToList)
import Marvel.Card
import Marvel.CampaignPlayers.Cards qualified as Cards
import Marvel.Cost ()
import Marvel.Damage
import Marvel.Effect.Types
import Marvel.Payment
import Marvel.Question
import Marvel.Modifier
import Marvel.Resource.Types qualified as R
import Marvel.Hero.Runner qualified as Hero
import Marvel.Id
import Marvel.Keyword qualified as K
import Marvel.Status
import Marvel.Window qualified as W
import Marvel.Deck
import Marvel.Discard
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Identity.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Upgrade.Types (Upgrade, Attrs (upgradeAttachedEnemy, upgradeAttachedSideScheme, upgradeAttachedMainScheme), Field (UpgradeAttachedEnemy, UpgradeAttachedSideScheme, UpgradeAttachedMainScheme))

-- Stored on the physical identity face, retaining order across form changes.
identityStoredCards :: HasGame m => IdentityId -> Text -> m [PlayerCard]
identityStoredCards you key = do
  sides <- project PlayerIdentitySides you
  pure $ concatMap (readCards . toJSON) $ toList sides
 where
  readCards = \case
    Object obj -> case KeyMap.lookup "contents" obj of
      Just (Object inner) -> case KeyMap.lookup (Key.fromText key) inner of
        Just value -> case fromJSON value of Success cards -> cards; _ -> []
        _ -> []
      Just (Array arr) -> concatMap readCards (toList arr)
      _ -> case KeyMap.lookup (Key.fromText key) obj of
        Just value -> case fromJSON value of Success cards -> cards; _ -> []
        _ -> concatMap readCards (KeyMap.elems obj)
    Array arr -> concatMap readCards (toList arr)
    _ -> []

identitySenseDeck :: HasGame m => IdentityId -> m [PlayerCard]
identitySenseDeck you = identityStoredCards you "campaignSenseCards"

tuckedPlayerCards :: HasGame m => IdentityId -> m [PlayerCard]
tuckedPlayerCards you = identityStoredCards you "campaignEchoCards"

setupSenseDeck :: ReverseQueue m => IdentityId -> [CardDef] -> m ()
setupSenseDeck you defs = do
  cards <- traverse (\def -> do { cid <- getRandom; pure $ MkPlayerCard cid def (Just you) (Just you) }) defs >>= shuffleM
  cardSpecific you "fneSetupSenseDeck" cards

playFromSenseDeck :: ReverseQueue m => IdentityId -> PlayerCard -> Bool -> m ()
playFromSenseDeck you card ignoreCost = do
  cardSpecific you "fneReserveSensePlay" card
  addToHand you card
  if ignoreCost then playCardIgnoringResourceCost you card else playCardWithReduction you 0 card

tuckPlayerCard :: ReverseQueue m => IdentityId -> PlayerCard -> m ()
tuckPlayerCard you card = do
  removeFromGame card
  cardSpecific you "fneTuckPlayerCard" card

discardTuckedPlayerCard :: ReverseQueue m => IdentityId -> PlayerCard -> m ()
discardTuckedPlayerCard you card = do
  cardSpecific you "fneReleaseTuckedCard" card
  discardCard card

addTuckedPlayerCardToHand :: ReverseQueue m => IdentityId -> PlayerCard -> m ()
addTuckedPlayerCardToHand you card = do
  cardSpecific you "fneReleaseTuckedCard" card
  addToHand you card

playTuckedEvent :: ReverseQueue m => IdentityId -> PlayerCard -> Natural -> Bool -> m ()
playTuckedEvent you card reduction suppressed = do
  cardSpecific you "fneReserveTuckedEvent" (card, suppressed)
  addToHand you card
  playCardWithReduction you reduction card

chooseSense :: ReverseQueue m => IdentityId -> source -> Bool -> m ()
chooseSense you _ ignoreCost = do
  cards <- identitySenseDeck you
  eligible <- filterM (fmap (maybe True notNull) . getPlayTargets you) cards
  chooseOne you $ labeled "$label.done" (pure ()) : [targeting card (playFromSenseDeck you card ignoreCost) | card <- eligible]

searchOwned :: ReverseQueue m => IdentityId -> source -> CardMatcher -> Bool -> m ()
searchOwned you _ matcher includeDiscard = do
  deck <- projectMap PlayerIdentityDeck unDeck you
  discard <- if includeDiscard then projectMap PlayerIdentityDiscard unDiscard you else pure []
  let cards = filter (cardMatch matcher) (deck <> discard)
  chooseOrRunOneSearched you (map PlayerCard cards) $
    labeled "$label.done" (shuffleDeck you) : [targeting card (addToHand you card >> shuffleDeck you) | card <- cards]

shuffleDiscardMatching :: ReverseQueue m => IdentityId -> source -> CardMatcher -> Bool -> m ()
shuffleDiscardMatching you source matcher draw = do
  cards <- filter (cardMatch matcher) <$> projectMap PlayerIdentityDiscard unDiscard you
  chooseOne you $ labeled "$label.done" (pure ()) :
    [targeting card do
       shuffleIntoDeck you [card]
       when draw $ drawCards you 1
    | card <- cards]

upgradesOnEnemy :: HasGame m => EnemyId -> m Natural
upgradesOnEnemy enemy = do
  upgrades <- selectList (UpgradeMatches [])
  fromIntegral . length <$> filterM (fmap (== Just enemy) . getUpgradeAttachedEnemy) upgrades

upgradesOnScheme :: HasGame m => SchemeId -> m Natural
upgradesOnScheme scheme = do
  upgrades <- selectList (UpgradeMatches [])
  fromIntegral . length <$> filterM (fmap (elem scheme) . getUpgradeAttachedSchemes) upgrades

windowEnemies :: Maybe W.WindowType -> [EnemyId]
windowEnemies = maybe [] $ \case
  W.EnemyAttack enemy _ -> [enemy]
  W.EnemyAttackAlly enemy _ _ -> [enemy]
  _ -> []

schemeThreat :: HasGame m => SchemeId -> m Natural
schemeThreat = getSchemeThreatAmount

removeThreatFrom :: (ReverseQueue m, IsRef source) => SchemeId -> source -> Natural -> m ()
removeThreatFrom scheme source n = resolve $ ThwartScheme (schemeRef scheme) (toSource source) n

chooseEnemyStatus :: ReverseQueue m => IdentityId -> StatusCard -> EnemyMatcher -> m ()
chooseEnemyStatus you status matcher = do
  let eligible = case status of StunnedStatus -> matcher <> EnemyCanBeStunned; ConfusedStatus -> matcher <> EnemyCanBeConfused; _ -> matcher
  enemies <- selectList eligible
  chooseOne you [targeting (enemyRef enemy) (giveStatus enemy status) | enemy <- enemies]

identityDefense :: HasGame m => IdentityId -> m Natural
identityDefense you = do
  sides <- project PlayerIdentitySides you
  case [hero | HeroSide hero <- HashMap.elems sides] of
    hero : _ -> Hero.getModifiedDefense $ toAttrs hero
    _ -> pure 0

addKeywordToAttack :: (ReverseQueue m, IsRef source) => IdentityId -> source -> K.Keyword -> m ()
addKeywordToAttack you source keyword = do
  createEffect Cards.cw56005 source $ IdentityEntity $ IdentityWithId you
  cardSpecific source "campaignBasicAttackKeyword" keyword

afterEnemyAttack :: ReverseQueue m => EnemyId -> m () -> m ()
afterEnemyAttack enemy action = do
  messages <- capture action
  replaceMatchingMessage (\msg -> msg : messages) $ \case
    CheckWindows ws -> any (\case W.Window W.After (W.EnemyAttack foe _) -> foe == enemy; W.Window W.After (W.EnemyAttackAlly foe _ _) -> foe == enemy; _ -> False) ws
    _ -> False

-- The temporary marker suppresses only the minion's printed When Defeated.
defeatMinionIgnoringWhenDefeated :: (ReverseQueue m, IsRef source) => IdentityId -> source -> MinionId -> m ()
defeatMinionIgnoringWhenDefeated you source mid = do
  attackEnemyWithoutDamage you mid source
  cardSpecific mid "ignoreWhenDefeated" True
  minionStep mid $ MinionDefeatedWithExcess (toSource source) (toDamage 0 $ FromPlayerAttack you) 0

chooseStatusRemoval :: ReverseQueue m => IdentityId -> m ()
chooseStatusRemoval you = do
  statuses <- filterM (\status -> selectAny $ CharacterWithId (IdentityCharacter you) <> CharacterWithStatus status) [minBound .. maxBound]
  chooseOne you [labeled (statusLabel status) (removeStatus you status) | status <- statuses]

reduceBasicPower :: ReverseQueue m => IdentityId -> Natural -> m ()
reduceBasicPower you n = replaceMatchingMessage change $ \case
  IdentityMessage who (SideMessage _) -> who == you
  ResolveThreatRemoval True _ who _ _ _ -> who == you
  _ -> False
 where
  change = \case
    IdentityMessage who (SideMessage (BasicAttack amount)) -> [IdentityMessage who $ SideMessage $ BasicAttack $ subtractNatural n amount]
    IdentityMessage who (SideMessage (BasicThwart amount)) -> [IdentityMessage who $ SideMessage $ BasicThwart $ subtractNatural n amount]
    ResolveThreatRemoval basic thwarting who target source amount -> [ResolveThreatRemoval basic thwarting who target source $ subtractNatural n amount]
    other -> [other]

reduceAllyBasicPower :: ReverseQueue m => AllyId -> Natural -> m ()
reduceAllyBasicPower aid n = replaceMatchingMessage change $ \case
  AllyMessage who (AllyAttackLands _ _) -> who == aid
  ResolveThreatRemoval True True _ _ (AllyRef who) _ -> who == aid
  _ -> False
 where
  change = \case
    AllyMessage who (AllyAttackLands enemy damage) -> [AllyMessage who $ AllyAttackLands enemy damage {damageAmount = subtractNatural n $ damageAmount damage}]
    ResolveThreatRemoval basic thwarting who target source amount -> [ResolveThreatRemoval basic thwarting who target source $ subtractNatural n amount]
    other -> [other]

gainResources :: ReverseQueue m => [R.Resource] -> m ()
gainResources = resolve . Pay . mconcat . map ResourcePayment

reduceIdentityDamage :: ReverseQueue m => IdentityId -> Natural -> m ()
reduceIdentityDamage = preventDamage

windowEnemyAttacks :: [W.WindowType] -> [EnemyId]
windowEnemyAttacks = concatMap $ \case
  W.EnemyAttack enemy _ -> [enemy]
  W.EnemyAttackAlly enemy _ _ -> [enemy]
  _ -> []

payingForIdentitySpecific :: HasGame m => IdentityId -> m Bool
payingForIdentitySpecific _ = getActiveCost >>= \case
  Nothing -> pure False
  Just acid -> maybe False ((`elem` ["56030","56031","56032","56033","56034","56035","56036","56037","56038","56039","56040"]) . canonicalCardCode . getCardDef) <$> getActiveCostCard acid

becomeDefender :: ReverseQueue m => IdentityId -> EnemyId -> m ()
becomeDefender you enemy = enemyDefendedBy enemy $ IdentityCharacter you

attachedScheme :: Attrs Upgrade -> Maybe SchemeId
attachedScheme attrs = (SchemeSideSchemeId <$> upgradeAttachedSideScheme attrs) <|> (SchemeMainSchemeId <$> upgradeAttachedMainScheme attrs)

attachableTarget :: Target -> Bool
attachableTarget = \case
  MinionRef _ -> True
  SideSchemeRef _ -> True
  SchemeRef (SchemeSideSchemeId _) -> True
  EnemyRef (EnemyMinionId _) -> True
  _ -> False

isExactDefeat :: W.WindowType -> Bool
isExactDefeat = \case
  W.ThwartRemovedThreat _ (SchemeSideSchemeId _) before removed -> before > 0 && before == removed
  _ -> False

removeAccelerationTokenFromPlay :: ReverseQueue m => m ()
removeAccelerationTokenFromPlay = cardSpecific GameRef "campaignRemoveAccelerationToken" ()

moveSenseUpgrade :: ReverseQueue m => IdentityId -> UpgradeId -> m ()
moveSenseUpgrade you uid = do
  controller <- getSourceController $ toSource uid
  targets <- senseMoveTargets uid
  chooseOne you [targeting target do
    upgradeRemoved uid
    detachUpgrade uid
    for_ controller $ \owner -> upgradeCreated owner uid
    case target of
      EnemyRef enemy -> upgradeStep uid $ UpgradeAttachedToEnemy enemy
      MinionRef mid -> upgradeStep uid $ UpgradeAttachedToEnemy $ EnemyMinionId mid
      VillainRef vid -> upgradeStep uid $ UpgradeAttachedToEnemy $ EnemyVillainId vid
      SideSchemeRef sid -> upgradeStep uid $ UpgradeAttachedToSideScheme sid
      MainSchemeRef mid -> upgradeStep uid $ UpgradeAttachedToMainScheme mid
      SchemeRef (SchemeSideSchemeId sid) -> upgradeStep uid $ UpgradeAttachedToSideScheme sid
      SchemeRef (SchemeMainSchemeId mid) -> upgradeStep uid $ UpgradeAttachedToMainScheme mid
      _ -> pure ()
    | target <- targets]

senseMoveTargets :: ReverseQueue m => UpgradeId -> m [Target]
senseMoveTargets uid = do
  controller <- getSourceController $ toSource uid
  card <- getsGame (`cardInPlay` toRef uid)
  enemy <- getUpgradeAttachedEnemy uid
  schemes <- getUpgradeAttachedSchemes uid
  let current = maybeToList (enemyRef <$> enemy) <> map schemeRef schemes
  case (controller,card) of
    (Just owner,Just (PlayerCard physical)) -> do
      targets <- fromMaybe [] <$> getPlayTargets owner physical
      pure $ filter (`notElem` current) targets
    _ -> pure []

nextEventReductionForRound :: (ReverseQueue m, IsRef source) => IdentityId -> source -> Natural -> m ()
nextEventReductionForRound you source n = do
  createEffect Cards.fne60039 source $ IdentityEntity $ IdentityWithId you
  cardSpecific source "campaignNextEventReduction" n

newtype CampaignNextEventEffect = CampaignNextEventEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

campaignNextEventEffect :: CardEffect CampaignNextEventEffect
campaignNextEventEffect = effectWith CampaignNextEventEffect Cards.fne60039 (endsL ?~ DisableAtEndOfRound)

instance RunMessage CampaignNextEventEffect where
  runMessage msg e@(CampaignNextEventEffect attrs) = runQueueT $ case msg of
    CardSpecific target "campaignNextEventReduction" (fromJSON -> Success n)
      | target == refocus (effectSource attrs), null (effectModifiers attrs) ->
          pure $ CampaignNextEventEffect attrs {effectModifiers = [ResourceCostReductionFor (CardWithType EventType) n]}
    IdentityMessage you (PlayedCard card _) -> do
      valid <- effectValidFor attrs $ toRef you
      when (valid && cardMatch (CardWithType EventType) card) $ disableEffect $ effectId attrs
      pure e
    _ -> CampaignNextEventEffect <$> runMessage msg attrs

identityStoredValue :: HasGame m => IdentityId -> Text -> m (Maybe Value)
identityStoredValue you key = do
  sides <- project PlayerIdentitySides you
  pure $ headMay $ concatMap (readValues . toJSON) $ toList sides
 where
  readValues = \case
    Object obj -> maybeToList (KeyMap.lookup (Key.fromText key) obj) <> concatMap readValues (KeyMap.elems obj)
    Array arr -> concatMap readValues (toList arr)
    _ -> []

echoReservedReplay :: HasGame m => IdentityId -> m (Maybe (PlayerCard,PlayerCard))
echoReservedReplay you = identityStoredValue you "campaignEchoReserved" >>= \case
  Just value -> case fromJSON value of Success result -> pure result; _ -> pure Nothing
  _ -> pure Nothing

echoReplayCards :: HasGame m => IdentityId -> m [PlayerCard]
echoReplayCards you = do
  hero <- identityMatches (HeroIdentity <> IdentityWithTitle "Echo") you
  blanked <- isBlanked you
  hand <- projectMap PlayerIdentityHand unHand you
  if hero && not blanked && any ((`elem` ["60040a","60040b","60040c"]) . toCardCode) hand
    then tuckedPlayerCards you
    else pure []

echoReplayDiscount :: HasGame m => IdentityId -> PlayerCard -> m Natural
echoReplayDiscount you card = do
  reserved <- echoReservedReplay you
  offered <- echoReplayCards you
  pure $ if maybe False ((== pcCardId card) . pcCardId . fst) reserved || card `elem` offered then 2 else 0

stickPowerPenalty :: (ReverseQueue m, IsRef source) => IdentityId -> source -> [W.WindowType] -> m ()
stickPowerPenalty you source windows = do
  let modifiers = concatMap (\case
        W.MadeBasicAttack who | who == you -> [AttackPenalty 1]
        W.MadeBasicThwart who | who == you -> [ThwartPenalty 1]
        W.MadeBasicDefense who _ | who == you -> [DefensePenalty 1]
        W.UsedRecovery who | who == you -> [RecoveryPenalty 1]
        _ -> []) windows
  unless (null modifiers) do
    createEffect Cards.fne60029 source $ IdentityEntity $ IdentityWithId you
    cardSpecific source "campaignStickPenalty" modifiers

newtype CampaignStickEffect = CampaignStickEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

campaignStickEffect :: CardEffect CampaignStickEffect
campaignStickEffect = effectWith CampaignStickEffect Cards.fne60029 (endsL ?~ DisableAtEndOfTurn)

instance RunMessage CampaignStickEffect where
  runMessage msg e@(CampaignStickEffect attrs) = runQueueT $ case msg of
    CardSpecific target "campaignStickPenalty" (fromJSON -> Success modifiers)
      | target == refocus (effectSource attrs), null (effectModifiers attrs) -> pure $ CampaignStickEffect attrs {effectModifiers = modifiers}
    CheckWindows windows -> do
      let players = [who | W.Window W.After window <- windows, who <- case window of
            W.MadeBasicAttack who -> [who]
            W.MadeBasicThwart who -> [who]
            W.MadeBasicDefense who _ -> [who]
            W.UsedRecovery who -> [who]
            _ -> []]
      whenM (anyM (effectValidFor attrs . IdentityRef) players) $ disableEffect $ effectId attrs
      pure e
    _ -> CampaignStickEffect <$> runMessage msg attrs

identitySensePending :: HasGame m => IdentityId -> m (Maybe (PlayerCard,Int))
identitySensePending you = identityStoredValue you "campaignSensePending" >>= \case
  Just value -> case fromJSON value of Success result -> pure result; _ -> pure Nothing
  _ -> pure Nothing

newtype CampaignBasicKeywordEffect = CampaignBasicKeywordEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

campaignBasicKeywordEffect :: CardEffect CampaignBasicKeywordEffect
campaignBasicKeywordEffect = effectWith CampaignBasicKeywordEffect Cards.cw56005 (endsL ?~ DisableAtEndOfTurn)

instance RunMessage CampaignBasicKeywordEffect where
  runMessage msg e@(CampaignBasicKeywordEffect attrs) = runQueueT $ case msg of
    CardSpecific target "campaignBasicAttackKeyword" (fromJSON -> Success keyword)
      | target == refocus (effectSource attrs), null (effectModifiers attrs) -> pure $ CampaignBasicKeywordEffect attrs {effectModifiers = [BasicAttackKeyword keyword]}
    CheckWindows windows -> do
      let players = [who | W.Window W.After (W.MadeBasicAttack who) <- windows]
      whenM (anyM (effectValidFor attrs . IdentityRef) players) $ disableEffect $ effectId attrs
      pure e
    _ -> CampaignBasicKeywordEffect <$> runMessage msg attrs

schemeTarget :: SchemeId -> Target
schemeTarget = schemeRef

-- Printed attachment permissions, independent of whether a legal host is presently
-- in play. In particular Hunted and Stakeout need no current minion/side scheme.
canTuckUnderContingency :: HasCardDef card => card -> Bool
canTuckUnderContingency = cardMatch contingencyAttachmentMatcher

contingencyAttachmentMatcher :: CardMatcher
contingencyAttachmentMatcher = CardWithType UpgradeType <> CardMatchesAny
  (map CardWithTitle
    [ "Spider-Tracer", "Webbed Up", "Followed", "Death-Glow", "Chance Encounter", "Overwatch", "Phased and Confused", "Gatekeeper", "Exploit Weakness", "Practiced Defense", "Priority Target", "Coordinated Attack", "Marked", "Befuddle", "Pinned Down", "Mental Paralysis", "Mind Control", "Containment Strategy", "Puncture Wound", "The Direct Approach", "Ambush", "Distraction", "Frozen Solid", "Suppressing Fire", "Bamf!", "Under Control", "Wrapped in Metal", "Hunted", "Caught in the Crossfire", "Coordinated Effort", "Acute Tactility", "Enhanced Olfaction", "Heightened Hearing", "Radar Sense", "Superior Taste", "Legal Trouble", "Dance with the Devil", "Stakeout", "Cruisin' for a Bruisin'" ])
