{-# LANGUAGE TemplateHaskell #-}
module Marvel.Identity
  ( module Marvel.Identity
  ) where

import Marvel.Prelude

import Data.HashSet qualified as HashSet
import Marvel.Ability
import Marvel.AlterEgo
import Marvel.AlterEgo.Attrs
import Marvel.Attack
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.EncounterCard
import Marvel.Card.PlayerCard
import Marvel.Card.Side
import Marvel.Cost
import Marvel.Criteria
import Marvel.Deck
import Marvel.Discard
import Marvel.EncounterCard
import Marvel.EncounterSet
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Matchers qualified as Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Window qualified as W
import System.Random.Shuffle

data PlayerIdentitySide = HeroSide Hero | AlterEgoSide AlterEgo
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Player Identity
-- An Identity is either a Hero or an alter ego
data PlayerIdentity = PlayerIdentity
  { playerIdentityId :: IdentityId
  , playerIdentitySide :: Side
  , playerIdentitySides :: HashMap Side PlayerIdentitySide
  , playerIdentityStartingHP :: HP GameValue
  , playerIdentityMaxHP :: HP Int
  , playerIdentityCurrentHP :: HP Int
  , playerIdentityDeck :: Deck
  , playerIdentityDiscard :: Discard
  , playerIdentityHand :: Hand
  , playerIdentityPassed :: Bool
  , playerIdentityAllies :: HashSet AllyId
  , playerIdentityAllyLimit :: Natural
  , playerIdentityMinions :: HashSet MinionId
  , playerIdentitySupports :: HashSet SupportId
  , playerIdentityUpgrades :: HashSet UpgradeId
  , playerIdentityExhausted :: Bool
  , playerIdentityEncounterCards :: [EncounterCard]
  , playerIdentityDamageReduction :: Natural
  , playerIdentityStunned :: Bool
  , playerIdentityConfused :: Bool
  , playerIdentityTough :: Bool
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith (suffixedWithFields "playerIdentity") ''PlayerIdentity

identityIsStunned :: PlayerIdentity -> Bool
identityIsStunned = playerIdentityStunned

identityIsConfused :: PlayerIdentity -> Bool
identityIsConfused = playerIdentityConfused

isHero :: PlayerIdentity -> Bool
isHero player = case currentIdentity player of
  HeroSide _ -> True
  AlterEgoSide _ -> False

isAlterEgo :: PlayerIdentity -> Bool
isAlterEgo player = case currentIdentity player of
  HeroSide _ -> False
  AlterEgoSide _ -> True

identityDamage :: PlayerIdentity -> Natural
identityDamage attrs =
  fromIntegral . max 0 $ unHp (playerIdentityMaxHP attrs) - unHp
    (playerIdentityCurrentHP attrs)

instance Exhaustable PlayerIdentity where
  isExhausted = playerIdentityExhausted

instance HasResources PlayerIdentity where
  resourcesFor player card =
    concatMapM (`resourcesFor` card) (unHand $ playerIdentityHand player)

instance ToJSON PlayerIdentity where
  toJSON = genericToJSON $ aesonOptions $ Just "playerIdentity"

instance FromJSON PlayerIdentity where
  parseJSON = genericParseJSON $ aesonOptions $ Just "playerIdentity"

instance IsSource PlayerIdentity where
  toSource = IdentitySource . playerIdentityId

instance IsTarget PlayerIdentity where
  toTarget = IdentityTarget . playerIdentityId

currentIdentity :: PlayerIdentity -> PlayerIdentitySide
currentIdentity a =
  case lookup (playerIdentitySide a) (playerIdentitySides a) of
    Nothing -> error "Should not happen"
    Just s -> s

createIdentity
  :: IdentityId -> PlayerIdentitySide -> PlayerIdentitySide -> PlayerIdentity
createIdentity ident alterEgoSide heroSide = PlayerIdentity
  { playerIdentitySide = B
  , playerIdentitySides = fromList [(A, heroSide), (B, alterEgoSide)]
  , playerIdentityId = ident
  , playerIdentityStartingHP = hp
  , playerIdentityCurrentHP = HP $ gameValue (unHp hp) 0
  , playerIdentityMaxHP = HP $ gameValue (unHp hp) 0
  , playerIdentityDeck = Deck []
  , playerIdentityDiscard = Discard []
  , playerIdentityHand = Hand []
  , playerIdentityPassed = False
  , playerIdentityAllies = mempty
  , playerIdentityAllyLimit = 3
  , playerIdentityMinions = mempty
  , playerIdentitySupports = mempty
  , playerIdentityUpgrades = mempty
  , playerIdentityExhausted = False
  , playerIdentityEncounterCards = []
  , playerIdentityDamageReduction = 0
  , playerIdentityStunned = False
  , playerIdentityConfused = False
  , playerIdentityTough = False
  }
 where
  hp = case alterEgoSide of
    AlterEgoSide x -> startingHP x
    HeroSide x -> startingHP x

setDeck :: Deck -> PlayerIdentity -> PlayerIdentity
setDeck deck player = player & deckL .~ deck'
 where
  ident = toId player
  deck' = Deck $ map
    (\c -> c { pcOwner = Just ident, pcController = Just ident })
    (unDeck deck)

lookupAlterEgo :: CardDef -> IdentityId -> Maybe PlayerIdentitySide
lookupAlterEgo cardDef ident =
  AlterEgoSide <$> (lookup (toCardCode cardDef) allAlterEgos <*> Just ident)

lookupHero :: CardDef -> IdentityId -> Maybe PlayerIdentitySide
lookupHero cardDef ident =
  HeroSide <$> (lookup (toCardCode cardDef) allHeroes <*> Just ident)

takeTurn :: MonadGame env m => PlayerIdentity -> m ()
takeTurn attrs =
  pushAll $ map (IdentityMessage $ toId attrs) [PlayerTurnOption, CheckIfPassed]

instance HasAbilities PlayerIdentity where
  getAbilities a =
    let
      sideAbilities = case currentIdentity a of
        HeroSide x -> getAbilities x
        AlterEgoSide x -> getAbilities x
    in
      [limitedAbility a 100 (PerTurn 1) Action IsSelf NoCost ChangeForm]
        <> sideAbilities

isPlayable :: MonadGame env m => PlayerIdentity -> PlayerCard -> m Bool
isPlayable attrs c = do
  resources <- getAvailableResourcesFor (Just c)
  modifiedCost <- getModifiedCost attrs c
  passedCriteria <- checkCriteria (cdCriteria def <> toAdditionalCriteria def)
  pure
    $ length resources
    >= modifiedCost
    && passedCriteria
    && isNothing (cdResponseWindow def)
    && (cdCardType def /= ResourceType)
 where
  ident = toId attrs
  def = getCardDef c
  checkCriteria = \case
    IsSelf -> error "Irrelevant"
    OwnsThis -> error "Irrelevant"
    NoCriteria -> pure True
    Never -> pure False
    SelfMatches identityMatcher ->
      member ident <$> select (IdentityWithId ident <> identityMatcher)
    InHeroForm -> member ident <$> select HeroIdentity
    InAlterEgoForm -> member ident <$> select AlterEgoIdentity
    Unexhausted -> member ident <$> select UnexhaustedIdentity
    Exhausted -> member ident <$> select Matchers.ExhaustedIdentity
    Criteria xs -> allM checkCriteria xs
    MinionExists m -> selectAny m
    EnemyExists m -> selectAny m
    CharacterExists m -> selectAny m
    AllyExists m -> selectAny m
    SchemeExists m -> selectAny m
    ExtendedCardExists m -> selectAny (NotCard c <> m)
    -- ^ this is critical and order matters to avoid infinite recursion

getModifiedCost :: MonadGame env m => PlayerIdentity -> PlayerCard -> m Int
getModifiedCost attrs c = do
  modifiers <- getModifiers attrs
  pure $ maybe 0 (modifiedCost modifiers) (cdCost $ getCardDef c)
 where
  modifiedCost ms cost' = foldr applyModifier cost' ms
  applyModifier (ResourceCostReduction n) = max 0 . subtract (fromIntegral n)
  applyModifier _ = id

getPlayableCards :: MonadGame env m => PlayerIdentity -> m [PlayerCard]
getPlayableCards player = filterM (isPlayable player) cards
  where cards = unHand $ playerIdentityHand player

getChoices :: MonadGame env m => PlayerIdentity -> m [Choice]
getChoices attrs = do
  let ident = toId attrs
  abilities <- getsGame getAbilities
  usedAbilities <- getUsedAbilities
  playableCards <- getPlayableCards attrs
  validAbilities <- filterM
    (andM . sequence
      [ pure . passesUseLimit ident usedAbilities
      , passesCriteria ident
      , passesCanAffordCost ident
      , pure . passesTiming ident
      , passesTypeIsRelevant ident
      ]
    )
    abilities
  pure
    $ map UseAbility validAbilities
    <> map (($ Nothing) . PlayCard) playableCards

getObligations :: MonadRandom m => PlayerIdentity -> m [EncounterCard]
getObligations attrs = case currentIdentity attrs of
  HeroSide _ -> error "Can not call via hero side"
  AlterEgoSide side ->
    traverse genEncounterCard (alterEgoObligations $ toAttrs side)

runIdentityMessage
  :: (MonadGame env m, CoerceRole m)
  => IdentityMessage
  -> PlayerIdentity
  -> m PlayerIdentity
runIdentityMessage msg attrs@PlayerIdentity {..} = case msg of
  SetupIdentity -> do
    let nemesisSet = getNemesisSet $ toCardCode attrs
    obligations <- getObligations attrs
    nemesisSetCards <- gatherEncounterSet nemesisSet
    pushAll
      $ SetAside nemesisSetCards
      : ShuffleIntoEncounterDeck obligations
      : map
          (IdentityMessage (toId attrs))
          [ShuffleDeck, DrawOrDiscardToHandLimit]
    pure attrs
  BeginTurn -> do
    takeTurn attrs
    pure $ attrs & passedL .~ False
  CheckIfPassed -> do
    unless playerIdentityPassed (takeTurn attrs)
    pure attrs
  PlayerTurnOption -> do
    choices <- getChoices attrs
    chooseOne (toId attrs) (EndTurn : choices)
    pure attrs
  EndedTurn -> do
    push (IdentityEndedTurn $ toId attrs)
    pure $ attrs & passedL .~ True
  ShuffleDeck -> do
    deck <- shuffleM (unDeck $ attrs ^. deckL)
    pure $ attrs & deckL .~ Deck deck
  ShuffleIdentityDiscardBackIntoDeck -> do
    deck' <- shuffleM (unDeck (attrs ^. deckL) <> unDiscard (attrs ^. discardL))
    pure $ attrs & deckL .~ Deck deck' & discardL .~ Discard []
  DrawOrDiscardToHandLimit -> do
    let
      diff = fromIntegral (unHandSize $ handSize attrs)
        - length (unHand playerIdentityHand)
    when
      (diff > 0)
      (push $ IdentityMessage (toId attrs) $ DrawCards
        FromDeck
        (fromIntegral diff)
      )
    pure attrs
  DrawToHandLimit -> do
    let
      diff = fromIntegral (unHandSize $ handSize attrs)
        - length (unHand playerIdentityHand)
    when
      (diff > 0)
      (push $ IdentityMessage (toId attrs) $ DrawCards
        FromDeck
        (fromIntegral diff)
      )
    pure attrs
  DrawCards fromZone n -> case fromZone of
    FromHand -> error "Impossible"
    RandomFromHand -> error "Impossible"
    FromDeck -> do
      let (cards, deck) = splitAt (fromIntegral n) (unDeck playerIdentityDeck)
      when (length (unDeck playerIdentityDeck) < fromIntegral n) $ pushAll
        [ IdentityMessage (toId attrs) ShuffleIdentityDiscardBackIntoDeck
        , DealEncounterCard (toId attrs)
        , IdentityMessage (toId attrs) $ DrawCards
          fromZone
          (subtractNatural (fromIntegral (length (unDeck playerIdentityDeck))) n
          )
        ]
      pure $ attrs & handL %~ Hand . (<> cards) . unHand & deckL .~ Deck deck
  ReadyCards -> do
    pushAll
      $ map
          (($ ReadiedAlly) . AllyMessage)
          (HashSet.toList playerIdentityAllies)
      <> map
           (($ ReadiedSupport) . SupportMessage)
           (HashSet.toList playerIdentitySupports)
      <> map
           (($ ReadiedUpgrade) . UpgradeMessage)
           (HashSet.toList playerIdentityUpgrades)
    pure $ attrs & exhaustedL .~ False
  ChooseOtherForm -> do
    let otherForms = filter (/= playerIdentitySide) $ keys playerIdentitySides
    chooseOrRunOne playerIdentityId $ map ChangeToForm otherForms
    pure attrs
  ChangedToForm side -> do
    push $ CheckWindows [W.Window W.After $ W.IdentityChangesForm (toId attrs)]
    pure $ attrs & sideL .~ side
  PlayedCard card mWindow -> do
    modifiedCost <- getModifiedCost attrs card
    let cost' = mconcat $ replicate modifiedCost (ResourceCost Nothing)
    push $ SetActiveCost $ ActiveCost
      (toId attrs)
      (ForCard card)
      cost'
      NoPayment
      mWindow
    pure
      $ attrs
      & (handL %~ Hand . filter (/= card) . unHand)
      & (discardL %~ Discard . filter (/= card) . unDiscard)
  PaidWithCard card -> do
    push $ Spent card
    pure
      $ attrs
      & (handL %~ Hand . filter (/= card) . unHand)
      & (discardL %~ Discard . (card :) . unDiscard)
  AllyCreated allyId -> do
    push $ IdentityMessage (toId attrs) CheckAllyLimit
    pure $ attrs & alliesL %~ HashSet.insert allyId
  CheckAllyLimit -> do
    limit <- fromIntegral <$> getModifiedAllyLimit attrs
    when (size playerIdentityAllies > limit) $ chooseOne
      (toId attrs)
      [ TargetLabel (AllyTarget aid) [DiscardTarget $ AllyTarget aid]
      | aid <- toList playerIdentityAllies
      ]
    pure attrs
  UpgradeCreated upgradeId -> do
    pure $ attrs & upgradesL %~ HashSet.insert upgradeId
  AllyRemoved allyId -> do
    pure $ attrs & alliesL %~ HashSet.delete allyId
  MinionEngaged minionId -> do
    pure $ attrs & minionsL %~ HashSet.insert minionId
  MinionDisengaged minionId -> do
    pure $ attrs & minionsL %~ HashSet.delete minionId
  SupportCreated supportId -> do
    pure $ attrs & supportsL %~ HashSet.insert supportId
  SupportRemoved supportId -> do
    pure $ attrs & supportsL %~ HashSet.delete supportId
  AddToHand card ->
    pure
      $ attrs
      & (handL %~ Hand . (card :) . unHand)
      & (discardL %~ Discard . filter (/= card) . unDiscard)
  DiscardFor _ FromDeck _ _ -> error "Unhandled"
  DiscardedFor _ FromDeck _ _ _ -> error "Unhandled"
  DiscardFor target FromHand discardMin discardMax -> do
    push
      (IdentityMessage (toId attrs)
      $ DiscardedFor target FromHand discardMin discardMax []
      )
    pure attrs
  DiscardedFor target FromHand discardMin discardMax cards -> do
    if length cards >= fromIntegral discardMax
      then push (WithDiscarded target FromHand cards)
      else
        push
        $ Ask (toId attrs)
        $ ChooseOne
        $ [ Label "Done" [Run [WithDiscarded target FromHand cards]]
          | length cards >= fromIntegral discardMin
          ]
        <> [ TargetLabel
               (CardIdTarget $ pcCardId c)
               [ Run $ map
                   (IdentityMessage (toId attrs))
                   [ DiscardCard c
                   , DiscardedFor
                     target
                     FromHand
                     discardMin
                     discardMax
                     (c : cards)
                   ]
               ]
           | c <- unHand playerIdentityHand
           ]
    pure attrs
  DiscardFor target RandomFromHand discardMin _ -> do
    let handCards = unHand playerIdentityHand
    discards <- take (fromIntegral discardMin) <$> shuffleM handCards
    pushAll
      $ map (IdentityMessage (toId attrs) . DiscardCard) discards
      <> [WithDiscarded target RandomFromHand discards]
    pure attrs
  DiscardedFor _ RandomFromHand _ _ _ -> error "Can not be called"
  DiscardCards -> do
    unless (null $ unHand playerIdentityHand) $ do
      chooseOne (toId attrs)
        $ Label "Continue without discarding" []
        : [ TargetLabel
              (CardIdTarget $ pcCardId c)
              [ Run $ map
                  (IdentityMessage (toId attrs))
                  [DiscardCard c, DiscardCards]
              ]
          | c <- unHand playerIdentityHand
          ]
    pure attrs
  DiscardCard card ->
    pure
      $ attrs
      & (discardL %~ Discard . (card :) . unDiscard)
      & (handL %~ Hand . filter (/= card) . unHand)
  DiscardFrom fromZone n mTarget -> case fromZone of
    FromHand -> error "Unhandled"
    RandomFromHand -> do
      discards <- take (fromIntegral n) <$> shuffleM (unHand playerIdentityHand)
      for_ mTarget $ \target -> push $ WithDiscarded target fromZone discards
      pure
        $ attrs
        & (discardL %~ Discard . (discards <>) . unDiscard)
        & (handL %~ Hand . filter (`notElem` discards) . unHand)
    FromDeck -> do
      let (cards, deck') = splitAt (fromIntegral n) $ unDeck playerIdentityDeck
      for_ mTarget $ \target -> push $ WithDiscarded target fromZone cards
      pure
        $ attrs
        & (discardL %~ Discard . (cards <>) . unDiscard)
        & (deckL .~ Deck deck')
  ReadyIdentity -> pure $ attrs & exhaustedL .~ False
  ExhaustedIdentity -> pure $ attrs & exhaustedL .~ True
  VillainAndMinionsActivate -> do
    villain <- selectJust ActiveVillain
    case currentIdentity attrs of
      HeroSide _ -> do
        pushAll
          $ VillainMessage villain (VillainAttacks $ toId attrs)
          : [ Ask (toId attrs) $ ChooseOneAtATime $ map
                (\minionId -> TargetLabel
                  (MinionTarget minionId)
                  [Run [MinionMessage minionId (MinionAttacks $ toId attrs)]]
                )
                (toList playerIdentityMinions)
            | not (null playerIdentityMinions)
            ]
      AlterEgoSide _ -> do
        pushAll
          $ VillainMessage villain VillainSchemes
          : [ Ask (toId attrs) $ ChooseOneAtATime $ map
                (\minionId -> TargetLabel
                  (MinionTarget minionId)
                  [Run [MinionMessage minionId MinionSchemes]]
                )
                (toList playerIdentityMinions)
            | not (null playerIdentityMinions)
            ]
    pure attrs
  DealtEncounterCard ec -> pure $ attrs & encounterCardsL %~ (ec :)
  RevealEncounterCards -> do
    pushAll
      $ map (RevealEncounterCard (toId attrs)) playerIdentityEncounterCards
    pure $ attrs & encounterCardsL .~ mempty
  IdentityWasAttacked attack' -> do
    let
      damage =
        subtractNatural playerIdentityDamageReduction $ attackDamage attack'
    when
      (damage > 0)
      (pushAll
        [ CheckWindows
          [ W.Window W.When
              $ W.IdentityTakeDamage (toId attrs) W.FromAttack damage
          ]
        , IdentityMessage (toId attrs)
          $ IdentityDamaged (attackSource attack') damage
        ]
      )
    pure $ attrs & damageReductionL .~ 0
  IdentityDamaged _ damage -> do
    pure
      $ attrs
      & (currentHPL
        %~ HP
        . fromIntegral
        . subtractNatural damage
        . fromIntegral
        . unHp
        )
  IdentityDefended n -> pure $ attrs & damageReductionL +~ n
  IdentityHealed n ->
    pure
      $ attrs
      & currentHPL
      %~ HP
      . min (unHp playerIdentityMaxHP)
      . (+ fromIntegral n)
      . unHp
  IdentityStunned -> pure $ attrs & stunnedL .~ True
  IdentityConfused -> pure $ attrs & confusedL .~ True
  IdentityRemoveStunned -> pure $ attrs & stunnedL .~ False
  IdentityRemoveConfused -> pure $ attrs & confusedL .~ False
  SideMessage _ -> case currentIdentity attrs of
    HeroSide x -> do
      newSide <-
        HeroSide <$> runMessage (IdentityMessage playerIdentityId msg) x
      pure $ attrs & sidesL . at playerIdentitySide ?~ newSide
    AlterEgoSide x -> do
      newSide <-
        AlterEgoSide <$> runMessage (IdentityMessage playerIdentityId msg) x
      pure $ attrs & sidesL . at playerIdentitySide ?~ newSide


instance RunMessage PlayerIdentity where
  runMessage msg attrs = case msg of
    UpgradeRemoved upgradeId -> do
      pure $ attrs & upgradesL %~ HashSet.delete upgradeId
    IdentityMessage ident msg' | ident == toId attrs ->
      runIdentityMessage msg' attrs
    _ -> case currentIdentity attrs of
      HeroSide x -> do
        newSide <- HeroSide <$> runMessage msg x
        pure $ attrs & sidesL . at (playerIdentitySide attrs) ?~ newSide
      AlterEgoSide x -> do
        newSide <- AlterEgoSide <$> runMessage msg x
        pure $ attrs & sidesL . at (playerIdentitySide attrs) ?~ newSide

instance HasStartingHP PlayerIdentity where
  startingHP a = case currentIdentity a of
    HeroSide x -> startingHP x
    AlterEgoSide x -> startingHP x

instance HasHandSize PlayerIdentity where
  handSize a = case currentIdentity a of
    HeroSide x -> handSize x
    AlterEgoSide x -> handSize x

instance HasCardCode PlayerIdentity where
  toCardCode a = case currentIdentity a of
    HeroSide x -> toCardCode x
    AlterEgoSide x -> toCardCode x

instance Entity PlayerIdentity where
  type EntityId PlayerIdentity = IdentityId
  type EntityAttrs PlayerIdentity = PlayerIdentity
  toId = playerIdentityId
  toAttrs = id

getModifiedAllyLimit :: MonadGame env m => PlayerIdentity -> m Natural
getModifiedAllyLimit attrs = do
  modifiers <- getModifiers attrs
  pure $ foldr applyModifier (playerIdentityAllyLimit attrs) modifiers
 where
  applyModifier (AllyLimitModifier n) = max 0 . (+ fromIntegral n)
  applyModifier _ = id
