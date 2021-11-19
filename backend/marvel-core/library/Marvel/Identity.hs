{-# LANGUAGE TemplateHaskell #-}
module Marvel.Identity
  ( module Marvel.Identity
  ) where

import Marvel.Prelude

import Data.HashSet qualified as HashSet
import Marvel.Ability
import Marvel.AlterEgo
import Marvel.AlterEgo.Attrs
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.EncounterCard
import Marvel.Card.PlayerCard
import Marvel.Card.Side
import Marvel.Cost
import Marvel.Criteria
import Marvel.Deck
import Marvel.Discard
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hero
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Queue
import Marvel.Source
import Marvel.Target
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
  , playerIdentityMinions :: HashSet MinionId
  , playerIdentitySupports :: HashSet SupportId
  , playerIdentityExhausted :: Bool
  , playerIdentityEncounterCards :: [EncounterCard]
  , playerIdentityDamageReduction :: Natural
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith (suffixedWithFields "playerIdentity") ''PlayerIdentity

isHero :: PlayerIdentity -> Bool
isHero player = case currentIdentity player of
  HeroSide _ -> True
  AlterEgoSide _ -> False

isAlterEgo :: PlayerIdentity -> Bool
isAlterEgo player = case currentIdentity player of
  HeroSide _ -> False
  AlterEgoSide _ -> True

instance Exhaustable PlayerIdentity where
  isExhausted = playerIdentityExhausted

instance HasResources PlayerIdentity where
  resourcesFor player card =
    concatMap (`resourcesFor` card) (unHand $ playerIdentityHand player)

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
  , playerIdentityMinions = mempty
  , playerIdentitySupports = mempty
  , playerIdentityExhausted = False
  , playerIdentityEncounterCards = []
  , playerIdentityDamageReduction = 0
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
  resources <- getAvailableResourcesFor c
  modifiedCost <- getModifiedCost attrs c
  passedCriteria <- checkCriteria (cdCriteria def)
  pure $ length resources >= modifiedCost && passedCriteria && isNothing
    (cdResponseWindow def)
 where
  ident = toId attrs
  def = getCardDef c
  checkCriteria = \case
    IsSelf -> error "Irrelevant"
    NoCriteria -> pure True
    InHeroForm -> member ident <$> select HeroIdentity
    Unexhausted -> member ident <$> select UnexhaustedIdentity
    Criteria xs -> allM checkCriteria xs
    MinionExists m -> selectAny m

getModifiedCost :: MonadGame env m => PlayerIdentity -> PlayerCard -> m Int
getModifiedCost attrs c = do
  modifiers <- getModifiers attrs
  pure $ maybe 0 (modifiedCost modifiers) (cdCost $ getCardDef c)
 where
  modifiedCost ms cost' = foldr applyModifier cost' ms
  applyModifier (ResourceCostReduction n) = max 0 . subtract (fromIntegral n)

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

runIdentityMessage
  :: (MonadGame env m, CoerceRole m)
  => IdentityMessage
  -> PlayerIdentity
  -> m PlayerIdentity
runIdentityMessage msg attrs@PlayerIdentity {..} = case msg of
  SetupIdentity -> do
    pushAll
      $ IdentityMessage (toId attrs)
      <$> [ShuffleDeck, DrawOrDiscardToHandLimit]
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
  DrawCards fromZone n -> case fromZone of
    FromDeck -> do
      let (cards, deck) = splitAt (fromIntegral n) (unDeck playerIdentityDeck)
      pure $ attrs & handL %~ Hand . (<> cards) . unHand & deckL .~ Deck deck
  ReadyCards -> do
    pushAll
      $ map
          (($ ReadiedAlly) . AllyMessage)
          (HashSet.toList playerIdentityAllies)
      <> map
           (($ ReadiedSupport) . SupportMessage)
           (HashSet.toList playerIdentitySupports)
    pure $ attrs & exhaustedL .~ False
  ChooseOtherForm -> do
    let otherForms = filter (/= playerIdentitySide) $ keys playerIdentitySides
    chooseOrRunOne playerIdentityId $ map ChangeToForm otherForms
    pure attrs
  ChangedToForm side -> pure $ attrs & sideL .~ side
  PlayedCard card mWindow -> do
    modifiedCost <- getModifiedCost attrs card
    let cost' = mconcat $ replicate modifiedCost (ResourceCost Nothing)
    push $ SetActiveCost $ ActiveCost
      (toId attrs)
      (ForCard card)
      cost'
      NoPayment
      mWindow
    pure $ attrs & handL %~ Hand . filter (/= card) . unHand
  PaidWithCard card -> do
    push $ Spent card
    pure
      $ attrs
      & (handL %~ Hand . filter (/= card) . unHand)
      & (discardL %~ Discard . (card :) . unDiscard)
  AllyCreated allyId -> do
    pure $ attrs & alliesL %~ HashSet.insert allyId
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
  DiscardCard card ->
    pure $ attrs & (discardL %~ Discard . (card :) . unDiscard)
  DiscardFrom fromZone n mTarget -> case fromZone of
    FromDeck -> do
      let (cards, deck') = splitAt (fromIntegral n) $ unDeck playerIdentityDeck
      for_ mTarget $ \target -> push $ WithDiscarded target fromZone cards
      pure
        $ attrs
        & (discardL %~ Discard . (cards <>) . unDiscard)
        & (deckL .~ Deck deck')
  ExhaustedIdentity -> pure $ attrs & exhaustedL .~ True
  VillainAndMinionsActivate -> do
    villain <- selectJust ActiveVillain
    case currentIdentity attrs of
      HeroSide _ -> push $ VillainMessage villain (VillainAttacks $ toId attrs)
      AlterEgoSide _ -> push $ VillainMessage villain VillainSchemes
    pure attrs
  DealtEncounterCard ec -> pure $ attrs & encounterCardsL %~ (ec :)
  RevealEncounterCards -> do
    pushAll
      $ map (RevealEncounterCard (toId attrs)) playerIdentityEncounterCards
    pure $ attrs & encounterCardsL .~ mempty
  IdentityDamaged _ n -> do
    let
      damage =
        max 0 (fromIntegral n - fromIntegral playerIdentityDamageReduction)
    pure $ attrs & currentHPL %~ HP . max 0 . subtract damage . unHp
  IdentityDefended n -> pure $ attrs & damageReductionL +~ n
  IdentityHealed n ->
    pure
      $ attrs
      & currentHPL
      %~ HP
      . min (unHp playerIdentityMaxHP)
      . (+ fromIntegral n)
      . unHp
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
    IdentityMessage ident msg' | ident == toId attrs ->
      runIdentityMessage msg' attrs
    _ -> pure attrs

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
