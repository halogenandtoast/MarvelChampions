{-# LANGUAGE TemplateHaskell #-}
module Marvel.Identity
  ( module Marvel.Identity
  ) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.AlterEgo
import Marvel.AlterEgo.Attrs
import Marvel.Card.Code
import Marvel.Card.Def
import Marvel.Card.Side
import Marvel.Deck
import Marvel.Entity
import Marvel.Exception
import {-# SOURCE #-} Marvel.Game
import Marvel.Hand
import Marvel.Hero
import Marvel.Message
import Marvel.Question
import Marvel.Queue
import Marvel.Source

data PlayerIdentitySide = HeroSide Hero | AlterEgoSide AlterEgo
  deriving stock (Show, Eq, Generic)

-- | Player Identity
-- An Identity is either a Hero or an alter ego
data PlayerIdentity = PlayerIdentity
  { playerIdentityId :: IdentityId
  , playerIdentitySide :: Side
  , playerIdentitySides :: HashMap Side PlayerIdentitySide
  , playerIdentityStartingHP :: HP
  , playerIdentityMaxHP :: HP
  , playerIdentityCurrentHP :: HP
  , playerIdentityDeck :: Deck
  , playerIdentityHand :: Hand
  , playerIdentityPassed :: Bool
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith (suffixedWithFields "playerIdentity") ''PlayerIdentity

instance IsSource PlayerIdentity where
  toSource = IdentitySource . playerIdentityId

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
  , playerIdentityCurrentHP = hp
  , playerIdentityMaxHP = hp
  , playerIdentityDeck = Deck []
  , playerIdentityHand = Hand []
  , playerIdentityPassed = False
  }
 where
  hp = case alterEgoSide of
    AlterEgoSide x -> startingHP x
    HeroSide x -> startingHP x

setDeck :: Deck -> PlayerIdentity -> PlayerIdentity
setDeck deck = deckL .~ deck

lookupAlterEgo :: CardDef -> IdentityId -> Maybe PlayerIdentitySide
lookupAlterEgo cardDef ident =
  AlterEgoSide <$> (lookup (toCardCode cardDef) allAlterEgos <*> Just ident)

lookupHero :: CardDef -> IdentityId -> Maybe PlayerIdentitySide
lookupHero cardDef ident =
  HeroSide <$> (lookup (toCardCode cardDef) allHeroes <*> Just ident)

takeTurn :: MonadGame env m => PlayerIdentity -> m PlayerIdentity
takeTurn attrs = do
  pushAll $ map (IdentityMessage $ toId attrs) [PlayerTurnOption, CheckIfPassed]
  pure attrs

instance HasAbilities PlayerIdentity where
  getAbilities a =
    let
      sideAbilities = case currentIdentity a of
        HeroSide x -> getAbilities x
        AlterEgoSide x -> getAbilities x
    in [limitedAbility a (PerTurn 1) Action IsSelf ChangeForm] <> sideAbilities

getChoices :: MonadGame env m => PlayerIdentity -> m [Choice]
getChoices attrs = do
  let ident = toId attrs
  abilities <- getsGame getAbilities
  usedAbilities <- getUsedAbilities
  let
    validAbilities = filter
      (and . sequence
        [ passesUseLimit ident usedAbilities
        , passesCriteria ident
        , passesTiming ident
        , passesTypeIsRelevant ident
        ]
      )
      abilities
  pure $ map UseAbility validAbilities

runIdentityMessage
  :: (MonadGame env m, CoerceRole m)
  => IdentityMessage
  -> PlayerIdentity
  -> m PlayerIdentity
runIdentityMessage msg attrs@PlayerIdentity {..} = case msg of
  BeginTurn -> takeTurn attrs
  CheckIfPassed -> if playerIdentityPassed then pure attrs else takeTurn attrs
  SetDeck deck -> pure $ attrs & deckL .~ deck
  PlayerTurnOption -> do
    choices <- getChoices attrs
    chooseOne (toId attrs) (EndTurn : choices)
    pure attrs
  EndedTurn -> do
    pure $ attrs & passedL .~ True
  DrawStartingHand (HandSize n) -> do
    let (hand, deck) = splitAt n (unDeck playerIdentityDeck)
    pure $ attrs & handL .~ Hand hand & deckL .~ Deck deck
  SetupIdentity ->
    throwM $ UnhandledMessage "SetupIdentity must be handled by AlterEgo/Hero"
  ChooseOtherForm -> do
    let otherForms = keys playerIdentitySides
    chooseOrRunOne playerIdentityId $ map ChangeToForm otherForms
    pure attrs
  RanAbility _ ->
    throwM $ UnhandledMessage "RanAbility must be handled by AlterEgo/Hero"
  ChangedToForm _ ->
    throwM $ UnhandledMessage "ChangedToForm must be handled by PlayerIdentity"
  SideMessage other -> case currentIdentity attrs of
    HeroSide x -> do
      newSide <-
        HeroSide <$> runMessage (IdentityMessage playerIdentityId other) x
      pure $ attrs & sidesL . at playerIdentitySide ?~ newSide
    AlterEgoSide x -> do
      newSide <-
        AlterEgoSide <$> runMessage (IdentityMessage playerIdentityId other) x
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

instance HasCardCode PlayerIdentity where
  toCardCode a = case currentIdentity a of
    HeroSide x -> toCardCode x
    AlterEgoSide x -> toCardCode x

instance Entity PlayerIdentity where
  type EntityId PlayerIdentity = IdentityId
  type EntityAttrs PlayerIdentity = PlayerIdentity
  toId = playerIdentityId
  toAttrs = id
