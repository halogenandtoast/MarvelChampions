module Marvel.Message.Deck where

import Marvel.Prelude

import Marvel.Id
import Marvel.Target

data FromZone = FromDeck | FromHand | FromDiscard | RandomFromHand | FromEncounterDeck
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SearchOption
  = SearchTarget Target
  | SearchDrawOne
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ChoiceRules = DifferentCards
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SearchSignifier
  = SearchIdentityDeck IdentityId DeckProjection
  | SearchEncounterDeckAndDiscardPile
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DeckProjection
  = AllOfDeck
  | TopOfDeck Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ReturnOption
  = ShuffleBackIn
  | DiscardRest
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

