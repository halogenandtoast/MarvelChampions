module Marvel.Message.Deck where

import Marvel.Prelude

import Marvel.Id
import Marvel.Ref

data FromZone = FromDeck | FromHand | FromDiscard | RandomFromHand | FromEncounterDeck
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SearchOption
  = SearchTarget Target
  | SearchDrawOne
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ChoiceRules
  = DifferentCards
  | -- | any of the cards, each once
    AnyCards
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data SearchSignifier
  = SearchIdentityDeck IdentityId DeckProjection
  | SearchEncounterDeckAndDiscardPile
  | SearchIdentityDeckAndDiscardPile IdentityId
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
  | -- | the rest on the bottom of the deck, in the order the player chooses
    BottomInAnyOrder
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
