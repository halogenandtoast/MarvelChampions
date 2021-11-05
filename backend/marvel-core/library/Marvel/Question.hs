module Marvel.Question where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Card.Side
import Marvel.Exception
import Marvel.Game.Source
import Marvel.Id
import {-# SOURCE #-} Marvel.Message
import Marvel.Queue
import Marvel.Resource
import Marvel.Target

data Question
  = ChooseOne [Choice]
  | ChoosePlayerOrder (Unsorted IdentityId) (Sorted IdentityId)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Sorted a = Sorted { unSorted :: [a] }
  deriving newtype (Show, Semigroup, Monoid, Eq, ToJSON, FromJSON)

newtype Unsorted a = Unsorted { unUnsorted :: [a] }
  deriving newtype (Show, Semigroup, Monoid, Eq, ToJSON, FromJSON)

data Choice
  = CardLabel CardCode Choice
  | EndTurn
  | UseAbility Ability
  | RunAbility Target Natural
  | ChangeForm
  | ChangeToForm Side
  | GenerateResources [Resource]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

choiceMessages :: IdentityId -> Choice -> [Message]
choiceMessages ident = \case
  CardLabel _ choice -> choiceMessages ident choice
  EndTurn -> [IdentityMessage ident EndedTurn]
  UseAbility a -> UsedAbility ident a : choiceMessages ident (abilityChoice a)
  RunAbility target n -> [RanAbility target n]
  ChangeForm -> [IdentityMessage ident ChooseOtherForm]
  ChangeToForm x -> [IdentityMessage ident $ ChangedToForm x]
  GenerateResources _ -> []

chooseOne :: MonadGame env m => IdentityId -> [Choice] -> m ()
chooseOne ident msgs = push (Ask ident $ ChooseOne msgs)

chooseOrRunOne :: MonadGame env m => IdentityId -> [Choice] -> m ()
chooseOrRunOne ident = \case
  [] -> throwM NoChoices
  [choice] -> pushAll $ choiceMessages ident choice
  choices -> push (Ask ident $ ChooseOne choices)

choosePlayerOrder :: MonadGame env m => IdentityId -> [IdentityId] -> m ()
choosePlayerOrder ident xs =
  push (Ask ident $ ChoosePlayerOrder (Unsorted xs) mempty)

cardLabel :: HasCardCode a => a -> Choice -> Choice
cardLabel a = CardLabel (toCardCode a)
