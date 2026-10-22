module Marvel.Question (
  module Marvel.Question,
  module Marvel.Payment,
) where

import Marvel.Prelude

import Marvel.Ability hiding (Attack, Thwart)
import Marvel.Card
import Marvel.Damage
import Marvel.Id
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import {-# SOURCE #-} Marvel.Message
import Marvel.Payment
import Marvel.Ref
import Marvel.Window (Window (..))

data Question
  = ChooseOne [Choice]
  | ChooseN Natural [Choice]
  | ChooseOneAtATime [Choice]
  | ChoosePlayerOrder (Unsorted IdentityId) (Sorted IdentityId)
  | -- | a question with a prompt (an i18n key under @$label.@, or text) and the card
    -- it is about, for the client to say what is being asked
    QuestionLabel Text (Maybe Target) Question
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

hasChoices :: Question -> Bool
hasChoices = \case
  ChooseOne cs -> notNull cs
  ChooseN _ cs -> notNull cs
  ChooseOneAtATime cs -> notNull cs
  ChoosePlayerOrder (Unsorted xs) _ -> notNull xs
  QuestionLabel _ _ q -> hasChoices q

-- | A question with a prompt.
prompted :: Text -> Question -> Question
prompted l = QuestionLabel l Nothing

-- | A question with a prompt, about a card.
promptedAbout :: (IsRef a) => Text -> a -> Question -> Question
promptedAbout l a = QuestionLabel l (Just $ toTarget a)

newtype Sorted a = Sorted {unSorted :: [a]}
  deriving newtype (Show, Semigroup, Monoid, Eq, ToJSON, FromJSON)

newtype Unsorted a = Unsorted {unUnsorted :: [a]}
  deriving newtype (Show, Semigroup, Monoid, Eq, ToJSON, FromJSON)

data ChooseATarget = ChooseAPlayer | TargetMatches EntityMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Choice
  = CardLabel CardCode Choice
  | Label Text [Choice]
  | TargetLabel Target [Choice]
  | EndTurn
  | UseAbility Ability
  | RunAbility Target Natural
  | ChangeForm
  | ChangeToForm Side
  | PlayCard PlayerCard (Maybe Window)
  | PayWithCard PlayerCard
  | FinishPayment
  | ReadyIdentity
  | Pay Payment
  | Run [Message]
  | DamageEnemy Target Source Damage
  | DamageAllEnemies EnemyMatcher Source Damage
  | DamageAllCharacters CharacterMatcher Source Damage
  | ThwartScheme Target Source Natural
  | -- | a hero's or ally's basic thwart: a thwart, whatever its source
    BasicThwartScheme Target Source Natural
  | Stun Target Source
  | Confuse Target Source
  | Recover
  | Heal CharacterId Natural
  | DamageCharacter CharacterId Source Damage
  | Attack
  | Thwart
  | Defend EnemyId
  | AllyAttack AllyId
  | AllyThwart AllyId
  | AllyDefend AllyId EnemyId
  | CreateEffect CardDef Source ChooseATarget
  | RemoveThreat Source Natural SchemeMatcher
  | PlaceThreat Source Natural SchemeMatcher
  | ChooseDamage Source Damage EnemyMatcher
  | ChooseHeal Natural CharacterMatcher
  | DiscardTarget Target
  | DiscardCard Card
  | ChooseDrawCards Natural IdentityMatcher
  | ChooseEnemy EnemyMatcher Target
  | ChoosePlayer IdentityMatcher Target
  | ChooseUpgrade UpgradeMatcher Target
  | ReturnTargetToHand Target
  | ChooseOneLabelChoice [(Text, Choice)]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

runAbility :: (IsRef a) => a -> Natural -> Choice
runAbility a = RunAbility (toTarget a)

cardLabel :: (HasCardCode a) => a -> Choice -> Choice
cardLabel a = CardLabel (toCardCode a)
