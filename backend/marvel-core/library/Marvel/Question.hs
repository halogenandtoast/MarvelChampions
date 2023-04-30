module Marvel.Question
  ( module Marvel.Question
  , module Marvel.Payment
  ) where

import Marvel.Prelude

import Marvel.Ability hiding (Attack, Thwart)
import Marvel.Card
import Marvel.Damage
import Marvel.Id
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import {-# SOURCE #-} Marvel.Message
import Marvel.Payment
import Marvel.Source
import Marvel.Target
import Marvel.Window (Window(..))

data Question
  = ChooseOne [Choice]
  | ChooseN Natural [Choice]
  | ChooseOneAtATime [Choice]
  | ChoosePlayerOrder (Unsorted IdentityId) (Sorted IdentityId)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Sorted a = Sorted { unSorted :: [a] }
  deriving newtype (Show, Semigroup, Monoid, Eq, ToJSON, FromJSON)

newtype Unsorted a = Unsorted { unUnsorted :: [a] }
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

runAbility :: IsTarget a => a -> Natural -> Choice
runAbility a = RunAbility (toTarget a)

cardLabel :: HasCardCode a => a -> Choice -> Choice
cardLabel a = CardLabel (toCardCode a)
