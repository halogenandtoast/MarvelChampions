module Marvel.Support.Supports.Xmansion where

import Marvel.Prelude
import Marvel.Ability
    ( AbilityType(AlterEgoAction), HasAbilities(..), ability )
import Data.List (nub)
import Marvel.Card.Side ()
import Marvel.Payment ()
import Marvel.Game.Source ()
import Marvel.Hand ()
import Marvel.Stats ()
import Marvel.Status ()
import Marvel.Id ()

import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W ()
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

xmansion :: SupportCard Xmansion
xmansion = support Xmansion Cards.xmansion

newtype Xmansion = Xmansion (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Xmansion where
  getAbilities (Xmansion a) =
    [ability a 1 AlterEgoAction (ForAnyPlayer <> AnyCriteria [OwnsThis, SelfMatches (IdentityWithTrait Trait.Mutant)] <> AnyCriteria [CharacterExists (CharacterWithAnyDamage <> CharacterWithTrait Trait.XMen), CharacterExists (CharacterWithAnyDamage <> CharacterWithTrait Trait.Mutant)]) ExhaustCost (runAbility a 1)]

instance RunMessage Xmansion where
  runMessage msg e@(Xmansion attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      mutants <- selectList (CharacterWithAnyDamage <> CharacterWithTrait Trait.Mutant)
      xmen <- selectList (CharacterWithAnyDamage <> CharacterWithTrait Trait.XMen)
      let characters = nub (mutants <> xmen)
      chooseOne you [targeting (characterRef character) (heal character 1) | character <- characters]
      pure e
    _ -> Xmansion <$> runMessage msg attrs
