module Marvel.Ally.Allies.Triage where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Query
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W

triage :: AllyCard Triage
triage = ally Triage Cards.triage (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype Triage = Triage (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Triage where
  getAbilities a = [limitedWindowAbility a 1 W.EntersPlayThis Response
    (OwnsThis <> CharacterExists (CharacterWithTrait Trait.XMen <> CharacterWithAnyDamage)) NoCost (runAbility a 1)]

instance RunMessage Triage where
  runMessage msg a@(Triage attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      characters <- selectList (CharacterWithTrait Trait.XMen <> CharacterWithAnyDamage)
      chooseOne you [targeting (characterRef character) (heal character 2) | character <- characters]
      pure a
    _ -> Triage <$> runMessage msg attrs
