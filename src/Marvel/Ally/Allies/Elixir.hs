module Marvel.Ally.Allies.Elixir where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Damage
import Marvel.Trait qualified as Trait
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

elixir :: AllyCard Elixir
elixir = ally Elixir Cards.elixir (Thw 1, 1) (Atk 1, 1) (HP 3)

newtype Elixir = Elixir (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Elixir where
  getAbilities a@(Elixir attrs) = [limitedWindowAbility a 1
    (W.WindowOneOf [W.AllyAttacked W.After self AnyEnemy, W.AllyThwarted W.After self AnyScheme])
    Response (OwnsThis <> CharacterExists others) NoCost (ChooseHeal 1 others)]
    where
      self = AllyWithId $ allyId attrs
      others = CharacterControlledBy AnyIdentity <> CharacterOtherThan (AllyCharacter $ allyId attrs) <> CharacterWithAnyDamage <> CharacterCanBeHealed

instance RunMessage Elixir where
  runMessage msg (Elixir attrs) = runQueueT $ Elixir <$> runMessage msg attrs
