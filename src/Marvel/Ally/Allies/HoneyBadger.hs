module Marvel.Ally.Allies.HoneyBadger where

import Marvel.Prelude
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Matchers
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Window qualified as W
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner

honeyBadger :: AllyCard HoneyBadger
honeyBadger = ally HoneyBadger Cards.honeyBadger (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype HoneyBadger = HoneyBadger (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities HoneyBadger where
  getAbilities a@(HoneyBadger attrs) =
    [ limitedWindowAbility a 1 (W.AllyTookDamageOf (AllyWithId $ allyId attrs) FromAnyDamageSource)
        HeroResponse (OwnsThis <> CharacterExists (IdentityCharacters (IdentityWithTitle "X-23") <> CharacterCanBeReadied)) NoCost (runAbility a 1)
    ]

instance RunMessage HoneyBadger where
  runMessage msg a@(HoneyBadger attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      selectList (IdentityWithTitle "X-23") >>= traverse_ ready
      pure a
    _ -> HoneyBadger <$> runMessage msg attrs
