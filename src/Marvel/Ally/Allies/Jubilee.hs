module Marvel.Ally.Allies.Jubilee where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W

jubilee :: AllyCard Jubilee
jubilee = ally Jubilee Cards.jubilee (Thw 1, 1) (Atk 1, 1) (HP 2)

newtype Jubilee = Jubilee (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Jubilee where
  getAbilities a = [limitedWindowAbility a 1 W.EntersPlayThis Response (OwnsThis <> EnemyExists AnyEnemy) NoCost (runAbility a 1)]

instance RunMessage Jubilee where
  runMessage msg a@(Jubilee attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      enemies <- selectList AnyEnemy
      chooseOne you [targeting (enemyRef enemy) do
        createEffect Cards.jubilee attrs (IdentityEntity $ HeroIdentity <> IdentityWithTitle "Wolverine")
        createEffect Cards.jubilee attrs (AllyEntity $ AllyWithTitle "Jubilee")
        createEffect Cards.jubilee attrs (AllyEntity $ AllyWithTitle "Wolverine")
        cardSpecific attrs "jubileeTarget" enemy
        | enemy <- enemies]
      pure a
    _ -> Jubilee <$> runMessage msg attrs

newtype JubileeEffect = JubileeEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

jubileeEffect :: CardEffect JubileeEffect
jubileeEffect = effectWith JubileeEffect Cards.jubilee (endsL ?~ DisableAtEndOfPhase)

instance RunMessage JubileeEffect where
  runMessage msg e@(JubileeEffect attrs) = runQueueT $ case msg of
    CardSpecific target "jubileeTarget" (fromJSON -> Success enemy) | target == refocus (effectSource attrs), null (effectModifiers attrs) ->
      pure $ JubileeEffect attrs {effectModifiers = [AttackModifierAgainst enemy 2]}
    _ -> JubileeEffect <$> runMessage msg attrs
