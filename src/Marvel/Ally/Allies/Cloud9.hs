module Marvel.Ally.Allies.Cloud9 where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Query
import Marvel.Trait

cloud9 :: AllyCard Cloud9
cloud9 = ally Cloud9 Cards.cloud9 (Thw 1, 1) (Atk 1, 1) (HP 3)

newtype Cloud9 = Cloud9 (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Cloud9 where
  getAbilities a = [ability a 1 HeroAction OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage Cloud9 where
  runMessage msg a@(Cloud9 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      players <- selectList AnyIdentity
      chooseOne you
        [ targeting player do
            createEffect Cards.cloud9 attrs (IdentityEntity $ IdentityWithId player <> IdentityWithTrait Aerial)
            createEffect Cards.cloud9 attrs (AllyEntity $ AllyControlledBy (IdentityWithId player) <> AllyWithTrait Aerial)
        | player <- players
        ]
      pure a
    _ -> Cloud9 <$> runMessage msg attrs

newtype Cloud9Effect = Cloud9Effect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

cloud9Effect :: CardEffect Cloud9Effect
cloud9Effect = effectWith Cloud9Effect Cards.cloud9 $
  (modifiersL .~ [ThwartModifier 1]) . (endsL ?~ DisableAtEndOfPhase)

instance RunMessage Cloud9Effect where
  runMessage msg (Cloud9Effect attrs) = Cloud9Effect <$> runMessage msg attrs
