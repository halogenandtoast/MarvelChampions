{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Ally.Allies.Fw53015 where
import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Hand
import Marvel.Hp
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource qualified as R
import Marvel.Stats
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.FalconWinter.Cards qualified as Cards
import Marvel.Ally.Types
import Marvel.Ally.Runner ()

fw53015 :: AllyCard Fw53015
fw53015 = ally Fw53015 Cards.fw53015 (Thw 1, 1) (Atk 1, 1) (HP 3)

newtype Fw53015 = Fw53015 (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw53015 where
  getAbilities a@(Fw53015 attrs) =
    [ability a 1 HeroAction OwnsThis ExhaustCost (runAbility a 1)]

instance RunMessage Fw53015 where
  runMessage msg a@(Fw53015 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      players <- filterM (\player -> selectAny $ CharacterControlledBy (IdentityWithId player) <> CharacterWithTrait T.Aerial) =<< selectList AnyIdentity
      chooseOne you [targeting player do
        lastingModifiers attrs (IdentityEntity $ IdentityWithId player <> IdentityWithTrait T.Aerial) [AttackModifier 1] False
        lastingModifiers attrs (AllyEntity $ AllyControlledBy (IdentityWithId player) <> AllyWithTrait T.Aerial) [AttackModifier 1] False
        | player <- players]
      pure a
    _ -> Fw53015 <$> runMessage msg attrs
