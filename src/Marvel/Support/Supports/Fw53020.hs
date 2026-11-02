{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-matches #-}
module Marvel.Support.Supports.Fw53020 where
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
import Marvel.Matchers qualified as M
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
import Marvel.Support.Types

fw53020 :: SupportCard Fw53020
fw53020 = support Fw53020 Cards.fw53020

newtype Fw53020 = Fw53020 (Attrs Support)
  deriving anyclass (IsSupport)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities Fw53020 where
  getAbilities a@(Fw53020 attrs) =
    [limitedWindowAbility a 1 (W.PlaysCard W.After (IdentityWithId $ supportController attrs) $ CardWithTrait T.Aerial) Response (OwnsThis <> NotCriteria (AllyExists $ AllyControlledBy (IdentityWithId $ supportController attrs) <> NotAlly (AllyWithTrait T.Aerial))) (ExhaustCost) (runAbility a 1)]

instance HasModifiersFor Fw53020 where
  getModifiersFor _ (IdentityRef you) (Fw53020 attrs) | you == supportController attrs = do
    nonAerial <- selectAny $ AllyControlledBy (IdentityWithId you) <> NotAlly (AllyWithTrait T.Aerial)
    pure [AllyLimitModifier 1 | not nonAerial]
  getModifiersFor _ _ _ = pure []

instance RunMessage Fw53020 where
  runMessage msg a@(Fw53020 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 windows payment -> do
      allies <- selectList $ AllyControlledBy (IdentityWithId you) <> AllyWithTrait T.Aerial <> M.ExhaustedAlly
      chooseOne you [targeting aid $ ready aid | aid <- allies]
      pure a
    _ -> Fw53020 <$> runMessage msg attrs
