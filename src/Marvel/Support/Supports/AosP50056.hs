{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Support.Supports.AosP50056 where

import Marvel.Prelude
import Marvel.Question
import Marvel.Id
import Marvel.Projection
import Marvel.Card
import Marvel.Id
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Payment
import Marvel.Query
import Marvel.Resource.Types qualified as R
import Marvel.Trait qualified as T
import Marvel.Window qualified as W
import Marvel.AgentsOfShield.PlayerHelpers

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

aosP50056 :: SupportCard AosP50056
aosP50056 = support AosP50056 Cards.aosP50056

newtype AosP50056 = AosP50056 (Attrs Support)
  deriving anyclass (IsSupport)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50056 where
  getAbilities (AosP50056 attrs) =
    [ability attrs 1 AlterEgoAction (OwnsThis) (ExhaustCost) (runAbility attrs 1)]

instance HasModifiersFor AosP50056 where
  getModifiersFor _ target (AosP50056 attrs) | target == CardIdRef (CardId $ unSupportId $ supportId attrs) = do
    shield <- identityMatches (IdentityWithTrait T.Shield) (supportController attrs)
    pure [ResourceCostReduction 2 | shield]
  getModifiersFor _ _ _ = pure []

instance RunMessage AosP50056 where
  runMessage msg card@(AosP50056 attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      search you (SearchIdentityDeck you AllOfDeck) (CardWithTrait T.Tech) SearchDrawOne ShuffleBackIn
      pure card
    _ -> AosP50056 <$> runMessage msg attrs
