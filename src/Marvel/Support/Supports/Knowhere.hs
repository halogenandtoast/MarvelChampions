module Marvel.Support.Supports.Knowhere (
  knowhere,
  Knowhere (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Trait (Trait (Guardian))
import Marvel.Window qualified as W

knowhere :: SupportCard Knowhere
knowhere = support Knowhere Cards.knowhere

newtype Knowhere = Knowhere (Attrs Support)
  deriving anyclass (IsSupport)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Increase your ally limit by 1.
instance HasModifiersFor Knowhere where
  getModifiersFor _ (IdentityRef iid) (Knowhere a)
    | iid == supportController a = pure [AllyLimitModifier 1]
  getModifiersFor _ _ _ = pure []

-- | Response: After a player plays a Guardian ally, exhaust Knowhere → that player draws
-- 1 card. An ally Star-Lord makes a Guardian counts (FAQ): its traits as in play.
instance HasAbilities Knowhere where
  getAbilities a =
    [ limitedWindowAbility
        a
        1
        (W.PlaysAlly (AllyWithTrait Guardian))
        Response
        OwnsThis
        ExhaustCost
        (runAbility a 1)
    ]

instance RunMessage Knowhere where
  runMessage msg s@(Knowhere attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 windows _ -> do
      for_ [aid | W.PlayedAlly aid <- windows] \aid -> do
        -- that player: the ally's controller
        players <- selectList AnyIdentity
        controllers <- filterM (\p -> selectAny (AllyWithId aid <> AllyControlledBy (IdentityWithId p))) players
        for_ (take 1 controllers) (`drawCards` 1)
      pure s
    _ -> Knowhere <$> runMessage msg attrs
