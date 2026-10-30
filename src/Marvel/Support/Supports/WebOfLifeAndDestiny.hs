module Marvel.Support.Supports.WebOfLifeAndDestiny where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Trait (Trait (WebWarrior))
import Marvel.Window qualified as W

webOfLifeAndDestiny :: SupportCard WebOfLifeAndDestiny
webOfLifeAndDestiny = support WebOfLifeAndDestiny Cards.webOfLifeAndDestiny

newtype WebOfLifeAndDestiny = WebOfLifeAndDestiny (Attrs Support)
  deriving anyclass (IsSupport)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor WebOfLifeAndDestiny where
  getModifiersFor _ (CardIdRef cid) (WebOfLifeAndDestiny attrs)
    | cid == CardId (unSupportId $ supportId attrs) = do
        warrior <- selectAny (IdentityWithId (supportController attrs) <> IdentityWithTrait WebWarrior)
        pure [IgnoresResourceCost | warrior]
  getModifiersFor _ _ _ = pure []

instance HasAbilities WebOfLifeAndDestiny where
  getAbilities a =
    [ limitedWindowAbility a 1 (W.AllyLeavesPlay W.After AnyIdentity $ CardWithTrait WebWarrior)
        Response OwnsThis NoCost (runAbility a 1)
    ]

instance RunMessage WebOfLifeAndDestiny where
  runMessage msg s@(WebOfLifeAndDestiny attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      players <- selectList AnyIdentity
      unless (null players) $ chooseOne you [targeting p (drawCards p 1) | p <- players]
      pure s
    _ -> WebOfLifeAndDestiny <$> runMessage msg attrs
