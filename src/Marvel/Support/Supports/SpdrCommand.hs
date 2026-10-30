module Marvel.Support.Supports.SpdrCommand where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Query
import Marvel.Trait
import Marvel.Question (runAbility)

spdrCommand :: SupportCard SpdrCommand
spdrCommand = support SpdrCommand Cards.spdrCommand

newtype SpdrCommand = SpdrCommand (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities SpdrCommand where
  getAbilities a =
    [ label "$label.spdrCommandDraw" $ ability a 1 HeroAction OwnsThis (Costs [ExhaustCost, ExhaustUpgradeCost $ UpgradeWithTrait Interface]) (runAbility a 1)
    , label "$label.spdrCommandReady" $ ability a 2 HeroAction (OwnsThis <> UpgradeExists (UpgradeWithTrait Interface <> UpgradeControlledBy You <> UpgradeCanBeReadied)) (Costs [ExhaustCost, DiscardHandCardsCost 1 (Just 1)]) (runAbility a 2)
    ]

instance RunMessage SpdrCommand where
  runMessage msg e@(SpdrCommand attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> e <$ drawCards you 1
    RanAbility you (isTarget attrs -> True) 2 _ _ -> do
      interfaces <- selectList (UpgradeWithTrait Interface <> upgradeControlledBy you <> UpgradeCanBeReadied)
      chooseOne you [targeting u (ready u) | u <- interfaces]
      pure e
    _ -> SpdrCommand <$> runMessage msg attrs
