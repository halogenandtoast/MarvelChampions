module Marvel.Upgrade.Upgrades.StickToItiveness where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Resource.Types (Resource (Physical))

stickToItiveness :: UpgradeCard StickToItiveness
stickToItiveness = upgrade StickToItiveness Cards.stickToItiveness

newtype StickToItiveness = StickToItiveness (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities StickToItiveness where
  getAbilities a =
    [ ability a 1 HeroAction (OwnsThis <> SelfMatches IdentityCanReady)
        (ResourceCost (Just Physical) <> ExhaustCost) (runAbility a 1)
    ]

instance RunMessage StickToItiveness where
  runMessage msg u@(StickToItiveness attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> u <$ ready you
    _ -> StickToItiveness <$> runMessage msg attrs
