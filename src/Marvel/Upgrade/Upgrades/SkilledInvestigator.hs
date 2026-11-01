module Marvel.Upgrade.Upgrades.SkilledInvestigator (
  skilledInvestigator,
  SkilledInvestigator (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Window

skilledInvestigator :: UpgradeCard SkilledInvestigator
skilledInvestigator = upgrade SkilledInvestigator Cards.skilledInvestigator

newtype SkilledInvestigator = SkilledInvestigator (Attrs Upgrade)
  deriving anyclass (IsUpgrade, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Response: After a side scheme is defeated, exhaust Skilled Investigator → draw
-- 1 card.
instance HasAbilities SkilledInvestigator where
  getAbilities a =
    [limitedWindowAbility a 1 (SideSchemeDefeated After AnySideScheme) HeroResponse OwnsThis ExhaustCost $ runAbility a 1]

instance RunMessage SkilledInvestigator where
  runMessage msg u@(SkilledInvestigator attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      drawCards (upgradeController attrs) 1
      pure u
    _ -> SkilledInvestigator <$> runMessage msg attrs
