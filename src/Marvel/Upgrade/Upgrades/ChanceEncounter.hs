module Marvel.Upgrade.Upgrades.ChanceEncounter where

import Marvel.Prelude
import Data.Maybe (maybeToList)
import Marvel.Ability
import Marvel.Ability qualified as Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Query
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question
import Marvel.Ref
import Marvel.Trait qualified as Trait
import Marvel.Upgrade.Cards qualified as Cards
import Marvel.Upgrade.Types
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Window qualified as W

chanceEncounter :: UpgradeCard ChanceEncounter
chanceEncounter = upgrade ChanceEncounter Cards.chanceEncounter

newtype ChanceEncounter = ChanceEncounter (Attrs Upgrade)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsUpgrade ChanceEncounter where
  upgradeTargets _ = Just . map toTarget <$> selectList (NotSideScheme $ SideSchemeWithUpgrade $ UpgradeIs Cards.chanceEncounter)

instance HasAbilities ChanceEncounter where
  getAbilities (ChanceEncounter a) =
    [ limitedWindowAbility a 1 (W.SideSchemeDefeated W.When $ SideSchemeWithId sid)
        Interrupt OwnsThis NoCost (runAbility a 1)
    | sid <- maybeToList (upgradeAttachedSideScheme a)
    ]

instance RunMessage ChanceEncounter where
  runMessage msg u@(ChanceEncounter attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      search you (SearchIdentityDeckAndDiscardPile you) (CardWithType AllyType) SearchDrawOne ShuffleBackIn
      pure u
    _ -> ChanceEncounter <$> runMessage msg attrs
