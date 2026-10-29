module Marvel.Ally.Allies.GamoraNebula (gamoraNebula, GamoraNebula (..)) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Ally.Cards qualified as Cards
import Marvel.Ally.Runner
import Marvel.Cost.Types
import Marvel.Criteria
import Marvel.Hero.Heroes.NebulaHero (resolveTechnique)
import Marvel.Matchers
import Marvel.Query
import Marvel.Trait (Trait (Technique))
import Marvel.Window qualified as W

gamoraNebula :: AllyCard GamoraNebula
gamoraNebula = ally GamoraNebula Cards.gamoraNebula (Thw 2, 1) (Atk 2, 1) (HP 3)

newtype GamoraNebula = GamoraNebula (Attrs Ally)
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities GamoraNebula where
  getAbilities a@(GamoraNebula attrs) =
    [limitedWindowAbility a 1 (W.PlayThis W.After) Response
      (OwnsThis <> UpgradeExists (UpgradeWithTrait Technique <> UpgradeWithResolvableSpecial <> UpgradeControlledBy (IdentityWithId $ allyController attrs)))
      NoCost (runAbility a 1)]

instance RunMessage GamoraNebula where
  runMessage msg a@(GamoraNebula attrs) = runQueueT $ case msg of
    RanAbility ident (isTarget attrs -> True) 1 _ _ -> do
      upgrades <- selectList (UpgradeWithTrait Technique <> UpgradeWithResolvableSpecial <> UpgradeControlledBy (IdentityWithId ident))
      unless (null upgrades) $ chooseOne ident [targeting uid (resolveTechnique ident uid) | uid <- upgrades]
      pure a
    _ -> GamoraNebula <$> runMessage msg attrs
