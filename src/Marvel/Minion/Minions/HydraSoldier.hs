module Marvel.Minion.Minions.HydraSoldier (
  hydraSoldier,
  HydraSoldier (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Minion.Cards qualified as Cards
import Marvel.Minion.Runner

hydraSoldier :: MinionCard HydraSoldier
hydraSoldier = minion HydraSoldier Cards.hydraSoldier (Sch 1) (Atk 2) (HP 4)

newtype HydraSoldier = HydraSoldier (Attrs Minion)
  deriving anyclass (IsMinion, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage HydraSoldier where
  runMessage msg (HydraSoldier attrs) = case msg of
    -- When Defeated: deal the engaged player an encounter card
    MinionMessage ident MinionDefeated | ident == minionId attrs -> do
      attrs' <- runMessage msg attrs
      push $ DealEncounterCard (minionEngagedIdentity attrs)
      pure $ HydraSoldier attrs'
    _ -> HydraSoldier <$> runMessage msg attrs
