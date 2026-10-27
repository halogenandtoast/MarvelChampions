-- | The Brotherhood of Badoon's side schemes (The Galaxy's Most Wanted, scenario 1), each
-- with the First Player Action: exhaust the Milano → remove 3 threat from this scheme.
module Marvel.SideScheme.SideSchemes.BrotherhoodOfBadoon (
  blockade,
  Blockade (..),
  bombardment,
  Bombardment (..),
  oppressiveArmada,
  OppressiveArmada (..),
  spatialPositioning,
  SpatialPositioning (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Environment.Environments.BadoonShip (chargeUp)
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

milanoAction :: Attrs SideScheme -> Ability
milanoAction a = ability a 1 Action (SelfMatches FirstPlayer) ExhaustMilanoCost $ runAbility a 1

-- | The First Player Action resolved: threat removed, not thwarted.
milanoRemovesThreat :: (ReverseQueue m) => Message -> Attrs SideScheme -> m (Attrs SideScheme)
milanoRemovesThreat msg attrs = case msg of
  RanAbility _ (isTarget attrs -> True) 1 _ _ -> attrs <$ removeSideSchemeThreat (sideSchemeId attrs) 3
  _ -> runMessage msg attrs

-- | Base threat 2; Hinder 2 per player is its keyword. A crisis icon.
blockade :: SideSchemeCard Blockade
blockade = sideSchemeWith Blockade Cards.blockade (Static 2) (crisisL .~ True)

newtype Blockade = Blockade (Attrs SideScheme)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsSideScheme Blockade where
  sideSchemeAbilities (Blockade a) = [milanoAction a]

instance RunMessage Blockade where
  runMessage msg (Blockade attrs) = runQueueT $ Blockade <$> milanoRemovesThreat msg attrs

-- | Base threat 3 per player.
bombardment :: SideSchemeCard Bombardment
bombardment = sideScheme Bombardment Cards.bombardment (PerPlayer 3)

newtype Bombardment = Bombardment (Attrs SideScheme)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsSideScheme Bombardment where
  sideSchemeAbilities (Bombardment a) = [milanoAction a]

-- | Forced Response: after resolving step one of the villain phase, Charge Up.
instance RunMessage Bombardment where
  runMessage msg (Bombardment attrs) = runQueueT $ case msg of
    AccelerateMainScheme -> do
      attrs' <- runMessage msg attrs
      forcedResponse (sideSchemeId attrs) chargeUp
      pure $ Bombardment attrs'
    _ -> Bombardment <$> milanoRemovesThreat msg attrs

-- | Base threat 1; Hinder 3 per player and a hazard icon are its card's.
oppressiveArmada :: SideSchemeCard OppressiveArmada
oppressiveArmada = sideScheme OppressiveArmada Cards.oppressiveArmada (Static 1)

newtype OppressiveArmada = OppressiveArmada (Attrs SideScheme)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsSideScheme OppressiveArmada where
  sideSchemeAbilities (OppressiveArmada a) = [milanoAction a]

instance RunMessage OppressiveArmada where
  runMessage msg (OppressiveArmada attrs) = runQueueT $ OppressiveArmada <$> milanoRemovesThreat msg attrs

-- | Base threat 3 per player; an amplify icon, its card's.
spatialPositioning :: SideSchemeCard SpatialPositioning
spatialPositioning = sideScheme SpatialPositioning Cards.spatialPositioning (PerPlayer 3)

newtype SpatialPositioning = SpatialPositioning (Attrs SideScheme)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance IsSideScheme SpatialPositioning where
  sideSchemeAbilities (SpatialPositioning a) = [milanoAction a]

instance RunMessage SpatialPositioning where
  runMessage msg (SpatialPositioning attrs) = runQueueT $ SpatialPositioning <$> milanoRemovesThreat msg attrs
