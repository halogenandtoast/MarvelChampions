module Marvel.SideScheme.SideSchemes.Cannonade (
  cannonade,
  Cannonade (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Question (runAbility)
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

-- | Base threat 1; Hinder 3 per player and its amplify icon are its card's (Ship Command).
cannonade :: SideSchemeCard Cannonade
cannonade = sideScheme Cannonade Cards.cannonade (Static 1)

newtype Cannonade = Cannonade (Attrs SideScheme)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | First Player Action: exhaust the Milano → remove 3 threat from this scheme.
instance IsSideScheme Cannonade where
  sideSchemeAbilities (Cannonade a) =
    [ability a 1 Action (SelfMatches FirstPlayer) ExhaustMilanoCost $ runAbility a 1]

instance RunMessage Cannonade where
  runMessage msg s@(Cannonade attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> s <$ removeSideSchemeThreat (sideSchemeId attrs) 3
    _ -> Cannonade <$> runMessage msg attrs
