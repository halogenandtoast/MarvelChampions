module Marvel.SideScheme.SideSchemes.InvoluntaryProcedures where

import Marvel.Prelude
import Marvel.Question
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source ()
import Marvel.Matchers
import Marvel.Query ()
import Marvel.Window qualified as W
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.GameValue

involuntaryProcedures :: SideSchemeCard InvoluntaryProcedures
involuntaryProcedures = sideScheme InvoluntaryProcedures Cards.involuntaryProcedures (Static 6)

newtype InvoluntaryProcedures = InvoluntaryProcedures (Attrs SideScheme)
  deriving anyclass HasModifiersFor
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance IsSideScheme InvoluntaryProcedures where
  sideSchemeAbilities (InvoluntaryProcedures attrs) =
    [limitedWindowAbility attrs 1 (W.IdentityDamaged W.After $ IdentityWithTitle "Deadpool")
      ForcedResponse NoCriteria NoCost (runAbility attrs 1)]

instance RunMessage InvoluntaryProcedures where
  runMessage msg s@(InvoluntaryProcedures attrs) = runQueueT $ case msg of
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      placeThreat (sideSchemeId attrs) 1
      cardSpecific attrs "checkRemoval" ()
      pure s
    CardSpecific (isTarget attrs -> True) "checkRemoval" _ -> do
      when (sideSchemeThreat attrs >= 10) $ removeFromGame attrs
      pure s
    _ -> InvoluntaryProcedures <$> runMessage msg attrs
