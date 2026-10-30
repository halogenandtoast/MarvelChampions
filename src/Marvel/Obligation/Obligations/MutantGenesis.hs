module Marvel.Obligation.Obligations.MutantGenesis where

import Marvel.Id
import Marvel.Prelude
import Marvel.Entity
import Marvel.Card
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Obligation.Types
import Marvel.Modifier
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.Query
import Marvel.Matchers
import Marvel.Question
import Marvel.SideScheme.Cards qualified as Sides
import Marvel.Window qualified as W
import Marvel.Obligation.Cards qualified as Cards

newtype MutantGenesisObligation = MutantGenesisObligation (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mgWarnTheOthers :: ObligationCard MutantGenesisObligation
mgWarnTheOthers = obligation MutantGenesisObligation Cards.mgWarnTheOthers

instance HasAbilities MutantGenesisObligation where
  getAbilities (MutantGenesisObligation a) =
    [ability a 1 AlterEgoAction NoCriteria (ExhaustCharacterCost $ IdentityCharacters You) (DiscardTarget $ toRef a)]
    <> [windowAbility a 2 (W.TurnEnds W.After $ IdentityWithId you) ForcedResponse NoCost (runAbility a 2) | Just you <- [obligationOwner a]]

instance RunMessage MutantGenesisObligation where
  runMessage msg e@(MutantGenesisObligation attrs) = runQueueT $ case msg of
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs -> pure e
    RanAbility _ (isTarget attrs -> True) 2 _ _ -> do
      zero <- selectList (SideSchemeIs Sides.mgOperationZeroTolerance)
      for_ zero (\z -> cardSpecific GameRef "mgTuckObligation" (obligationId attrs, z))
      pure e
    _ -> MutantGenesisObligation <$> runMessage msg attrs
