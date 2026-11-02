{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.Obligation.Obligations.AosP50029 where

import Marvel.Prelude
import Marvel.Question (runAbility)
import Marvel.Window qualified as W
import Marvel.Phase
import Marvel.Query
import Marvel.Matchers
import Marvel.Game.Source
import Marvel.Criteria
import Marvel.Cost
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types

aosP50029 :: ObligationCard AosP50029
aosP50029 = obligation AosP50029 Cards.aosP50029

newtype AosP50029 = AosP50029 (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasAbilities AosP50029 where
  getAbilities (AosP50029 a) = case obligationOwner a of
    Nothing -> []
    Just _ -> [windowAbility a 1 (W.PhaseEnds W.After PlayerPhase) ForcedResponse NoCost (runAbility a 1), ability a 2 AlterEgoAction OwnsThis ExhaustAnyIdentityCost (runAbility a 2)]

instance RunMessage AosP50029 where
  runMessage msg e@(AosP50029 a) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId a -> pure $ AosP50029 a {obligationOwner=Just you}
    ObligationMessage oid ResolvedObligation{} | oid == obligationId a -> pure e
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      for_ (obligationOwner a) $ \you -> selectList (SupportControlledBy $ IdentityWithId you) >>= traverse_ (`removeCounters` 1)
      pure e
    RanAbility _ (isTarget a -> True) 2 _ _ -> e <$ removeFromPlay a
    _ -> AosP50029 <$> runMessage msg a
