module Marvel.Obligation.Obligations.AgeOfApocalypseDystopianNightmare where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Ability
import Marvel.Matchers ()
import Marvel.Modifier (HasModifiersFor)
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types

newtype AoaDystopianNightmareObligation = AoaDystopianNightmareObligation (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

hunted :: ObligationCard AoaDystopianNightmareObligation
hunted = obligation AoaDystopianNightmareObligation Cards.hunted

instance HasAbilities AoaDystopianNightmareObligation where
  getAbilities (AoaDystopianNightmareObligation a) = discardToDiscardAbility a AnyCard

instance RunMessage AoaDystopianNightmareObligation where
  runMessage msg e@(AoaDystopianNightmareObligation a) = runQueueT $ case msg of
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId a -> pure e
    RanAbility you (isTarget a -> True) 1 _ _ -> e <$ discardToDiscard a you AnyCard
    _ -> AoaDystopianNightmareObligation <$> runMessage msg a
