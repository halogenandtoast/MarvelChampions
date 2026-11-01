module Marvel.Obligation.Obligations.AgeOfApocalypseCampaign where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Message.Lifted
import Marvel.Ref
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Matchers
import Marvel.Modifier (HasModifiersFor)
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Question (runAbility)

newtype AoaCampaignObligation = AoaCampaignObligation (Attrs Obligation)
  deriving anyclass (IsObligation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

panickedRefugees :: ObligationCard AoaCampaignObligation
panickedRefugees = obligation AoaCampaignObligation Cards.panickedRefugees

instance HasAbilities AoaCampaignObligation where
  getAbilities (AoaCampaignObligation a) = [ability a 1 AlterEgoAction OwnsThis (ExhaustCharacterCost $ IdentityCharacters You) (runAbility a 1)]

instance RunMessage AoaCampaignObligation where
  runMessage msg e@(AoaCampaignObligation a) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId a -> do
      drawCards you 1
      pure $ AoaCampaignObligation a {obligationOwner = Just you}
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId a -> pure e
    RanAbility _ (isTarget a -> True) 1 _ _ -> do
      removeFromGame a
      pure e
    _ -> AoaCampaignObligation <$> runMessage msg a
