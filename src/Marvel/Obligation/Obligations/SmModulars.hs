module Marvel.Obligation.Obligations.SmModulars where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Criteria ()
import Marvel.Matchers
import Marvel.Card.Code ()
import Marvel.Entity
import Marvel.GameValue ()
import Marvel.Hp ()
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Stats ()
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types

newtype SmModularsObligation = SmModularsObligation (Attrs Obligation)
  deriving anyclass (IsObligation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smFriendsAndFamily :: ObligationCard SmModularsObligation
smFriendsAndFamily = obligation SmModularsObligation Cards.smFriendsAndFamily

instance HasModifiersFor SmModularsObligation where
  getModifiersFor _ (IdentityRef you) (SmModularsObligation attrs)
    | obligationOwner attrs == Just you = do
        hero <- identityMatches HeroIdentity you
        pure [AdditionalResourceCost (CardWithType EventType) 1 | hero]
  getModifiersFor _ _ _ = pure []

instance HasAbilities SmModularsObligation where
  getAbilities (SmModularsObligation attrs) = discardToDiscardAbility attrs HeroSpecificCard

instance RunMessage SmModularsObligation where
  runMessage msg o@(SmModularsObligation attrs) = runQueueT $ case msg of
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs -> pure o
    RanAbility you (isTarget attrs -> True) 1 _ _ -> o <$ discardToDiscard attrs you HeroSpecificCard
    _ -> SmModularsObligation <$> runMessage msg attrs
