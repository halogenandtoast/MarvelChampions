module Marvel.Obligation.Obligations.Claustrophobia where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query ()
import Marvel.Question
import Marvel.Ref
import Marvel.Status ()
import Marvel.Window qualified as W ()
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types

claustrophobia :: ObligationCard Claustrophobia
claustrophobia = obligation Claustrophobia Cards.claustrophobia

newtype Claustrophobia = Claustrophobia (Attrs Obligation)
  deriving anyclass (IsObligation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor Claustrophobia where
  getModifiersFor _ (IdentityRef you) (Claustrophobia attrs) =
    pure [CannotChangeToHeroForm | obligationOwner attrs == Just you]
  getModifiersFor _ _ _ = pure []

instance HasAbilities Claustrophobia where
  getAbilities a@(Claustrophobia attrs) =
    [ ability a 1 AlterEgoAction (SelfMatches $ IdentityWithId owner) ExhaustCost (runAbility a 1)
    | Just owner <- [obligationOwner attrs]
    ]

instance RunMessage Claustrophobia where
  runMessage msg o@(Claustrophobia attrs) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId attrs -> do
      changeToAlterEgo you
      pure $ Claustrophobia attrs {obligationOwner = Just you}
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs -> pure o
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> o <$ removeFromGame attrs
    _ -> Claustrophobia <$> runMessage msg attrs
