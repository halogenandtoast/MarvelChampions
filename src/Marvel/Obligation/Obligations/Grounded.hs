module Marvel.Obligation.Obligations.Grounded where
import Marvel.Prelude
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Game.Source
import Marvel.Phase
import Marvel.Entity
import Marvel.Jubilee.Cards qualified as Cards
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Obligation.Types
import Marvel.Question
import Marvel.Ref
import Marvel.Window qualified as W

grounded :: ObligationCard Grounded
grounded = obligation Grounded Cards.jubilee47023
newtype Grounded = Grounded (Attrs Obligation)
  deriving anyclass IsObligation
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)
instance HasModifiersFor Grounded where
  getModifiersFor _ (IdentityRef you) (Grounded attrs) | obligationOwner attrs == Just you = do
    phase <- getPhase
    active <- getActivePlayerId
    pure [AdditionalChangeToHeroFormCost $ SameResourcesCost 2 | phase == PlayerPhase, active == you]
  getModifiersFor _ _ _ = pure []
instance HasAbilities Grounded where
  getAbilities a@(Grounded attrs) = case obligationOwner attrs of
    Just you -> [limitedWindowAbility a 1 (W.PlaysEvent W.After (IdentityWithId you) (CardMatchesAny $ map CardIs [Cards.jubilee47006, Cards.jubilee47007a, Cards.jubilee47007b, Cards.jubilee47007c, Cards.jubilee47008a, Cards.jubilee47008b, Cards.jubilee47008c, Cards.jubilee47009])) Response OwnsThis NoCost (runAbility a 1)]
    Nothing -> []
instance RunMessage Grounded where
  runMessage msg a@(Grounded attrs) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId attrs -> do
      changeToAlterEgo you
      pure $ Grounded attrs {obligationOwner = Just you}
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> a <$ removeFromGame attrs
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs -> pure a
    _ -> Grounded <$> runMessage msg attrs
