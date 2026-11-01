module Marvel.Obligation.Obligations.TheMercWithTheMouth where

import Marvel.Prelude
import Marvel.Question
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Game.Source
import Marvel.Identity.Types (Field (PlayerIdentityPassed))
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Phase
import Marvel.Projection
import Marvel.Query
import Marvel.Ref
import Marvel.Window qualified as W
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types

theMercWithTheMouth :: ObligationCard TheMercWithTheMouth
theMercWithTheMouth = obligation TheMercWithTheMouth Cards.theMercWithTheMouth

newtype TheMercWithTheMouth = TheMercWithTheMouth (Attrs Obligation)
  deriving anyclass IsObligation
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor TheMercWithTheMouth where
  getModifiersFor _ target (TheMercWithTheMouth attrs) = case obligationOwner attrs of
    Nothing -> pure []
    Just owner -> do
      present <- selectAny (IdentityWithId owner)
      if not present then pure [] else case target of
        AllyRef aid -> do
          yours <- allyMatches (AllyControlledBy $ IdentityWithId owner) aid
          pure [CannotReady | yours]
        IdentityRef who | who /= owner -> do
          active <- getActivePlayerId
          phase <- getPhase
          passed <- project PlayerIdentityPassed owner
          windows <- getCurrentWindows
          let ending = W.Window W.When (W.TurnEnded owner) `elem` windows
          pure [CannotResolvePlayerCardAbilities | active == owner, phase == PlayerPhase, not passed || ending]
        _ -> pure []

instance HasAbilities TheMercWithTheMouth where
  getAbilities (TheMercWithTheMouth attrs) =
    [limitedWindowAbility attrs 1 (W.PhaseEnds W.After PlayerPhase) ForcedResponse
      (SelfMatches $ IdentityWithId owner) NoCost (runAbility attrs 1)
    | Just owner <- [obligationOwner attrs]]

instance RunMessage TheMercWithTheMouth where
  runMessage msg o@(TheMercWithTheMouth attrs) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId attrs -> do
      allies <- selectList (AllyControlledBy $ IdentityWithId you)
      for_ allies exhaust
      pure $ TheMercWithTheMouth attrs {obligationOwner = Just you}
    ObligationMessage oid (ResolvedObligation _) | oid == obligationId attrs -> pure o
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      chooseOnePrompted you "$label.didYouTalkThisPhase"
        [ labeled "$label.talkedThisPhase" (pure ())
        , labeled "$label.didNotTalkThisPhase" (removeFromPlay attrs)
        ]
      pure o
    _ -> TheMercWithTheMouth <$> runMessage msg attrs
