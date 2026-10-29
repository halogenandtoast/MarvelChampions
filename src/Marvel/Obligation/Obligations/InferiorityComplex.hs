module Marvel.Obligation.Obligations.InferiorityComplex (inferiorityComplex, InferiorityComplex (..)) where

import Marvel.Prelude
import Marvel.Ability (HasAbilities)
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Modifier (HasModifiersFor)
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Ref
import Marvel.Query
import Marvel.Trait

inferiorityComplex :: ObligationCard InferiorityComplex
inferiorityComplex = obligation InferiorityComplex Cards.inferiorityComplex

newtype InferiorityComplex = InferiorityComplex (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage InferiorityComplex where
  runMessage msg o@(InferiorityComplex attrs) = runQueueT $ case msg of
    ObligationMessage oid (RevealObligation you) | oid == obligationId attrs -> do
      hero <- identityMatches HeroIdentity you
      when hero $ chooseOne you
        [ labeled "$label.doNotFlipToAlterEgo" (pure ())
        , labeled "$label.changeToB" (changeToAlterEgo you)
        ]
      resolveObligation oid you
      pure o
    ObligationMessage oid (ResolveObligation you) | oid == obligationId attrs -> do
      available <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) you
      chooseOrRunOne you $
        [labeled "$label.exhaustAlterEgoRemoveInferiorityComplex" (exhaust you >> removeFromGame attrs) | available]
        <> [labeled "$label.discardTechniquesInferiorityComplex" (cardSpecific attrs "discardTechniques" (you, 2 :: Int, False))]
      pure o
    CardSpecific (isTarget attrs -> True) "discardTechniques" (fromJSON -> Success (you, remaining, discarded)) -> do
      upgrades <- selectList $ upgradeControlledBy you <> UpgradeWithTrait Technique <> UpgradeThatCanBeDiscarded
      if remaining <= (0 :: Int) || null upgrades
        then unless discarded $ gainSurge attrs
        else chooseOne you
          [targeting uid $ discardUpgrade uid >> cardSpecific attrs "discardTechniques" (you, remaining - 1, True) | uid <- upgrades]
      pure o
    _ -> InferiorityComplex <$> runMessage msg attrs
