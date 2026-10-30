module Marvel.Obligation.Obligations.UncertainLoyalties (
  uncertainLoyalties,
  UncertainLoyalties (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers hiding (ExhaustedIdentity)
import Marvel.Message.Lifted
import Marvel.Obligation.Cards qualified as Cards
import Marvel.Obligation.Types
import Marvel.Ability.Types (HasAbilities)
import Marvel.Modifier (HasModifiersFor)
import Marvel.Query

uncertainLoyalties :: ObligationCard UncertainLoyalties
uncertainLoyalties = obligation UncertainLoyalties Cards.uncertainLoyalties

newtype UncertainLoyalties = UncertainLoyalties (Attrs Obligation)
  deriving anyclass (IsObligation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Choose: exhaust Jessica Drew → remove Uncertain Loyalties from the game; or place 3
-- threat on the main scheme and discard this obligation.
instance RunMessage UncertainLoyalties where
  runMessage msg o@(UncertainLoyalties attrs) = runQueueT $ case msg of
    ObligationMessage ident msg' | obligationId attrs == ident -> case msg' of
      RevealObligation identityId -> do
        resolveObligation (obligationId attrs) identityId
        pure o
      ResolveObligation identityId -> do
        canExhaust <- identityMatches (AlterEgoIdentity <> UnexhaustedIdentity) identityId
        mainSchemes <- selectList EncounterMainScheme
        chooseOrRunOne identityId $
          [ labeled "$label.exhaustJessicaDrewRemoveUncertainLoyalties" (exhaust identityId >> removeFromGame attrs)
          | canExhaust
          ]
            <> [labeled (countLabel "placeThreatDiscardObligation" 3) (for_ mainSchemes (`placeThreat` 3))]
        pure o
      _ -> UncertainLoyalties <$> runMessage msg attrs
    _ -> UncertainLoyalties <$> runMessage msg attrs
