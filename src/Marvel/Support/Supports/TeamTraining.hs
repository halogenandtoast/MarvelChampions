module Marvel.Support.Supports.TeamTraining (
  teamTraining,
  TeamTraining (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

teamTraining :: SupportCard TeamTraining
teamTraining = support TeamTraining Cards.teamTraining

newtype TeamTraining = TeamTraining (Attrs Support)
  deriving anyclass (IsSupport, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Play under any player's control. Max 1 per player. Each ally you control gets +1
-- hit point.
instance HasModifiersFor TeamTraining where
  getModifiersFor _ (AllyRef aid) (TeamTraining a) = do
    yours <- selectAny $ AllyWithId aid <> AllyControlledBy (IdentityWithId $ supportController a)
    pure [HitPointModifier 1 | yours]
  getModifiersFor _ _ _ = pure []

instance RunMessage TeamTraining where
  runMessage msg (TeamTraining attrs) = TeamTraining <$> runMessage msg attrs
