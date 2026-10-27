module Marvel.Support.Supports.Milano (
  milano,
  Milano (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Matchers (IdentityMatcher (FirstPlayer))
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Question
import Marvel.Ref
import Marvel.Resource
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

milano :: SupportCard Milano
milano = support Milano Cards.milano

-- | Permanent. Setup (Ship Command): put into play under the first player's control as
-- setup begins ('PutSetupEncounterCardsIntoPlay'). The first player controls the
-- Milano: as the first player token passes, or the first player is chosen or
-- eliminated, it goes to the new first player's play area.
newtype Milano = Milano (Attrs Support)
  deriving anyclass (IsSupport, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Piloting — Resource: exhaust the Milano → generate a [wild] resource for any player.
instance HasAbilities Milano where
  getAbilities a = [ability a 1 Resource ForAnyPlayer ExhaustCost $ Pay (ResourcePayment Wild)]

instance RunMessage Milano where
  runMessage msg s@(Milano attrs) = runQueueT $ case msg of
    PassFirstPlayer -> follow
    SetPlayerOrder _ -> follow
    RemoveFromPlay (IdentityRef _) -> follow
    CardSpecific (isTarget attrs -> True) "followFirstPlayer" _ -> do
      let controller = supportController attrs
      selectOne FirstPlayer >>= traverse_ \first -> when (first /= controller) do
        identityStep controller (SupportRemoved $ supportId attrs)
        identityStep first (SupportCreated $ supportId attrs)
        supportStep (supportId attrs) (SupportChangedController first)
      pure s
    _ -> Milano <$> runMessage msg attrs
   where
    -- once the game has seen to the player order
    follow = s <$ cardSpecific attrs "followFirstPlayer" ()
