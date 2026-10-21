module Marvel.Treachery.Treacheries.SwarmAttack (
  swarmAttack,
  SwarmAttack (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Query
import Marvel.Queue
import Marvel.Trait
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

swarmAttack :: TreacheryCard SwarmAttack
swarmAttack = treachery SwarmAttack Cards.swarmAttack

newtype SwarmAttack = SwarmAttack (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage SwarmAttack where
  runMessage msg t@(SwarmAttack attrs) = case msg of
    TreacheryMessage ident (RevealTreachery identityId) | ident == treacheryId attrs -> do
      drones <-
        selectList $ MinionWithTrait Drone <> MinionEngagedWith (IdentityWithId identityId)
      -- ponytail: a stunned drone counts as attacking, though it only loses its stun
      pushAll $
        if null drones
          then [PutTopOfDeckIntoPlayAsDrone identityId]
          else map (`MinionMessage` MinionAttacks identityId) drones
      pure t
    _ -> SwarmAttack <$> runMessage msg attrs
