module Marvel.Treachery.Treacheries.SwarmAttack (
  swarmAttack,
  SwarmAttack (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Trait
import Marvel.Treachery.Cards qualified as Cards
import Marvel.Treachery.Types

swarmAttack :: TreacheryCard SwarmAttack
swarmAttack = treachery SwarmAttack Cards.swarmAttack

newtype SwarmAttack = SwarmAttack (Attrs Treachery)
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

instance RunMessage SwarmAttack where
  runMessage msg t@(SwarmAttack attrs) = runQueueT $ case msg of
    TreacheryMessage ident (RevealTreachery identityId) | ident == treacheryId attrs -> do
      drones <-
        selectList $ MinionWithTrait Drone <> MinionEngagedWith (IdentityWithId identityId)
      -- a stunned drone only loses its stun: it makes no attack
      attacking <-
        selectList $ MinionWithTrait Drone <> MinionEngagedWith (IdentityWithId identityId) <> NotMinion StunnedMinion
      for_ drones (`enemyAttacks` identityId)
      when (null attacking) $ putTopOfDeckIntoPlayAsDrone identityId
      pure t
    _ -> SwarmAttack <$> runMessage msg attrs
