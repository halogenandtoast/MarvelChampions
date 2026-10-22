module Marvel.Event.Events.CrisisInterdiction (
  crisisInterdiction,
  CrisisInterdiction (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Trait

crisisInterdiction :: EventCard CrisisInterdiction
crisisInterdiction = event CrisisInterdiction Cards.crisisInterdiction

newtype CrisisInterdiction = CrisisInterdiction (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage CrisisInterdiction where
  runMessage msg e@(CrisisInterdiction attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent identityId _ _ -> do
        aerial <- identityMatches (IdentityWithTrait Aerial) identityId
        schemesWithRest <- removeEach <$> selectList ThwartableScheme
        chooseOne identityId
          [ targeting scheme do
              thwartScheme scheme attrs 2
              when (aerial && notNull rest) $
                chooseOne identityId [targeting other $ thwartScheme other attrs 2 | other <- rest]
          | (scheme, rest) <- schemesWithRest
          ]
        pure e
      _ -> CrisisInterdiction <$> runMessage msg attrs
    _ -> CrisisInterdiction <$> runMessage msg attrs
