module Marvel.Event.Events.CrisisAverted (
  crisisAverted,
  CrisisAverted (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Payment
import Marvel.Query
import Marvel.Ref
import Marvel.Resource

crisisAverted :: EventCard CrisisAverted
crisisAverted = event CrisisAverted Cards.crisisAverted

newtype CrisisAverted = CrisisAverted (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action (thwart): remove 6 threat from the main scheme. Paid for using a
-- [mental] resource (a wild declared as one counts), the thwart ignores crisis icons;
-- otherwise a crisis icon in play keeps it from removing any.
instance RunMessage CrisisAverted where
  runMessage msg e@(CrisisAverted attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you payment _) | ident == eventId attrs -> do
      mental <- paidWith Mental payment
      mains <- filter isMain <$> selectList (if mental then ThwartableSchemeIgnoringCrisis else ThwartableScheme)
      unless (null mains) $ chooseOrRunOne you [targeting (schemeRef scheme) (thwartScheme scheme attrs 6) | scheme <- mains]
      pure e
    _ -> CrisisAverted <$> runMessage msg attrs
   where
    isMain = \case
      SchemeMainSchemeId _ -> True
      _ -> False
