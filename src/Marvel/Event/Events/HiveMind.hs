module Marvel.Event.Events.HiveMind (
  hiveMind,
  HiveMind (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Card.Def (CardDef (..))
import Marvel.Support.Cards qualified as Supports

hiveMind :: EventCard HiveMind
hiveMind = event HiveMind Cards.hiveMind

newtype HiveMind = HiveMind (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Remove 2 threat from a scheme. Remove 1 additional threat from that scheme for each
-- Army of Ants support you control.
instance RunMessage HiveMind where
  runMessage msg e@(HiveMind attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      ants <- selectListCount $ SupportMatches [SupportNamed (cdName Supports.armyOfAnts), SupportControlledBy (IdentityWithId you)]
      removeThreat you attrs (2 + ants) ThwartableScheme
      pure e
    _ -> HiveMind <$> runMessage msg attrs
