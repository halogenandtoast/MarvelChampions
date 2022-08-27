module Marvel.Event.Events.Emergency
  ( emergency
  , Emergency(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Message
import Marvel.Modifier
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Window

emergency :: EventCard Emergency
emergency = event Emergency Cards.emergency

newtype Emergency = Emergency (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsSource, IsTarget)

instance RunMessage Emergency where
  runMessage msg e@(Emergency attrs) = case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent _ _ (Just (ThreatPlaced _ schemeId n)) -> do
        let
          newMsg = case schemeId of
            SchemeMainSchemeId sid -> MainSchemeMessage
              sid
              (MainSchemePlaceThreat (subtractNatural 1 n))
            SchemeSideSchemeId sid -> SideSchemeMessage
              sid
              (SideSchemePlaceThreat (subtractNatural 1 n))
        replaceMatchingMessage (const [newMsg]) $ \case
          (MainSchemeMessage mid (MainSchemePlaceThreat _)) ->
            schemeId == SchemeMainSchemeId mid
          (SideSchemeMessage mid (SideSchemePlaceThreat _)) ->
            schemeId == SchemeSideSchemeId mid
          _ -> False
        pure e
      _ -> Emergency <$> runMessage msg attrs
    _ -> Emergency <$> runMessage msg attrs
