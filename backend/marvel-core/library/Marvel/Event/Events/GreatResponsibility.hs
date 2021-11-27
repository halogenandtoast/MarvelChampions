module Marvel.Event.Events.GreatResponsibility
  ( greatResponsibility
  , GreatResponsibility(..)
  ) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Attrs
import Marvel.Event.Cards qualified as Cards
import Marvel.Id
import Marvel.Message
import Marvel.Queue
import Marvel.Source
import Marvel.Target
import Marvel.Window

greatResponsibility :: EventCard GreatResponsibility
greatResponsibility = event GreatResponsibility Cards.greatResponsibility

newtype GreatResponsibility = GreatResponsibility EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, Entity, IsSource, IsTarget)

instance RunMessage GreatResponsibility where
  runMessage msg e@(GreatResponsibility attrs) = case msg of
    EventMessage eid msg' | eid == toId e -> case msg' of
      PlayedEvent identityId _ (Just (ThreatPlaced schemeId n)) -> do
        replaceMatchingMessage
            [IdentityMessage identityId $ IdentityDamaged (toSource attrs) n]
          $ \msg'' -> case schemeId of
              SchemeMainSchemeId mid -> case msg'' of
                MainSchemeMessage mid' (MainSchemePlaceThreat _) -> mid == mid'
                _ -> False
              SchemeSideSchemeId mid -> case msg'' of
                SideSchemeMessage mid' (SideSchemePlaceThreat _) -> mid == mid'
                _ -> False
        pure e
      _ -> GreatResponsibility <$> runMessage msg attrs
    _ -> GreatResponsibility <$> runMessage msg attrs