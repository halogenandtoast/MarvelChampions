module Marvel.Event.Events.Reload (
  reload,
  Reload (..),
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

reload :: EventCard Reload
reload = event Reload Cards.reload

newtype Reload = Reload (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Hero Action: Ready each Tech upgrade you control.
instance RunMessage Reload where
  runMessage msg e@(Reload attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      traverse_ ready =<< selectList (upgradeControlledBy you <> UpgradeWithTrait Tech)
      pure e
    _ -> Reload <$> runMessage msg attrs
