module Marvel.Event.Events.RunAndGun (
  runAndGun,
  RunAndGun (..),
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
import Marvel.Trait qualified as Trait

runAndGun :: EventCard RunAndGun
runAndGun = event RunAndGun Cards.runAndGun

newtype RunAndGun = RunAndGun (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Ready Venom and each Weapon upgrade you control.
instance RunMessage RunAndGun where
  runMessage msg e@(RunAndGun attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      ready you
      weapons <- selectList $ UpgradeWithTrait Trait.Weapon <> upgradeControlledBy you
      traverse_ ready weapons
      pure e
    _ -> RunAndGun <$> runMessage msg attrs
