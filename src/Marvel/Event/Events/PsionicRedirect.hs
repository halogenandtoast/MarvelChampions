module Marvel.Event.Events.PsionicRedirect where

import Marvel.Prelude
import Marvel.Ability ()
import Marvel.Card
import Marvel.Damage ()
import Marvel.Entity
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Upgrades
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Window qualified as W
import Marvel.Attack (sourceEnemy)

psionicRedirect :: EventCard PsionicRedirect
psionicRedirect = event PsionicRedirect Cards.psionicRedirect

newtype PsionicRedirect = PsionicRedirect (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance RunMessage PsionicRedirect where
  runMessage msg a@(PsionicRedirect attrs) = runQueueT $ case msg of
    EventMessage eid (PlayedEvent you _ (Just (W.IdentityTakeDamageFrom ident source _))) | eid == eventId attrs, ident == you -> do
      katanas <- selectListCount $ UpgradeControlledBy (IdentityWithId you) <> UpgradeIs Upgrades.psiKatana
      knives <- selectListCount $ UpgradeControlledBy (IdentityWithId you) <> UpgradeIs Upgrades.psiKnife
      preventDamage you (2 + 2 * katanas)
      for_ (sourceEnemy source) $ \enemy -> for_ [1..knives] $ \_ -> confuse enemy attrs
      pure a
    _ -> PsionicRedirect <$> runMessage msg attrs
