module Marvel.Event.Events.GammaBlast (
  gammaBlast,
  GammaBlast (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.Upgrade.Cards qualified as Upgrades

gammaBlast :: EventCard GammaBlast
gammaBlast = event GammaBlast Cards.gammaBlast

newtype GammaBlast = GammaBlast (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Change to Gamma energy form and deal 7 damage to an enemy. If you were already in
-- Gamma energy form, this attack gains overkill.
instance RunMessage GammaBlast where
  runMessage msg e@(GammaBlast attrs) = runQueueT $ case msg of
    EventMessage ident (PlayedEvent you _ _) | ident == eventId attrs -> do
      already <- isInEnergyForm you Upgrades.gamma
      changeToEnergyForm you Upgrades.gamma
      let overkill = if already then withOverkill else id
      chooseDamage you attrs (overkill $ toDamage 7 $ FromPlayerAttack you) AttackableEnemy
      pure e
    _ -> GammaBlast <$> runMessage msg attrs
