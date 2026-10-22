module Marvel.SideScheme.SideSchemes.TheRedHouse (
  theRedHouse,
  TheRedHouse (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

theRedHouse :: SideSchemeCard TheRedHouse
theRedHouse = sideScheme TheRedHouse Cards.theRedHouse (PerPlayer 3)

newtype TheRedHouse = TheRedHouse (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Red Skull cannot take damage. Interrupt: a character thwarting this scheme may use
-- their ATK instead of their THW.
instance HasModifiersFor TheRedHouse where
  getModifiersFor _ (VillainRef _) _ = pure [CannotTakeDamage]
  getModifiersFor _ (SideSchemeRef sid) (TheRedHouse a) | sid == sideSchemeId a = pure [ThwartWithAttack]
  getModifiersFor _ _ _ = pure []

instance RunMessage TheRedHouse where
  runMessage msg (TheRedHouse attrs) = TheRedHouse <$> runMessage msg attrs
