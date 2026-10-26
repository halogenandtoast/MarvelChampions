module Marvel.Support.Supports.BootCamp (
  bootCamp,
  BootCamp (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.Support.Cards qualified as Cards
import Marvel.Support.Types

bootCamp :: SupportCard BootCamp
bootCamp = support BootCamp Cards.bootCamp

newtype BootCamp = BootCamp (Attrs Support)
  deriving anyclass (IsSupport, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Play under any player's control. Max 1 per player. Each ally you control gets +1 ATK.
instance HasModifiersFor BootCamp where
  getModifiersFor _ (AllyRef aid) (BootCamp a) = do
    yours <- selectAny $ AllyWithId aid <> AllyControlledBy (IdentityWithId $ supportController a)
    pure [AttackModifier 1 | yours]
  getModifiersFor _ _ _ = pure []

instance RunMessage BootCamp where
  runMessage msg (BootCamp attrs) = BootCamp <$> runMessage msg attrs
