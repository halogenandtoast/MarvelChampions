module Marvel.Support.Supports.MightyAvengers (
  mightyAvengers,
  MightyAvengers (..),
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
import Marvel.Trait (Trait (Avenger))

mightyAvengers :: SupportCard MightyAvengers
mightyAvengers = support MightyAvengers Cards.mightyAvengers

newtype MightyAvengers = MightyAvengers (Attrs Support)
  deriving anyclass (IsSupport, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Play under any player's control. Max 1 Team per player (the def). If each of your
-- characters (your identity, in the form it is in, and every ally you control) has the
-- Avenger trait, each ally you control gets +1 THW and +1 ATK.
instance HasModifiersFor MightyAvengers where
  getModifiersFor _ (AllyRef aid) (MightyAvengers a) = do
    let you = IdentityWithId $ supportController a
    yours <- selectAny $ AllyWithId aid <> AllyControlledBy you
    if not yours
      then pure []
      else do
        identity <- selectAny $ you <> IdentityWithTrait Avenger
        others <- selectAny $ AllyControlledBy you <> NotAlly (AllyWithTrait Avenger)
        pure [m | identity, not others, m <- [ThwartModifier 1, AttackModifier 1]]
  getModifiersFor _ _ _ = pure []

instance RunMessage MightyAvengers where
  runMessage msg (MightyAvengers attrs) = MightyAvengers <$> runMessage msg attrs
