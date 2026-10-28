module Marvel.SideScheme.SideSchemes.ReactorMeltdown (
  reactorMeltdown,
  ReactorMeltdown (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Damage
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

-- | Base threat 2 per player; a crisis icon.
reactorMeltdown :: SideSchemeCard ReactorMeltdown
reactorMeltdown = sideSchemeWith ReactorMeltdown Cards.reactorMeltdown (PerPlayer 2) (crisisL .~ True)

newtype ReactorMeltdown = ReactorMeltdown (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | When Defeated: deal 1 damage to each friendly character in play.
instance RunMessage ReactorMeltdown where
  runMessage msg s@(ReactorMeltdown attrs) = runQueueT $ case msg of
    SideSchemeMessage ident DefeatSideScheme | ident == sideSchemeId attrs -> do
      damageAllCharacters attrs (toDamage 1 FromAbility) (CharacterControlledBy AnyIdentity)
      pure s
    _ -> ReactorMeltdown <$> runMessage msg attrs
