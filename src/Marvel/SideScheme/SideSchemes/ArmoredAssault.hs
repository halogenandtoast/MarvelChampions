module Marvel.SideScheme.SideSchemes.ArmoredAssault where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Status

armoredAssault :: SideSchemeCard ArmoredAssault
armoredAssault = sideScheme ArmoredAssault Cards.armoredAssault (PerPlayer 3)

newtype ArmoredAssault = ArmoredAssault (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

instance HasModifiersFor ArmoredAssault where
  getModifiersFor _ target _ = case target of
    MinionRef mid -> bonus (MinionCharacter mid)
    VillainRef vid -> bonus (VillainCharacter vid)
    _ -> pure []
   where
    bonus character = do
      tough <- characterMatches (CharacterWithStatus ToughStatus) character
      pure [AttackModifier 3 | tough]

instance RunMessage ArmoredAssault where
  runMessage msg (ArmoredAssault attrs) = ArmoredAssault <$> runMessage msg attrs
