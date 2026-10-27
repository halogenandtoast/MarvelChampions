module Marvel.SideScheme.SideSchemes.KlyntarFrenzy (
  klyntarFrenzy,
  KlyntarFrenzy (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Trait

-- | Base threat 2 per player.
klyntarFrenzy :: SideSchemeCard KlyntarFrenzy
klyntarFrenzy = sideScheme KlyntarFrenzy Cards.klyntarFrenzy (PerPlayer 2)

newtype KlyntarFrenzy = KlyntarFrenzy (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | "Threat cannot be removed from this scheme while a Symbiote enemy is in play."
instance HasModifiersFor KlyntarFrenzy where
  getModifiersFor _ (SideSchemeRef sid) (KlyntarFrenzy a)
    | sid == sideSchemeId a = do
        symbiote <- selectAny (EnemyWithTrait Symbiote)
        pure [ThreatCannotBeRemoved | symbiote]
  getModifiersFor _ _ _ = pure []

instance RunMessage KlyntarFrenzy where
  runMessage msg (KlyntarFrenzy attrs) = KlyntarFrenzy <$> runMessage msg attrs
