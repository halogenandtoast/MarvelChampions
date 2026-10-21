module Marvel.SideScheme.SideSchemes.TheImmortalKlaw (
  theImmortalKlaw,
  TheImmortalKlaw (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message
import Marvel.Modifier
import Marvel.Query
import Marvel.Queue
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

theImmortalKlaw :: SideSchemeCard TheImmortalKlaw
theImmortalKlaw =
  sideScheme TheImmortalKlaw Cards.theImmortalKlaw (PerPlayer 3)

newtype TheImmortalKlaw = TheImmortalKlaw (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Klaw gets +10 hit points: the modifier counts when a new stage's hit points are
-- set, and revealing or defeating this scheme changes the stage in play.
instance HasModifiersFor TheImmortalKlaw where
  getModifiersFor _ (VillainRef _) (TheImmortalKlaw _) =
    pure [HitPointModifier 10]
  getModifiersFor _ _ _ = pure []

instance RunMessage TheImmortalKlaw where
  runMessage msg s@(TheImmortalKlaw attrs) = case msg of
    SideSchemeMessage ident msg' | ident == sideSchemeId attrs -> case msg' of
      RevealSideScheme -> do
        villain <- selectJust ActiveVillain
        push $ VillainMessage villain (VillainHitPointsChanged 10)
        pure s
      DefeatSideScheme -> do
        villain <- selectJust ActiveVillain
        push $ VillainMessage villain (VillainHitPointsChanged (-10))
        pure s
      _ -> TheImmortalKlaw <$> runMessage msg attrs
    _ -> TheImmortalKlaw <$> runMessage msg attrs
