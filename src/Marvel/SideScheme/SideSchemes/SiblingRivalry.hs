module Marvel.SideScheme.SideSchemes.SiblingRivalry (
  siblingRivalry,
  SiblingRivalry (..),
) where

import Marvel.Prelude

import Marvel.Card.Code
import Marvel.Entity
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Phase (Phase (VillainPhase))
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types

-- | Base threat 4.
siblingRivalry :: SideSchemeCard SiblingRivalry
siblingRivalry = sideScheme SiblingRivalry Cards.siblingRivalry (Static 4)

newtype SiblingRivalry = SiblingRivalry (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode)

-- | Players other than Gamora cannot remove threat from Sibling Rivalry.
instance HasModifiersFor SiblingRivalry where
  getModifiersFor _ (SideSchemeRef sid) (SiblingRivalry a)
    | sid == sideSchemeId a = maybe [] (pure . ThreatRemovableOnlyBy) <$> selectOne (IdentityWithTitle "Gamora")
  getModifiersFor _ _ _ = pure []

-- | Forced Response: after the villain phase begins, deal 1 facedown encounter card to
-- Gamora.
instance RunMessage SiblingRivalry where
  runMessage msg s@(SiblingRivalry attrs) = runQueueT $ case msg of
    BeginPhase VillainPhase -> do
      traverse_ dealEncounterCard =<< selectOne (IdentityWithTitle "Gamora")
      pure s
    _ -> SiblingRivalry <$> runMessage msg attrs
