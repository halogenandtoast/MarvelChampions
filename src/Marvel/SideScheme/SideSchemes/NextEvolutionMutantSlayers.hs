module Marvel.SideScheme.SideSchemes.NextEvolutionMutantSlayers where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Ability ()
import Data.List (nub)
import Marvel.Modifier
import Marvel.Game.Source ()
import Marvel.Id ()
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Query
import Marvel.Trait (Trait (Marauder, Mutant, XFactor, XForce, XMen))
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype MutantSlayersSideScheme = MutantSlayersSideScheme (Attrs SideScheme)
  deriving anyclass IsSideScheme
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40101 :: SideSchemeCard MutantSlayersSideScheme
ne40101 = sideScheme MutantSlayersSideScheme Cards.ne40101 (Static 3)

instance HasModifiersFor MutantSlayersSideScheme where
  getModifiersFor _ (MinionRef mid) _ = do
    marauder <- selectAny (MinionWithId mid <> MinionWithTrait Marauder)
    pure [KeywordModifier Quickstrike | marauder]
  getModifiersFor _ _ _ = pure []

instance RunMessage MutantSlayersSideScheme where
  runMessage msg e@(MutantSlayersSideScheme a) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId a -> do
      characters <- nub . concat <$> traverse (selectList . CharacterWithTrait) [Mutant,XFactor,XForce,XMen]
      placeThreat sid (fromIntegral $ length characters)
      pure e
    _ -> MutantSlayersSideScheme <$> runMessage msg a
