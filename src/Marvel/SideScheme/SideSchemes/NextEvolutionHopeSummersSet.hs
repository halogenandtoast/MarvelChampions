module Marvel.SideScheme.SideSchemes.NextEvolutionHopeSummersSet where

import Marvel.Prelude
import Marvel.Status
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability
import Marvel.Attack
import Marvel.Cost
import Marvel.Count
import Marvel.Criteria
import Marvel.Damage
import Marvel.Difficulty
import Marvel.Game.Source
import Marvel.Identity.Types
import Marvel.Keyword
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.NextEvolution.Juggernaut
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Resource.Types
import Marvel.Window qualified as W
import Marvel.Card
import Marvel.Entity
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Ability (HasAbilities)
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype HopeSummersSetSideScheme = HopeSummersSetSideScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40131 :: SideSchemeCard HopeSummersSetSideScheme
ne40131 = sideScheme HopeSummersSetSideScheme Cards.ne40131 (PerPlayer 3)


instance HasModifiersFor HopeSummersSetSideScheme where
  getModifiersFor _ (AllyRef aid) _ = do
    hope <- allyMatches (AllyWithTitle "Hope Summers") aid
    pure [CannotReady | hope]
  getModifiersFor _ _ _ = pure []

instance RunMessage HopeSummersSetSideScheme where
  runMessage msg a@(HopeSummersSetSideScheme attrs) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs -> do
      hopes <- selectList (AllyWithTitle "Hope Summers")
      for_ hopes exhaust
      pure a
    _ -> HopeSummersSetSideScheme <$> runMessage msg attrs
