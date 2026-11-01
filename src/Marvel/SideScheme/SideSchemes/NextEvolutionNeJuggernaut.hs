module Marvel.SideScheme.SideSchemes.NextEvolutionNeJuggernaut where

import Marvel.Villain.Cards qualified as CardsJuggernaut
import Marvel.Prelude
import Marvel.Id
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

newtype NeJuggernautSideScheme = NeJuggernautSideScheme (Attrs SideScheme)
  deriving anyclass (HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40124 :: SideSchemeCard NeJuggernautSideScheme
ne40124 = sideScheme NeJuggernautSideScheme Cards.ne40124 (PerPlayer 3)


instance IsSideScheme NeJuggernautSideScheme where
  sideSchemeAbilities a = [windowAbility a 1 (W.WindowOneOf [W.HeroDefended W.After AnyIdentity (EnemyIs card) | card <- [CardsJuggernaut.ne40118, CardsJuggernaut.ne40119, CardsJuggernaut.ne40120]])
    HeroResponse NoCost (runAbility a 1)]

instance RunMessage NeJuggernautSideScheme where
  runMessage msg a@(NeJuggernautSideScheme attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ -> do
      removeThreat you attrs 1 (SchemeWithId $ SchemeSideSchemeId $ sideSchemeId attrs)
      pure a
    _ -> NeJuggernautSideScheme <$> runMessage msg attrs
