module Marvel.SideScheme.SideSchemes.NextEvolutionNastyBoys where

import Marvel.Prelude
import Data.HashMap.Strict qualified as HashMap
import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Criteria
import Marvel.Damage
import Marvel.Game.Source
import Marvel.Hand
import Marvel.Identity.Types
import Marvel.Keyword
import Marvel.Matchers hiding (UnexhaustedIdentity, UnexhaustedAlly)
import Marvel.Matchers qualified as Matchers
import Marvel.Modifier
import Marvel.NextEvolution.Modulars
import Marvel.Payment
import Marvel.Projection
import Marvel.Query
import Marvel.Question
import Marvel.Resource.Types
import Marvel.Status
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W
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

newtype NastyBoysSideScheme = NastyBoysSideScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40117 :: SideSchemeCard NastyBoysSideScheme
ne40117 = sideScheme NastyBoysSideScheme Cards.ne40117 (Static 3)


instance HasModifiersFor NastyBoysSideScheme where
  getModifiersFor _ (MinionRef _) _ = pure [AttackModifier 1]
  getModifiersFor _ _ _ = pure []

instance RunMessage NastyBoysSideScheme where
  runMessage msg a@(NastyBoysSideScheme attrs) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs -> do
      minions <- selectList AnyMinion
      nasty <- selectList (MinionWithTrait Trait.NastyBoy)
      placeThreat (sideSchemeId attrs) (fromIntegral $ length minions + length nasty)
      cardSpecific attrs "searchNasty" ()
      pure a
    CardSpecific (isTarget attrs -> True) "searchNasty" _ -> do
      you <- getYou
      searchAndRevealEncounterCard you (CardWithTrait Trait.NastyBoy <> CardWithType MinionType)
      pure a
    _ -> NastyBoysSideScheme <$> runMessage msg attrs
