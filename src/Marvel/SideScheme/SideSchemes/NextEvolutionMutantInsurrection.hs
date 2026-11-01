module Marvel.SideScheme.SideSchemes.NextEvolutionMutantInsurrection where

import Marvel.Prelude
import Data.HashMap.Strict qualified as HashMap ()
import Marvel.Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Game.Source
import Marvel.Hand ()
import Marvel.Identity.Types
import Marvel.Keyword
import Marvel.Matchers hiding (UnexhaustedIdentity, UnexhaustedAlly)
import Marvel.Matchers qualified as Matchers ()
import Marvel.Modifier
    ( HasModifiersFor(getModifiersFor), Modifier(KeywordModifier) )
import Marvel.NextEvolution.Modulars ()
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Resource.Types ()
import Marvel.Status ()
import Marvel.Trait qualified as Trait
import Marvel.Window qualified as W ()
import Marvel.Entity ()
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types


import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype MutantInsurrectionSideScheme = MutantInsurrectionSideScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40189 :: SideSchemeCard MutantInsurrectionSideScheme
ne40189 = sideScheme MutantInsurrectionSideScheme Cards.ne40189 (Static 3)


instance HasModifiersFor MutantInsurrectionSideScheme where
  getModifiersFor _ (MinionRef _) _ = pure [KeywordModifier Toughness]
  getModifiersFor _ _ _ = pure []

instance RunMessage MutantInsurrectionSideScheme where
  runMessage msg a@(MutantInsurrectionSideScheme attrs) = runQueueT $ case msg of
    SideSchemeMessage sid RevealSideScheme | sid == sideSchemeId attrs -> do
      characters <- selectList (CharacterWithTrait Trait.MutantLiberationFront)
      if null characters
        then getYou >>= surge
        else placeThreat (sideSchemeId attrs) (2 * fromIntegral (length characters))
      pure a
    _ -> MutantInsurrectionSideScheme <$> runMessage msg attrs
