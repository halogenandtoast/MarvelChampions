module Marvel.SideScheme.SideSchemes.NextEvolutionMilitaryGrade where

import Marvel.Prelude
import Data.HashMap.Strict qualified as HashMap ()
import Marvel.Ability ()
import Marvel.Card
import Marvel.Cost ()
import Marvel.Criteria ()
import Marvel.Damage ()
import Marvel.Game.Source ()
import Marvel.Hand ()
import Marvel.Identity.Types
import Marvel.Keyword ()
import Marvel.Matchers hiding (UnexhaustedIdentity, UnexhaustedAlly)
import Marvel.Matchers qualified as Matchers ()
import Marvel.Modifier ( HasModifiersFor )
import Marvel.NextEvolution.Modulars ()
import Marvel.Payment ()
import Marvel.Projection ()
import Marvel.Query
import Marvel.Question ()
import Marvel.Resource.Types ()
import Marvel.Status ()
import Marvel.Trait qualified as Trait ()
import Marvel.Window qualified as W ()
import Marvel.Entity
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types


import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype MilitaryGradeSideScheme = MilitaryGradeSideScheme (Attrs SideScheme)
  deriving anyclass (IsSideScheme, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40093 :: SideSchemeCard MilitaryGradeSideScheme
ne40093 = sideScheme MilitaryGradeSideScheme Cards.ne40093 (Static 3)


instance RunMessage MilitaryGradeSideScheme where
  runMessage msg a@(MilitaryGradeSideScheme attrs) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId attrs -> do
      discardUntil FromEncounterDeck (CardWithType AttachmentType) attrs
      pure a
    WithDiscardedMatch (isTarget attrs -> True) FromEncounterDeck (EncounterCard card) -> do
      first <- selectJust FirstPlayer
      revealEncounterCard first card
      pure a
    _ -> MilitaryGradeSideScheme <$> runMessage msg attrs
