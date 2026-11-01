module Marvel.MainScheme.MainSchemes.NextEvolutionNeJuggernaut where

import Marvel.Prelude
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Attachment.Cards qualified as Attachments
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
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Ability (HasAbilities)
import Marvel.Modifier (HasModifiersFor)
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype NeJuggernautMainScheme = NeJuggernautMainScheme (Attrs MainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40121 :: MainSchemeCard NeJuggernautMainScheme
ne40121 = mainScheme NeJuggernautMainScheme Cards.ne40121 (PerPlayer 7) (PerPlayer 1) (PerPlayer 1)


instance IsMainScheme NeJuggernautMainScheme where
  mainSchemeAbilities a@(NeJuggernautMainScheme attrs) = [windowAbility a 1
    (W.MainSchemeWouldComplete $ SchemeWithId $ SchemeMainSchemeId $ mainSchemeId attrs)
    ForcedInterrupt NoCost (runAbility a 1)]

instance RunMessage NeJuggernautMainScheme where
  runMessage msg a@(NeJuggernautMainScheme attrs) = runQueueT $ case msg of
    SetupMainScheme -> do
      first <- selectJust FirstPlayer
      present <- selectAny (AttachmentIs Attachments.ne40122a)
      unless present $ revealEncounterCard first =<< genEncounterCard Attachments.ne40122a
      pure a
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> do
      cancelMatchingMessage $ \case
        CardSpecific target "resolveThreatPlacement" _ -> target == toTarget attrs
        AdvanceMainScheme sid -> sid == mainSchemeId attrs
        _ -> False
      first <- selectJust FirstPlayer
      removeThreat first attrs (mainSchemeThreat attrs) (SchemeWithId $ SchemeMainSchemeId $ mainSchemeId attrs)
      restoreJuggernautHelmet
      juggernaut >>= traverse_ \vid -> do
        placeCounters vid 1
        players <- getPlayers
        for_ players $ enemyAttacks vid
      pure a
    _ -> NeJuggernautMainScheme <$> runMessage msg attrs
