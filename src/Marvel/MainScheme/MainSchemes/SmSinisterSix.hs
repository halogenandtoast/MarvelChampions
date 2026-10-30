module Marvel.MainScheme.MainSchemes.SmSinisterSix where

import Marvel.Prelude
import Data.Aeson (Result (..), fromJSON)
import Marvel.Card.Code
import Marvel.Difficulty
import Marvel.Entity
import Marvel.EncounterCard (genEncounterCard)
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Id
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.SideScheme.Cards qualified as SideSchemes

newtype SmSinisterSixMainScheme = SmSinisterSixMainScheme (Attrs MainScheme)
  deriving anyclass IsMainScheme
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

smSinisterSynchronization, smSinisterBeatdown :: MainSchemeCard SmSinisterSixMainScheme
smSinisterSynchronization = mainScheme SmSinisterSixMainScheme Cards.smSinisterSynchronization (PerPlayer 8) (PerPlayer 2) (PerPlayer 1)
smSinisterBeatdown = mainScheme SmSinisterSixMainScheme Cards.smSinisterBeatdown (PerPlayer 7) (PerPlayer 3) (PerPlayer 1)

instance RunMessage SmSinisterSixMainScheme where
  runMessage msg e@(SmSinisterSixMainScheme attrs) = runQueueT $ case msg of
    AdvanceMainScheme sid | sid == mainSchemeId attrs -> do
      if toCardCode attrs == "27100" then nextMainScheme sid else gameOver Lost
      pure e
    SetupMainScheme | toCardCode attrs == "27100" -> do
      first <- selectJust FirstPlayer
      putEncounterCardIntoPlay first =<< genEncounterCard SideSchemes.smLightAtTheEndA
      pure e
    MainSchemeMessage mid RevealMainScheme | mid == mainSchemeId attrs, toCardCode attrs == "27101" -> do
      -- This is not Ambush!: its expert penalty is a facedown encounter card,
      -- whereas the Special puts two threat on Light at the End.
      cardSpecific GameRef "sinisterSixStageTwo" (Just (toTarget attrs, ("beatdownEntered" :: Text), toJSON ()))
      pure e
    CardSpecific (isTarget attrs -> True) "beatdownEntered" (fromJSON -> Success (entered, _ :: Value)) -> do
      difficulty <- getDifficulty
      when (isNothing (entered :: Maybe VillainId) || difficulty == Expert) $ selectJust FirstPlayer >>= dealEncounterCard
      pure e
    _ -> SmSinisterSixMainScheme <$> runMessage msg attrs
