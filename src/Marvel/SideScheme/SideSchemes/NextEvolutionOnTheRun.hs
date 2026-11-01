module Marvel.SideScheme.SideSchemes.NextEvolutionOnTheRun where

import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.SideScheme.Cards qualified as Cards
import Marvel.SideScheme.Types
import Marvel.Ability (HasAbilities)
import Marvel.Modifier
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Name (display)
import Marvel.Query
import Marvel.Trait (Trait (Marauder))
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats
import Marvel.Hp
import Marvel.Message.Lifted

newtype OnTheRunSideScheme = OnTheRunSideScheme (Attrs SideScheme)
  deriving anyclass IsSideScheme
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

ne40108 :: SideSchemeCard OnTheRunSideScheme
ne40108 = sideScheme OnTheRunSideScheme Cards.ne40108 (Static 1)

ne40109 :: SideSchemeCard OnTheRunSideScheme
ne40109 = sideScheme OnTheRunSideScheme Cards.ne40109 (Static 4)

instance HasModifiersFor OnTheRunSideScheme where
  getModifiersFor _ target (OnTheRunSideScheme a) | toCardCode a == "40109", target == toRef a = do
    enemies <- selectList AnyEnemy
    titles <- traverse (\enemy -> getsGame $ \g -> maybe "" (display . cdName) (abilityCardDef g $ enemyRef enemy)) enemies
    pure $ [CrisisIcon | "Blockbuster" `elem` titles] <> [AmplifyIconModifier 1 | "Chimera" `elem` titles]
  getModifiersFor _ _ _ = pure []

instance RunMessage OnTheRunSideScheme where
  runMessage msg e@(OnTheRunSideScheme a) = runQueueT $ case msg of
    SideSchemeMessage sid DefeatSideScheme | sid == sideSchemeId a, toCardCode a == "40108" -> do
      first <- selectJust FirstPlayer
      let you = fromMaybe first (sideSchemeDefeatedBy a)
      searchAndPutIntoPlayEncounterCard you (CardWithType MinionType <> CardWithTrait Marauder)
      OnTheRunSideScheme <$> runMessage msg a
    _ -> OnTheRunSideScheme <$> runMessage msg a
