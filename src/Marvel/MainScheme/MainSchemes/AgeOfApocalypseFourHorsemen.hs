module Marvel.MainScheme.MainSchemes.AgeOfApocalypseFourHorsemen where

import Marvel.Prelude
import Marvel.Ability
import Marvel.Cost
import Marvel.Criteria
import Marvel.Effect.Types
import Marvel.Game.Source
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Phase
import Marvel.Query ()
import Marvel.Question
import Marvel.Scenario.AgeOfApocalypseFourHorsemen
import Marvel.Window qualified as W
import System.Random.Shuffle (shuffleM)
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Card
import Marvel.Entity
import Marvel.MainScheme.Cards qualified as Cards
import Marvel.MainScheme.Types
import Marvel.Ability ()
import Marvel.Modifier ()
import Marvel.Ref
import Marvel.GameValue
import Marvel.Stats ()
import Marvel.Hp ()
import Marvel.Message.Lifted

newtype AoaFourHorsemenMainScheme = AoaFourHorsemenMainScheme (Attrs MainScheme)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

theHorsemenOfApocalypse :: MainSchemeCard AoaFourHorsemenMainScheme
theHorsemenOfApocalypse = mainScheme AoaFourHorsemenMainScheme Cards.theHorsemenOfApocalypse (PerPlayer 12) (Static 0) (PerPlayer 1)


instance IsMainScheme AoaFourHorsemenMainScheme where
  mainSchemeModifiersFor _ GameRef _ = pure [PlayersChooseTheVillain]
  mainSchemeModifiersFor _ _ _ = pure []
  mainSchemeAbilities a = [limitedWindowAbility a 1 (W.EnemyActivates W.After VillainEnemy AnyIdentity)
    ForcedResponse NoCriteria NoCost (runAbility a 1)]

instance RunMessage AoaFourHorsemenMainScheme where
  runMessage msg a@(AoaFourHorsemenMainScheme attrs) = runQueueT $ case msg of
    SetupMainScheme -> do
      players <- getPlayers
      shuffled <- shuffleM [SideSchemes.theRavagesOfWar, SideSchemes.aTimeOfFamine, SideSchemes.plagueAndPestilence, SideSchemes.theSpecterOfDeath]
      for_ (zip players shuffled) $ \(you, def) -> searchAndRevealEncounterCard you $ CardIs def
      pure a
    RanAbility _ (isTarget attrs -> True) 1 _ _ -> a <$ moveHorsemanCounter
    _ -> AoaFourHorsemenMainScheme <$> runMessage msg attrs

newtype AoaHorsemenBlankEffect = AoaHorsemenBlankEffect (Attrs Effect)
  deriving anyclass IsEffect
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsRef)

aoaHorsemenBlankEffect :: CardEffect AoaHorsemenBlankEffect
aoaHorsemenBlankEffect = effectWith AoaHorsemenBlankEffect Cards.theHorsemenOfApocalypse (modifiersL .~ [Blank])

instance RunMessage AoaHorsemenBlankEffect where
  runMessage msg e@(AoaHorsemenBlankEffect attrs) = runQueueT $ case msg of
    BeginPhase VillainPhase -> e <$ disableEffect (effectId attrs)
    _ -> AoaHorsemenBlankEffect <$> runMessage msg attrs
