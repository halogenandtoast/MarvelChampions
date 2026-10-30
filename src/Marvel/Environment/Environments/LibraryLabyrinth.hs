module Marvel.Environment.Environments.LibraryLabyrinth (
  libraryLabyrinth,
  museumShip,
  LibraryLabyrinth (..),
) where

import Marvel.Prelude

import Marvel.Ability
import Marvel.Card.Code
import Marvel.Cost
import Marvel.Criteria
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Game.Source
import Marvel.GameValue
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Phase
import Marvel.Query
import Marvel.Question (runAbility)
import Marvel.Ref

-- | Escape the Museum's environment: Library Labyrinth, flipped to Museum Ship by The
-- Great Escape (3A).
libraryLabyrinth :: EnvironmentCard LibraryLabyrinth
libraryLabyrinth = environment LibraryLabyrinth Cards.libraryLabyrinth

museumShip :: EnvironmentCard LibraryLabyrinth
museumShip = environment LibraryLabyrinth Cards.museumShip

newtype LibraryLabyrinth = LibraryLabyrinth (Attrs Environment)
  deriving anyclass (IsEnvironment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

isMuseumShip :: Attrs Environment -> Bool
isMuseumShip a = toCardCode a == toCardCode Cards.museumShip

-- | Library Labyrinth, "This way?" — Hero Action: deal yourself 1 facedown encounter card
-- → remove 5 threat from the main scheme. (Limit once per round per player.)
instance HasAbilities LibraryLabyrinth where
  getAbilities (LibraryLabyrinth a)
    | isMuseumShip a = []
    | otherwise = [limited (EachPlayer (PerRound 1)) $ ability a 1 HeroAction NoCriteria NoCost $ runAbility a 1]

-- | Museum Ship, "Hold on to your butts!" — Forced Interrupt: when the villain phase
-- begins, choose one: exhaust the Milano → assign 2[per hero] indirect damage among
-- players; or assign 3[per hero] indirect damage among players. The first player
-- chooses for the group; with the Milano not ready to exhaust, only the second is open.
instance RunMessage LibraryLabyrinth where
  runMessage msg e@(LibraryLabyrinth attrs) = runQueueT $ case msg of
    RanAbility you (isTarget attrs -> True) 1 _ _ | not (isMuseumShip attrs) -> do
      dealEncounterCard you
      scheme <- selectOne EncounterMainScheme
      for_ scheme \s -> thwartScheme s attrs 5
      pure e
    BeginPhase VillainPhase | isMuseumShip attrs -> do
      first <- selectJust FirstPlayer
      ready <- milanoIsReady
      two <- fromIntegral <$> fromGameValue (PerPlayer 2)
      three <- fromIntegral <$> fromGameValue (PerPlayer 3)
      chooseOrRunOne first $
        [ labeled (countLabel "exhaustTheMilanoIndirectDamageAmongPlayers" two) do
            exhaustMilano
            dealIndirectDamageAmongPlayers attrs two
        | ready
        ]
          <> [labeled (countLabel "indirectDamageAmongPlayers" three) (dealIndirectDamageAmongPlayers attrs three)]
      pure e
    _ -> LibraryLabyrinth <$> runMessage msg attrs
