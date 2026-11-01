module Marvel.Environment.Environments.MojoOpening where

import Marvel.Ability
import Marvel.Card
import Marvel.Cost
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Modifier
import Marvel.MojoOpening
import Marvel.Prelude
import Marvel.Query
import Marvel.Ref

newtype MojoOpeningEnvironment = MojoOpeningEnvironment (Attrs Environment)
  deriving anyclass (IsEnvironment, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

mojo39003a, mojo39003b, mojo39004a, mojo39004b :: EnvironmentCard MojoOpeningEnvironment
mojo39003a = environment MojoOpeningEnvironment Cards.mojo39003a
mojo39003b = environment MojoOpeningEnvironment Cards.mojo39003b
mojo39004a = environment MojoOpeningEnvironment Cards.mojo39004a
mojo39004b = environment MojoOpeningEnvironment Cards.mojo39004b

instance RunMessage MojoOpeningEnvironment where
  runMessage msg e@(MojoOpeningEnvironment a) = runQueueT $ case msg of
    PlaceCounters target _ | target == toRef a -> do
      updated <- runMessage msg a
      players <- fromIntegral <$> getStartingPlayerCount
      let n = environmentCounters updated
      when (toCardCode updated `elem` ["39003a", "39004a"] && n >= 5 * players) do
        flipEnvironment (environmentId a)
        cardSpecific a "crowdFlipped" ()
      when (n >= 10 * players) $ gameOver (if toCardCode a `elem` ["39003a", "39003b"] then Lost else Won)
      pure $ MojoOpeningEnvironment updated
    CardSpecific (isTarget a -> True) "crowdFlipped" _ -> do
      if toCardCode a == "39003b"
        then getPlayers >>= traverse_ (`drawCards` 1)
        else do
          first <- selectJust FirstPlayer
          contender <- selectList (MinionIs Minions.mojo39007)
          if null contender
            then searchAndPutIntoPlayEncounterCard first (CardIs Minions.mojo39007)
            else traverse_ (`giveStatus` ToughStatus) contender
      pure e
    _ -> MojoOpeningEnvironment <$> runMessage msg a
