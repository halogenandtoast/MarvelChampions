-- | Shared steps of the Mojo-seum and Spiral's show deck.
module Marvel.MojoOpening where

import Marvel.Prelude
import Marvel.Entity
import Marvel.Environment.Cards qualified as Environments
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Window qualified as W

showDeck :: DeckKey
showDeck = NamedDeckKey "mojoShow"

championEnvironments, challengerEnvironments :: (HasGame m) => m [EnvironmentId]
championEnvironments = (<>) <$> selectList (EnvironmentIs Environments.mojo39003a) <*> selectList (EnvironmentIs Environments.mojo39003b)
challengerEnvironments = (<>) <$> selectList (EnvironmentIs Environments.mojo39004a) <*> selectList (EnvironmentIs Environments.mojo39004b)

championRatings, challengerRatings :: (HasGame m) => m Natural
championRatings = sum <$> (traverse getCountersOn =<< championEnvironments)
challengerRatings = sum <$> (traverse getCountersOn =<< challengerEnvironments)

rateChampion, rateChallengers :: (ReverseQueue m) => Natural -> m ()
rateChampion n = championEnvironments >>= traverse_ (`placeCounters` n)
rateChallengers n = challengerEnvironments >>= traverse_ (`placeCounters` n)

revealShow :: (ReverseQueue m) => IdentityId -> m ()
revealShow you = do
  pile <- getPile showDeck
  for_ (headMay $ pileDeck pile) $ revealEncounterCardFrom W.RevealedFromSetAside you
