module Marvel.Environment.Environments.AgeOfApocalypseStandardIII where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Prelude
import Marvel.Card
import Marvel.Entity
import Marvel.Environment.Cards qualified as Cards
import Marvel.Environment.Types
import Marvel.Ability (HasAbilities)
import Marvel.Modifier
import Marvel.Ref
import Marvel.Game.Source
import Marvel.EncounterSet (getNemesisSet)
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Minion.Cards qualified as Minions
import Marvel.Query
import Marvel.Treachery.Treacheries.ShadowOfThePast (partitionNemesisSet)

newtype StandardIIIEnvironment = StandardIIIEnvironment (Attrs Environment)
  deriving anyclass (IsEnvironment, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

pursuedByThePastFront, pursuedByThePastBack :: EnvironmentCard StandardIIIEnvironment
pursuedByThePastFront = environment StandardIIIEnvironment Cards.pursuedByThePastFront
pursuedByThePastBack = environment StandardIIIEnvironment Cards.pursuedByThePastBack

instance RunMessage StandardIIIEnvironment where
  runMessage msg (StandardIIIEnvironment a) = runQueueT $ case msg of
    CardSpecific (isTarget a -> True) "addPursuit" (fromJSON -> Success (you,n)) -> do
      players <- getStartingPlayerCount
      let total = environmentCounters a + n
      if total < fromIntegral players + 3 then pure $ StandardIIIEnvironment a {environmentCounters = total}
      else do
        code <- getIdentityCode you
        let set = getNemesisSet code
            defs = filter (\d -> cdEncounterSet d == Just set && Minions.isNemesisMinion d) (HashMap.elems Minions.allMinions)
        present <- concatMapM (selectList . MinionIs) defs
        if null present then cardSpecific a "revealNemesis" you
          else traverse_ (`minionActivatesAgainst` you) present
        pure $ StandardIIIEnvironment a {environmentCounters = 0}
    CardSpecific (isTarget a -> True) "revealNemesis" (fromJSON -> Success you) -> do
      code <- getIdentityCode you
      cards <- findEncounterCards (CardWithType MinionType)
      let minions = filter (\card -> cdEncounterSet (getCardDef card) == Just (getNemesisSet code) && Minions.isNemesisMinion (getCardDef card)) cards
      traverse_ (revealFoundEncounterCard you) minions
      cardSpecific a "remainingNemesis" you
      pure $ StandardIIIEnvironment a {environmentCardDef = Cards.pursuedByThePastBack}
    CardSpecific (isTarget a -> True) "remainingNemesis" (fromJSON -> Success you) -> do
      cards <- selectList (NemesisSetFor you)
      let (_,schemes,rest) = partitionNemesisSet cards
      traverse_ (revealEncounterCard you) schemes
      shuffleIntoEncounterDeck rest
      pure $ StandardIIIEnvironment a {environmentCardDef = Cards.pursuedByThePastFront}
    _ -> StandardIIIEnvironment <$> runMessage msg a
