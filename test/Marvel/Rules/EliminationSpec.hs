module Marvel.Rules.EliminationSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Minion.Types qualified as Minion
import TestImport

spec :: Spec
spec = describe "an eliminated player" do
  it "takes their cards out of play, hands their minions on and has their dealt encounter cards discarded" do
    tg <- testGame ["spider-man", "captain-marvel"]
    let p = player1 tg
        other = player2 tg
    inGame tg do
      putIntoPlay p "01064"
      minion <- addMinion p Minions.hydraMercenary
      dealt <- encounterCard Treacheries.advance
      runNow [IdentityMessage p (DealtEncounterCard dealt)]
      runNow [IdentityMessage p IdentityDefeated]
      getsGame (HashMap.member p . gamePlayers) `shouldReturn` False
      runNow [ClearRemoved]
      getsGame (HashMap.member p . entitiesPlayers . gameRemovedEntities) `shouldReturn` True
      getsGame (length . gameSupports) `shouldReturn` 0
      getsGame (fmap (Minion.minionEngagedIdentity . toAttrs) . HashMap.lookup minion . gameMinions) `shouldReturn` Just other
      encounterDiscardNow >>= (`shouldSatisfy` elem dealt)
