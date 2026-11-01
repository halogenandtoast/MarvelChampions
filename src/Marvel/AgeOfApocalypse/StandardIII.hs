module Marvel.AgeOfApocalypse.StandardIII where

import Marvel.Prelude
import Marvel.Environment.Cards qualified as Environments
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query

pursuitEnvironment :: HasGame m => m (Maybe EnvironmentId)
pursuitEnvironment = selectOne (EnvironmentIs Environments.pursuedByThePastFront)

addPursuit :: ReverseQueue m => IdentityId -> Natural -> m ()
addPursuit you n = pursuitEnvironment >>= traverse_ (\eid -> cardSpecific eid "addPursuit" (you,n))

hasPursuit :: HasGame m => m Bool
hasPursuit = pursuitEnvironment >>= maybe (pure False) (fmap (>0) . getCountersOn)
