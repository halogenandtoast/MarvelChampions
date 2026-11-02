{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.AgentsOfShield.NemesisHelpers where
import Marvel.Prelude
import Marvel.AgentsOfShield.Helpers
import Marvel.AgentsOfShield.EncounterCards qualified as Encounter
import Marvel.Card
import Marvel.Environment.Cards qualified as Environments
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref

removeSupportCounter :: ReverseQueue m => IdentityId -> m ()
removeSupportCounter you = do
  supports <- selectList (SupportControlledBy AnyIdentity)
  eligible <- filterM (fmap (>0) . getCountersOn) supports
  chooseEffect you [targeting sid (removeCounters sid 1) | sid <- eligible]

addControlledMinion :: ReverseQueue m => IdentityId -> m ()
addControlledMinion you = do
  present <- selectAny (EnvironmentIs Environments.aosP50032)
  when present $ cardSpecific GameRef "aosControlledMinion" you
