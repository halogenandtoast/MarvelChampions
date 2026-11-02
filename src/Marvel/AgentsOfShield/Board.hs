{-# OPTIONS_GHC -Wno-unused-imports #-}
module Marvel.AgentsOfShield.Board where

import Marvel.Prelude
import Marvel.AgentsOfShield.Helpers
import Marvel.Card
import Marvel.Game.Source
import Marvel.Id
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Query
import Marvel.Ref
import Marvel.Trait qualified as Trait

boardMembers :: HasGame m => m [Target]
boardMembers = (<>) <$> selectRefs (EnvironmentWithTrait Trait.BoardMember)
  <*> selectRefs (AttachmentWithTrait Trait.BoardMember)

addSecretsToLeast :: ReverseQueue m => Natural -> m ()
addSecretsToLeast n = do
  members <- selectRefs (EnvironmentWithTrait Trait.BoardMember)
  scored <- traverse (\t -> (t,) <$> getCountersOn t) members
  unless (null scored) do
    first <- selectJust FirstPlayer
    chooseEffect first [targeting t (placeCounters t n) | (t,c) <- scored, c == minimum (map snd scored)]

addSecretsToAll :: ReverseQueue m => Natural -> m ()
addSecretsToAll n = boardMembers >>= traverse_ (`placeCounters` n)

removeSecrets :: ReverseQueue m => IdentityId -> Natural -> m ()
removeSecrets _ 0 = pure ()
removeSecrets you n = do
  members <- selectRefs (EnvironmentWithTrait Trait.BoardMember)
  legal <- filterM (fmap (>0) . getCountersOn) members
  chooseEffect you [targeting t do
    removeCounters t 1
    cardSpecific t "aosRemoveMoreSecrets" (you,n-1)
    | t <- legal]

-- The mole's printed reverse side keeps the same physical counters.
flipBoardMember :: ReverseQueue m => EnvironmentId -> m ()
flipBoardMember eid = cardSpecific eid "aosFlipBoardMember" ()
