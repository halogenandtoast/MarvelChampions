module Marvel.Event.Events.VaporsOfValtorr (
  vaporsOfValtorr,
  VaporsOfValtorr (..),
) where

import Marvel.Prelude

import Data.List (nub)

import Marvel.Card.Code
import Marvel.Entity
import Marvel.Event.Cards qualified as Cards
import Marvel.Event.Types
import Marvel.Id
import Marvel.Invocation
import Marvel.Matchers
import Marvel.Message.Lifted
import Marvel.Modifier
import Marvel.Query
import Marvel.Ref

vaporsOfValtorr :: EventCard VaporsOfValtorr
vaporsOfValtorr = event VaporsOfValtorr Cards.vaporsOfValtorr

newtype VaporsOfValtorr = VaporsOfValtorr (Attrs Event)
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, HasCardCode, IsRef)

-- | Special: choose a status card in play and replace it with one of a different type
-- that character does not have: the character, then which of its status cards, then
-- what takes its place.
instance RunMessage VaporsOfValtorr where
  runMessage msg e@(VaporsOfValtorr attrs) = runQueueT $ case msg of
    EventMessage ident msg' | ident == eventId attrs -> case msg' of
      PlayedEvent {} -> do
        characters <- concatMapM (selectList . CharacterWithStatus) statusCards
        replaceable <- filterM (fmap notNull . lacking) (nub characters)
        ask "$label.chooseStatusToReplace"
          [targeting (characterRef c) $ cardSpecific attrs "character" c | c <- replaceable]
        pure e
      ResolvedEvent -> e <$ invocationResolved attrs
    CardSpecific (isTarget attrs -> True) "character" (fromJSON -> Success (c :: CharacterId)) -> do
      held <- filterM (\s -> member c <$> select (CharacterWithStatus s)) statusCards
      ask "$label.chooseStatusToReplace"
        [labeled (statusLabel s) $ cardSpecific attrs "status" (c, s) | s <- held]
      pure e
    CardSpecific (isTarget attrs -> True) "status" (fromJSON -> Success (c :: CharacterId, s)) -> do
      others <- lacking c
      ask "$label.chooseReplacementStatus"
        [ labeled (statusLabel s') do
            removeStatus c s
            giveStatus c s'
        | s' <- others
        ]
      pure e
    _ -> VaporsOfValtorr <$> runMessage msg attrs
   where
    statusCards = [minBound .. maxBound]
    lacking c = filterM (\s -> not . member c <$> select (CharacterWithStatus s)) statusCards
    ask l = chooseOnePrompted (eventController attrs) l
