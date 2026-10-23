-- | Playing a game as its players see it: each message told to the log as it runs.
module Marvel.Game.Run (runGame, answerQuestion, runDebug) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Frontend
import Marvel.Game
import Marvel.Log (idText, sendLog)
import Marvel.Log.Narrator
import Marvel.Id
import Marvel.Message
import Marvel.Message.Identity
import Marvel.Message.Villain

narrate :: Message -> GameAppT ()
narrate msg = do
  for_ (narrationFor msg) \build -> traverse_ sendLog =<< build
  when (intoRiver msg) $ send . Board . toJSON =<< getGame
  traverse_ send (cue msg)

-- | A card dealt into the river, which waits for the board to show what came before it.
intoRiver :: Message -> Bool
intoRiver = \case
  VillainMessage _ (VillainDealtBoost _) -> True
  RevealEncounterCard {} -> True
  _ -> False

-- | What the board animates for a message, read before it runs.
cue :: Message -> Maybe FrontendMessage
cue = \case
  ShuffleEncounterDeck -> Just $ Shuffled "deck-encounter"
  ShuffleIntoEncounterDeck _ -> Just $ Shuffled "deck-encounter"
  IdentityMessage ident ShuffleDeck -> Just $ Shuffled ("deck-" <> idText ident)
  IdentityMessage ident (ShuffleIntoIdentityDeck _) -> Just $ Shuffled ("deck-" <> idText ident)
  IdentityMessage ident ShuffleIdentityDiscardBackIntoDeck -> Just $ Shuffled ("deck-" <> idText ident)
  VillainMessage _ (VillainDealtBoost card) -> Just $ BoostDealt (ecCardId card)
  RevealEncounterCard _ card -> Just $ Revealing (ecCardId card) (toCardCode card) False
  RevealBoostCard card _ -> Just $ Revealing (ecCardId card) (toCardCode card) True
  _ -> Nothing

-- | Play on until someone has to answer a question.
runGame :: GameAppT ()
runGame = runGameMessagesWith narrate

answerQuestion :: IdentityId -> Int -> GameAppT ()
answerQuestion = answerQuestionWith narrate

runDebug :: Message -> GameAppT ()
runDebug = runDebugWith narrate
