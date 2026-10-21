-- | Playing a game as its players see it: each message told to the log as it runs.
module Marvel.Game.Run (runGame, answerQuestion, runDebug) where

import Marvel.Prelude

import Marvel.Game
import Marvel.Log (sendLog)
import Marvel.Log.Narrator
import Marvel.Id
import Marvel.Message

narrate :: Message -> GameAppT ()
narrate msg = for_ (narrationFor msg) \build -> traverse_ sendLog =<< build

-- | Play on until someone has to answer a question.
runGame :: GameAppT ()
runGame = runGameMessagesWith narrate

answerQuestion :: IdentityId -> Int -> GameAppT ()
answerQuestion = answerQuestionWith narrate

runDebug :: Message -> GameAppT ()
runDebug = runDebugWith narrate
