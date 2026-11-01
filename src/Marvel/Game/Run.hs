-- | Playing a game as its players see it: each message told to the log as it runs.
module Marvel.Game.Run (runGame, answerQuestion, runDebug) where

import Marvel.Prelude

import Marvel.Card
import Marvel.Ability qualified as Ability
import Marvel.Entity (toAttrs)
import Marvel.Event.Types (eventCardDef)
import Marvel.Matchers
import Marvel.Frontend
import Marvel.Game
import Marvel.Log (idText, sendLog)
import Marvel.Log.Narrator
import Marvel.Id
import Marvel.Message
import Marvel.Message.Identity ()
import Marvel.Message.Villain ()

narrate :: Message -> GameAppT ()
narrate msg = do
  when (stepBegins msg) showBoard
  for_ (narrationFor msg) \build -> traverse_ sendLog =<< build
  when (intoRiver msg) showBoard
  traverse_ send (cue msg)
  -- Show the paid event before its effect is replaced and its status cards removed.
  case msg of
    ResolveEventEffect ident (EventMessage eid _) -> do
      g <- getGame
      for_ (lookup eid (gameEvents g)) \event -> do
        let def = eventCardDef (toAttrs event)
        stunned <- identityMatches StunnedIdentity ident
        confused <- identityMatches ConfusedIdentity ident
        let statuses = ["stunned" | stunned && Ability.Attack `elem` cdAbilitySubTypes def]
              <> ["confused" | confused && Ability.Thwart `elem` cdAbilitySubTypes def]
        unless (null statuses) do
          showBoard
          send $ EventCanceled (toCardCode def) statuses
    _ -> pure ()
 where
  showBoard = send . Board . toJSON =<< getGame

-- | A step of the round beginning: the board is shown as the step before left it, so each
-- step is seen on its own whether or not anyone is asked anything during it.
stepBegins :: Message -> Bool
stepBegins = \case
  BeginPhase _ -> True
  EndPhase _ -> True
  AccelerateMainScheme -> True
  VillainPhaseStep _ -> True
  PassFirstPlayer -> True
  EndRound -> True
  _ -> False

-- | A card dealt into the river, which waits for the board to show what came before it.
intoRiver :: Message -> Bool
intoRiver = \case
  VillainMessage _ (VillainDealtBoost _) -> True
  RevealEncounterCardFrom {} -> True
  _ -> False

-- | What the board animates for a message, read before it runs.
-- RevealEncounterCard delegates to RevealEncounterCardFrom; only the latter cues
-- the reveal, so the board does not draw the same card twice.
cue :: Message -> Maybe FrontendMessage
cue = \case
  ShuffleEncounterDeck -> Just $ Shuffled "deck-encounter"
  ShuffleIntoEncounterDeck _ -> Just $ Shuffled "deck-encounter"
  IdentityMessage ident ShuffleDeck -> Just $ Shuffled ("deck-" <> idText ident)
  IdentityMessage ident (ShuffleIntoIdentityDeck _) -> Just $ Shuffled ("deck-" <> idText ident)
  IdentityMessage ident ShuffleIdentityDiscardBackIntoDeck -> Just $ Shuffled ("deck-" <> idText ident)
  VillainMessage _ (VillainDealtBoost card) -> Just $ BoostDealt (ecCardId card)
  RevealEncounterCardFrom _ _ card -> Just $ Revealing (ecCardId card) (toCardCode card) False
  RevealBoostCard card _ -> Just $ Revealing (ecCardId card) (toCardCode card) True
  _ -> Nothing

-- | Play on until someone has to answer a question.
runGame :: GameAppT ()
runGame = runGameMessagesWith narrate

answerQuestion :: IdentityId -> Int -> GameAppT ()
answerQuestion = answerQuestionWith narrate

runDebug :: Message -> GameAppT ()
runDebug = runDebugWith narrate
