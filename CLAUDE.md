# Marvel Champions

The Haskell engine is in `src/Marvel`, the API in `app/`, the client in `frontend/`. The
server and much of the client follow `~/Code/LordOfTheRings`; look up how it does a
feature before adding one. Rules are in `docs/rules/` (Rules Reference v1.8): check a
mechanic there before implementing it.

## The engine

- `runMessage` handlers return the entity and say what follows through the lifted layer
  (below); it runs in the order written, before anything already on the queue.
- Messages addressed to one entity are wrapped: `IdentityMessage ident msg`,
  `VillainMessage vid msg`, and so on. Fall through with `_ -> X <$> runMessage msg attrs`.
- Questions are `Ask ident (ChooseOne choices)`. An empty question is never asked.
- Randomness comes from the game's stored generator (`GameAppT`'s `MonadRandom`), so a
  reloaded game and an undone step roll the same. Never use IO randomness.
- The client is sent `Marvel.Game.Public`'s view, not the engine's JSON. When the board
  needs something new, add it to the view.
- `test/Marvel/SmokeSpec.hs` deals every starter deck against each scenario and answers
  at random. Keep it passing; a crash there is an engine bug.
- Every core card has a behaviour test that plays it as a player would
  (`test/Marvel/Hero/`, `test/Marvel/Aspect/`, `test/Marvel/Encounter/`), and
  `CoverageSpec` checks each MarvelCDB core code is implemented. A new card gets a test
  in the same style: `testGame [heroes]`, `inGame`, `putInHand`, `playWith`, `choose`,
  `offers` and friends from `test/TestImport.hs`.
- Damage to a hero from anything but an attack goes through `damageIdentity`, and threat a
  card places through `placeThreatOn`, so "would take damage" and "would place threat"
  interrupts can answer it.
- Labels the client shows are i18n keys (`$label.…`); add the English to
  `frontend/src/locales/en/label.json`. Log keys live in `log.json`.

## The lifted layer

Every handler, card or engine, is written on it; nothing outside the queue itself and the
game loop pushes a message.

- A handler is `runMessage msg e@(X attrs) = runQueueT $ case msg of ...`, falling through
  with `_ -> X <$> runMessage msg attrs`. Under `runQueueT` what it does runs in the order
  written; queue edits (`preventDamage`, `cancelWhenRevealed`, `insteadOf`) still see the
  game's queue, not what the handler has done so far. What must run before the rest of a
  handler's work, though decided after it, is a second `runQueueT` (see
  `RunMessage PlayerIdentity`).
- A card imports `Marvel.Message.Lifted` (helpers in `src/Marvel/Message/Lifted.hs`, by
  theme; `QueueT` in `Lifted/Queue.hs`) instead of `Marvel.Message`, `Marvel.Choice` and
  `Marvel.Queue`, and never pushes a message itself: `dealDamage enemy attrs dmg`,
  `removeThreat you attrs 2 ThwartableScheme`, `drawCards you 1`, `cardSpecific attrs k v`.
  A missing one is added there under its theme, one line, its arguments any id that names
  the element (`IsEnemy`, `IsCharacter`, `IsScheme` in `Marvel.Ref`, or `IsRef`). An
  element's own plumbing is its step: `identityStep`, `villainStep`, `minionStep`, …
- A helper that skips the window its message would open says so:
  `revealEncounterCardWithoutWindows`, `placeThreatWithoutWindow`.
- A question is its choices, each built from what choosing it does:
  `chooseOne you [targeting (enemyRef enemy) (dealDamage enemy attrs dmg) | enemy <- enemies]`,
  `labeled "$label.done" (pure ())`. What a choice does is worked out as it is asked, so a
  step that must see the game after an earlier one is a `cardSpecific` step of its own.
- Keep the target a choice is offered on as it was (`enemyRef` for a villain's or minion's
  own ref, `characterRef`): tests and the client find choices by it.

Card text is in `frontend/public/data/cards.json` (MarvelCDB, rebuilt by
`tools/generate-cards.py`).

## The game log

The log is derived from the message stream: `Marvel.Log.Narrator` says what each
`Message` means, read before the message runs. Cards write nothing. Every English string
is under `log.` in `frontend/src/locales/en/log.json`.

## Commits

Small, focused commits with no Co-Authored-By or other AI trailer. Stage only your own
changes.
