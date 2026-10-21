# Marvel Champions

The Haskell engine is in `src/Marvel`, the API in `app/`, the client in `frontend/`. The
server and much of the client follow `~/Code/LordOfTheRings`; look up how it does a
feature before adding one. Rules are in `docs/rules/` (Rules Reference v1.8): check a
mechanic there before implementing it.

## The engine

- `runMessage` handlers return the entity and `push`/`pushAll` what follows. A pushed
  list runs in the order written, before anything already on the queue.
- Messages addressed to one entity are wrapped: `IdentityMessage ident msg`,
  `VillainMessage vid msg`, and so on. Fall through with `_ -> X <$> runMessage msg attrs`.
- Questions are `Ask ident (ChooseOne choices)`. An empty question is never asked.
- Randomness comes from the game's stored generator (`GameAppT`'s `MonadRandom`), so a
  reloaded game and an undone step roll the same. Never use IO randomness.
- The client is sent `Marvel.Game.Public`'s view, not the engine's JSON. When the board
  needs something new, add it to the view.
- `test/Marvel/SmokeSpec.hs` deals every starter deck against each scenario and answers
  at random. Keep it passing; a crash there is an engine bug.

## The game log

The log is derived from the message stream: `Marvel.Log.Narrator` says what each
`Message` means, read before the message runs. Cards write nothing. Every English string
is under `log.` in `frontend/src/locales/en/log.json`.

## Commits

Small, focused commits with no Co-Authored-By or other AI trailer. Stage only your own
changes.
