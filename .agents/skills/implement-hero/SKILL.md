---
name: implement-hero
description: Implement a Marvel Champions hero pack end to end — pull its cards from MarvelCDB data, map the new rules and mechanics, build the engine support, then implement and test the cards in parallel batches with subagents while one build owner runs every stack command. Use when asked to add, implement or port a hero or hero pack (e.g. "implement Wasp", "add the wsp pack").
---

# Implement a hero pack

You are the **orchestrator** and the **only build owner**. Subagents write code; they never
run `stack`, `cabal`, `ghc`, `hpack` or `git`. One shared working tree, one `.stack-work`,
one build at a time — no worktrees (each would rebuild the whole engine from scratch and
fight over `~/.stack`). The Ant-Man pack (commits `57d47df16`, `9a5c42ca2`, `eff7b5505`) is
the worked example of every step below; read its diffs when unsure how something is wired.

Follow AGENTS.md throughout: the lifted layer, `damageIdentity` / `placeThreatOn`, i18n
labels, the narrator for logs, `Marvel.Game.Public` for anything the board must show.

## Visible progress

Show a visual progress bar in commentary when starting, at each phase transition, and
in ongoing status updates. Include the hero, an approximate percentage, the current
step, and what remains. Render it in the conversation UI; this does not call for a
progress feature in the game client. For example:

**Nova pack — approximately 30% complete**
`██████░░░░░░░░░░░░░░ 30%` — Engine foundation compiling; card behavior and tests next.

Use these approximate milestones: gather/plan 10–20%, engine/scaffold 20–35%, card
batches 35–70%, targeted/full tests 70–90%, independent audit and fixes 90–95%,
committed and verified 100%. Advance on completed work, not elapsed time. While a
build, test, or ruling holds progress, keep the percentage steady and name that step.
Keep updates concise and report actual failures; never show 100% with required work
remaining. Include a final bar with the completion report.

## 0. The tree

Run `git status`. Preserve unrelated uncommitted changes and work alongside them; ask
only if they conflict with the requested work. Save `git diff > <scratchpad>/foreign.patch` now. To
commit later in a file both touched, stage the file minus their hunks: copy it,
`patch -R` their hunks for that file onto the copy, then
`git update-index --cacheinfo 100644,$(git hash-object -w copy),<file>`. Check
`git diff --cached` shows none of theirs. Tell every subagent the edits are someone
else's and to leave them alone.

## 1. Gather

1. Find the pack: `python3 -I .agents/skills/implement-hero/pack.py <hero name or pack code>`
   prints every card grouped by set (hero set, nemesis set, aspect, basic) with stats,
   resources, traits, text, quantity and reprints (`= <code> (in engine | NOT in engine)`).
   Data is the local MarvelCDB dump `frontend/public/data/cards.json`. If the pack is
   missing or stale, run `tools/generate-cards.py` (fetches https://marvelcdb.com/api/public).
   For a card's raw MarvelCDB record: `https://marvelcdb.com/api/public/card/<code>.json`.
2. Rules: the pack's rulesheet (FFG PDF, same CDN as the sources in `docs/rules/README.md`;
   ask the user for the URL if you can't find it) and the RR v1.8 in `docs/rules/`. Grep
   `docs/rules/` for every keyword and term the cards use. Transcribe the rulesheet's new
   rules and Q&A to `docs/rules/hero-<slug>.md` and add it to the README table, like
   `hero-ant-man.md`.
3. Check MarvelCDB against the printed card where it matters (stats, costs, traits):
   MarvelCDB has errors (it lists Tiny Ant-Man at ATK 2; printed 1). Rulesheet, card text
   and errata (`docs/rules/faq-errata.md`) beat MarvelCDB.

## 2. Map and plan

Write the plan to `<scratchpad>/<slug>-plan.md`. For every card:

| code | name | type | batch | what it does in engine terms | nearest implemented analog | new mechanic? |

- **Reprints**: `= <code> (in engine)` needs no module — add the new code with
  `reprintedAs [...]` on the existing def (see `nickFury` in `src/Marvel/Ally/Cards.hs`).
  A reprint of a card not yet in the engine is implemented under its original code and
  `reprintedAs` this one.
- **Analog**: grep `src/Marvel` for a card worded the same way (`grep -rn "After you change
  form" src/Marvel`, a trait, a window). Name the file; agents copy its shape.
- **New mechanics**: anything no card does yet — a keyword, trait, window, modifier, cost
  kind, counter, form rule, deck-building rule, something the client must show. For each:
  the rule reference, the engine files it touches (`Window/Types.hs`, `Modifier/Types.hs`,
  `Cost/Types.hs`, `Trait/Types.hs`, `Keyword.hs`, `Matchers/Types.hs`,
  `Message/Lifted.hs`, `Identity.hs`, `Game/Public.hs`, …), and the test that proves it.
- **Starter deck**: the pack's prebuilt deck (rulesheet / quantities) for
  `src/Marvel/Decklist.hs`.

Then show the user the plan's new-mechanic list and **every open ruling** (MarvelCDB vs
printed, ambiguous timing, interactions the rules don't settle) in one `functions.request_user_input_async`
batch. Record their answers under "Rulings (from the project owner, <date>)" in the
`hero-<slug>.md` file. Don't guess a ruling; don't ship a known simplification (memory:
rules correct before sign-off).

## 3. Engine (serial)

Engine changes touch shared files, so they go first and one writer at a time: do them
yourself, or one subagent with the whole list. Each mechanic gets a test (in
`test/Marvel/Rules/` if general, else in the hero spec once it exists). Build, run the
tests, keep `SmokeSpec` green. Commit: `Engine for <Hero>: <mechanics>` with a bullet per
mechanic (style of `57d47df16`).

## 4. Scaffold (serial)

Before any fan-out, make every shared file final so batch agents never touch one. One
subagent (or you) adds, for every non-reprint card:

- its `CardDef` in `src/Marvel/<Type>/Cards.hs` (code, name, cost, traits, resources,
  aspect, and the criteria / cost kind / keywords the plan settled), under a `-- <Hero>`
  heading, and its entry in that file's `all<Type>s` list;
- a stub module `src/Marvel/<Type>/<Type>s/<CardName>.hs` with the stats and no behaviour,
  copied from the nearest vanilla card of the type (`Ally/Allies/GiantMan.hs` minus its
  modifier), its import in `<Type>/<Type>s.hs` and `Some<Type>Card` in `<Type>.hs`;
- hero / alter-ego forms the same way (`Hero/Cards.hs`, `Hero/Heroes/`, `AlterEgo/…`);
- the starter deck in `Decklist.hs`, the pack code in `CoverageSpec`'s `packs`;
- empty spec modules, one per batch (below), `spec = describe "<…>" do pure ()`.

hpack picks up new modules; `MarvelChampions.cabal` regenerates on build — commit it.
Build (`stack build --fast --test --no-run-tests`) until green; `CoverageSpec` for the pack
should pass now.

## 5. Batches (parallel)

Split the cards so **each agent owns its card modules and exactly one spec file**:

| batch | cards | spec file |
|---|---|---|
| hero | identity forms, alter-ego, hero-set signature cards | `test/Marvel/Hero/<Hero>Spec.hs` |
| aspect | the pack's new aspect and basic cards | `test/Marvel/Aspect/<Hero>PackSpec.hs` |
| nemesis | obligation, nemesis set (minion, side scheme, treacheries, attachments) | `test/Marvel/Encounter/<Hero>NemesisSpec.hs` |

Split a batch over ~10 behaviour cards into two, the second with its own spec file
(`<Hero>SignatureSpec.hs`); hspec-discover finds it (Wasp: forms + upgrades, signature
events + obligation, aspect/basic, nemesis). At most 4 agents at once. Put the shared
rules in one brief file in the scratchpad and point every agent at it. Skip a batch
with nothing to do (vanilla cards need no agent — the stub is the card; still test them).

Spawn them with `collaboration.spawn_agent` in the shared working tree (no isolation). Each brief
contains, verbatim: the card rows and MarvelCDB text for its cards (from `pack.py <pack>
<codes…>`), the plan's analog for each, the rulings, the list of files it owns, and these
rules:

> You are implementing <cards> for the <Hero> pack in /Users/halogenandtoast/Code/MarvelChampions.
> Read AGENTS.md first and follow it (lifted layer, i18n labels, `damageIdentity`,
> `placeThreatOn`). Check each card's mechanic in `docs/rules/`.
> - Edit ONLY these files: <owned files>. Read anything.
> - NEVER run stack, cabal, ghc, hpack, make or git. Another agent shares this tree and
>   the orchestrator owns the build; it will send you compile and test errors.
> - Need anything outside your files (a lifted helper, a modifier, a window, a CardDef
>   change, a `label.json` / `log.json` key, a `TestImport` helper)? Don't add it. Write
>   code that uses it under the name you'd give it, and list it in your report with its
>   exact text and file.
> - Every card gets a behaviour test in your spec file, played as a player would, in the
>   style of `test/Marvel/Hero/AntManSpec.hs`: `testGame ["<deck>"]`, `inGame`,
>   `putInHand`, `playWith`, `choose`, `offers`, … from `test/TestImport.hs`. Test each
>   clause of the card text, including "instead", limits, and when it must NOT trigger.
> - Test pitfalls seen before: `resolve` / `addMinion` / `putIntoPlay` answer every
>   question with its first choice, so a window they open is already answered — use
>   `revealNow` and answer it yourself; `handSizeOf` is the hand-size stat, count cards
>   with `length <$> handOf p`; `isAbilityOfCard` also matches an ally's basic THW/ATK;
>   put cards in hand before the step whose window should offer them.
> - Report: files changed; per card, done or what's missing; requests for shared
>   additions (exact text); any rules question you couldn't settle.

While they work, do nothing that edits their files. When all have reported:

1. Apply their shared-file requests yourself (dedupe; one line per lifted helper under its
   theme; English for labels in `frontend/src/locales/en/label.json`).
2. Build: `stack build --fast --test --no-run-tests 2>&1 | grep -A12 -E "error|rror:"`.
   Group errors by file → owner, and `collaboration.send_message` each owner its errors (exact GHC text).
   Wait for every owner's reply before the next build. Repeat until green. Fix errors in
   files you own yourself.
3. Test: `stack test --fast --ta '--match "<Hero>"'` (and the batch spec names). Route
   failures to owners the same way, with the failing expectation and the test name.
4. When green, run every non-smoke test (including `CoverageSpec`) with
   `stack --work-dir .stack-work-codex test --fast --ta '--skip Marvel.Smoke --fail-fast +RTS -N2 -RTS'`,
   then focused smoke verification: `SMOKE_HEROES=<starter-deck-name> SMOKE_MODULARS=<new-set-name>
   stack --work-dir .stack-work-codex test --fast --ta '--match Marvel.Smoke --fail-fast --jobs 2 +RTS -N2 -A32m -RTS'`.
   Omit `SMOKE_MODULARS` if there is no new modular set; both focus variables accept
   comma-separated names (`SMOKE_MODULARS` uses the engine's set name, e.g. `ZzzaxSet`). This keeps every focused hero case (including solo against
   every scenario on both difficulties) and each new modular set against every scenario,
   then samples 50 distinct cases from the rest. The runner prints `SMOKE_SAMPLE_SEED`;
   record it so the exact sample can be replayed. Keep four game seeds per case and the
   five-second new-question watchdog. Stop on the first timeout or crash and eliminate
   it before continuing; do not sweep through potentially cascading failures or relax
   the deadline. Filter large candidate sets with pure/cheap checks before modifier,
   trait, criteria and payment searches. A smoke crash is an engine bug: find which card,
   send it to its owner or fix the engine yourself. Full smoke sweeps are optional unless
   explicitly requested; `SMOKE_FULL=1` still enables the exhaustive catalogue.

Always build and test with `stack --work-dir .stack-work-codex …` (every command above):
the user usually has `make api.watch` rebuilding `.stack-work` with other flags, and
sharing it makes both thrash.

## 6. Audit, then commit

- Spawn one fresh reviewer (using `collaboration.spawn_agent`): give it each
  card's text, the rulings file and the implementation files — not your conclusions — and
  ask for every place behaviour differs from the text or rules. Fix every gap it confirms
  (route to owners if still alive, else fix yourself), rebuild, rerun tests. No "engine
  limitation" leftovers.
- Narrator: a new `Message` gets its line in `Marvel.Log.Narrator` and `log.json`.
- Client: anything new on the board (counters, a third card side, a new zone) goes in
  `Marvel.Game.Public`; check `frontend/` renders it.
- Commit in focused pieces (engine already done): the pack's cards, decks and tests; the
  rules doc and rulings. Message style: a descriptive sentence like `9a5c42ca2`. No
  Co-Authored-By or other AI trailer. Stage only this run's files.
- Card images are separate (`tools/import-images.py`, needs `MarvelHD.zip`); mention it,
  don't run it unasked.

Finish with a short report: cards done, mechanics added, rulings taken, test count, and
anything the user should look at (e.g. images, a ruling you recorded).
