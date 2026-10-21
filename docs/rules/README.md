# Marvel Champions Rules (engine reference)

Markdown transcription of the official *Marvel Champions: The Card Game* rules, for implementing the rules engine.

## Sources

| Document | Version | URL |
|---|---|---|
| Rules Reference | **v1.8** (PDF created 2026-05-26, modified 2026-07-23; announced in FFG's "Mission Updates", 2026-07-23), 71 pages | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc_rulesreference_v18_compressed.pdf (byte-identical copy at `.../uploads/2026/08/mc_rulesreference_v18_compressed-1.pdf`) |
| Core Set Learn to Play | 2019 print edition (PDF dated 2019-10-26), 24 pages | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mvc01_learn_to_play_eng-compressed.pdf |

Both are linked from https://www.fantasyflightgames.com/marvel-champions-the-card-game/ (and the v1.8 announcement
https://www.fantasyflightgames.com/en/news/2026/7/23/mission-updates/). The PDFs are not committed.

Precedence (RR "The Golden Rules"): card text / scenario rules > Rules Reference > Learn to Play.

## Files

| File | Contents |
|---|---|
| [glossary-a-c.md](glossary-a-c.md) | RR glossary entries A–C (Ability … Crisis Icon) |
| [glossary-d-h.md](glossary-d-h.md) | RR glossary entries D–H (Damage … Hit Points) |
| [glossary-i-o.md](glossary-i-o.md) | RR glossary entries I–O (Icons … Ownership and Control) |
| [glossary-p-r.md](glossary-p-r.md) | RR glossary entries P–R (Patrol … Running Out of Cards) |
| [glossary-s-z.md](glossary-s-z.md) | RR glossary entries S–Z (Scenario-Specific Card … You, Your) |
| [round-sequence.md](round-sequence.md) | Round overview, setup (Appendix II), player phase, villain phase, enemy attack/scheme steps, boosts, defending, deal/reveal, damage/defeat/winning |
| [timing.md](timing.md) | Golden/Grim rules, ability types (Action, Interrupt, Response, Forced, Resource, Special, Setup, When Revealed/Defeated/Completed, Hero/Alter-Ego), windows, simultaneous timing priority, initiating abilities, costs, targets, replacement/cancel, wording ("then", "and", "would", …) |
| [appendices.md](appendices.md) | RR Overview, Appendix I (deck customization), III (card anatomy), VI (game environments) |
| [faq-errata.md](faq-errata.md) | RR Appendix IV (FAQ) and V (errata), by product and card number |
| [learn-to-play.md](learn-to-play.md) | Learn to Play specifics not in the RR: components, anatomy positions, tutorial setup, nemesis sets, starter decks, core scenarios (Rhino/Klaw/Ultron) with standard/expert/modular sets, LTP vs RR differences |

## Conventions

- Each glossary entry is `## Term` (title-cased from the RR's all-caps headings), followed by
  `<!-- RR v1.8 p.N -->` with its PDF page. Search with e.g. `grep -n '^## Surge' docs/rules/*.md`.
- `round-sequence.md` and `timing.md` hold **verbatim copies** of the relevant glossary entries (as `### Term`),
  grouped by topic. The glossary files are the canonical copy. Each also starts with a short summary marked
  **non-normative**; quote the entries, not the summary, when you need the exact rule.
- Text is verbatim from the PDF; formatting follows the source: bold timing triggers (`**Forced Response**`),
  italic examples and reminder text, `See also` cross-references kept as written. Source typos are kept
  (e.g. "Accleration", "resouces", "**not** is considered").
- Icons are written as `[energy]`, `[mental]`, `[physical]`, `[wild]`, `[boost]`, `[star]`, `[per player]`,
  `[acceleration]`, `[amplify]`, `[crisis]`, `[hazard]`, `[consequential damage]`, `[unique]`; the cost arrow is `→`.
- The RR marks changes since v1.7 in red. That colouring is not kept. The RR's own summary of notable changes (p.1):
  keywords, encounter icons and status cards are now defined in game language; revised Simultaneous Timing Priority
  chart (p.5); added a step to Attack (Enemy Activation) (p.8, "Calculate damage"); revised Overkill (p.31); added
  Resolve (p.37); revised Surge (p.42); revised player deck customization (p.50); FAQ (pp.57–62) and errata
  (pp.65–69) additions.
- Not transcribed: the RR index (pp.2–3; it only maps terms to pages, and "See" redirects are already glossary
  entries), card illustrations, and the anatomy pages' card images (the anatomy keys and callouts are in
  `appendices.md`).
