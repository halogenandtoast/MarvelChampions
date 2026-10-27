# Marvel Champions Rules (engine reference)

Markdown transcription of the official *Marvel Champions: The Card Game* rules, for implementing the rules engine.

## Sources

| Document | Version | URL |
|---|---|---|
| Rules Reference | **v1.8** (PDF created 2026-05-26, modified 2026-07-23; announced in FFG's "Mission Updates", 2026-07-23), 71 pages | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc_rulesreference_v18_compressed.pdf (byte-identical copy at `.../uploads/2026/08/mc_rulesreference_v18_compressed-1.pdf`) |
| Core Set Learn to Play | 2019 print edition (PDF dated 2019-10-26), 24 pages | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mvc01_learn_to_play_eng-compressed.pdf |
| The Green Goblin rules insert (MC02) | PDF dated 2019-05-17, 8 pages | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc02_green_goblin_rules_insert.pdf |
| The Wrecking Crew rules insert (MC03) | PDF created 2019-06-11, modified 2023-02-02, 8 pages | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc03_wrecking_crew_rules_insert-compressed.pdf |
| The Rise of Red Skull rulebook (MC10) | PDF dated 2020-09-23, 20 pages | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc10_the_rise_of_red_skull_rules_web.pdf (campaign log: https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc10_the_rise_of_red_skull_campaign-log.pdf) |
| The Once and Future Kang rules insert (MC11) | PDF created 2020-07-30, modified 2023-02-02, 8 pages | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc11_once_and_future_kang_rules_insert-compressed.pdf |
| Ant-Man Hero Pack rulesheet (MC12) | 1 page | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc12_ant-man_rulesheet.pdf |
| Scarlet Witch Hero Pack rulesheet (MC15) | 1 page | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc15_scarlet_witch_rulesheet.pdf |
| Wasp Hero Pack rulesheet (MC13) | 1 page | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc13_wasp_rulesheet.pdf |
| Gamora Hero Pack rulesheet (MC18) | 1 page | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc18_gamora_rulesheet.pdf |
| Star-Lord Hero Pack rulesheet (MC17) | 1 page | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc17_star-lord_rulesheet.pdf |
| Quicksilver Hero Pack rulesheet (MC14) | 2 pages | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc14_quicksilver_rulesheet.pdf |
| MarvelCDB card data (scenario card text quoted in the product files) | public API, fetched 2026-10-22 | https://marvelcdb.com/api/public/cards/{gob,twc,trors,toafk}.json |

The FFG PDFs are all linked from https://www.fantasyflightgames.com/marvel-champions-the-card-game/ (and the v1.8 announcement
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
| [scenario-green-goblin.md](scenario-green-goblin.md) | Green Goblin pack: Risky Business (Norman Osborn / Green Goblin flipping, Criminal Enterprise / State of Madness, infamy and madness counters), Mutagen Formula (Goblin minion boost timing); scenario card text |
| [scenario-wrecking-crew.md](scenario-wrecking-crew.md) | Wrecking Crew pack, Breakout: four villains, the active villain, per-villain encounter decks and signature side schemes, setup, winning, expert/extreme difficulty; scenario card text |
| [campaign-rise-of-red-skull.md](campaign-rise-of-red-skull.md) | Rise of Red Skull: keywords as printed, campaign mode, campaign log, per-scenario setup/victory steps (Crossbones … Red Skull), Experimental Weapons deck, side-scheme deck, expert campaign (persistent damage, obligations in player decks); campaign card text (Hydra Campaign upgrades, Captive allies, expert obligations) |
| [scenario-kang.md](scenario-kang.md) | Once and Future Kang: setup, separate game areas (creating, playing in, joining), stage 2B, Kang stages, expert mode, modular sets, clarifications; scenario card text |
| [hero-ant-man.md](hero-ant-man.md) | Ant-Man pack: the three-sided identity card, Team-Up, form-change clarifications, rulings |
| [hero-wasp.md](hero-wasp.md) | Wasp pack: the three-sided identity card, Team-Up, Piercing, divided thwart and attack, rulings |
| [hero-quicksilver.md](hero-quicksilver.md) | Quicksilver pack: Team-Up, Incite X, Piercing, the Super Speed FAQ, rulings (wild resources declared, exhausting as a target) |
| [hero-gamora.md](hero-gamora.md) | Gamora pack: the Skilled Tactician FAQ, starter deck, rulings |
| [hero-star-lord.md](hero-star-lord.md) | Star-Lord pack: Hinder X, Piercing, Ranged, the Star-Lord, Mister Knife and Dive Bomb FAQ, Cosmo errata, rulings |
| [hero-scarlet-witch.md](hero-scarlet-witch.md) | Scarlet Witch pack: Team-Up, counting boost icons, the Hex Bolt and Slipping Sanity FAQ, rulings |

## Conventions

- Product files (`scenario-*.md`, `campaign-*.md`) transcribe the product's insert/rulebook verbatim with
  `<!-- GOB insert p.N -->`-style markers; the card text they quote comes from MarvelCDB and is labelled as such.

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
