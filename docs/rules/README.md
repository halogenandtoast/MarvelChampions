# Marvel Champions Rules (engine reference)

Markdown transcription of the official *Marvel Champions: The Card Game* rules, for implementing the rules engine.

## Sources

| Document | Version | URL |
|---|---|---|
| Civil War rulebook (MC56) | Custom scenarios, competitive mode and FAQ | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc56_rulebook-web_1.pdf |
| Synthezoid Smackdown rulebook (MC57) | Scenario and competitive rules | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc57_rulebook-web.pdf |
| Fear No Evil rulebook (MC60) | Scenarios and campaign | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc60_rulebook-web.pdf |
| Shadowland rulebook (MC65) | Scenarios and campaign | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/10/MC65-Rulebook.pdf |
| Agents of S.H.I.E.L.D. rulebook (MC50) | 24 pages | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc50_rulebook-web.pdf |
| Black Panther (Shuri) rulesheet (MC51) | Rulesheet | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc51_rulesheet-web.pdf |
| Silk rulesheet (MC52) | Rulesheet | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc52_rulesheet-web.pdf |
| Falcon rulesheet (MC53) | Rulesheet | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc53_rulesheet-web.pdf |
| Winter Soldier rulesheet (MC54) | Rulesheet | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc54_rulesheet-web.pdf |
| Magneto Hero Pack rules insert (MC49) | 2 pages | https://images-cdn.fantasyflightgames.com/filer_public/84/33/8433f555-b1bc-4b23-a022-86120417b018/mc49_magneto_rules_insert.pdf |
| Nightcrawler Hero Pack rules insert (MC48) | 2 pages | https://images-cdn.fantasyflightgames.com/filer_public/e3/7b/e37bce25-fb98-4d80-91cd-64f13284972c/mc48_nightcrawler_rules_insert.pdf |
| Jubilee Hero Pack rules insert (MC47) | 2 pages | https://images-cdn.fantasyflightgames.com/filer_public/68/7e/687e6308-1160-42c6-bade-3469b6b53d11/mc47_jubilee_rules_insert-web.pdf |
| Iceman Hero Pack rules insert (MC46) | 2 pages | https://images-cdn.fantasyflightgames.com/filer_public/93/f8/93f8dca2-fed8-4e66-88c7-11d89539d1f0/mc46_iceman_rules_insert-web.pdf |
| Age of Apocalypse rulebook (MC45) | 24 pages | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc45_age_of_apocalypse_rulebook.pdf |
| Deadpool Hero Pack rules insert (MC44) | 2 rules pages and poster art | https://images-cdn.fantasyflightgames.com/filer_public/8a/98/8a987c42-a835-4fa7-b0b7-11f466e0faa4/mc44_deadpool_rules_insert-web.pdf |
| X-23 Hero Pack rules insert (MC43) | 2 pages (p.2 artwork) | https://images-cdn.fantasyflightgames.com/filer_public/32/e8/32e82755-7b86-4ba0-9201-aa32b52faaa8/mc43_x-23_rules_insert-web.pdf |
| Psylocke Hero Pack rules insert (MC41) | 2 pages (p.2 blank) | https://images-cdn.fantasyflightgames.com/filer_public/b3/41/b341e3d0-01ef-471d-b0d3-657ae05757c6/mc41_psylocke_rules_insert-web.pdf |
| MojoMania rules insert | 20 pages | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc39_mojomania_rules_insert-compressed.pdf |
| [scenario-mojo-mania.md](scenario-mojo-mania.md) | Standalone MaGog, Spiral and Mojo, genre sets, show deck, ratings and character threat |
| Rogue Hero Pack rules insert (MC38) | 2 pages (p.2 blank) | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc38_rogue_rules_insert.pdf |
| Gambit Hero Pack rules insert (MC37) | 2 pages (p.2 blank) | https://images-cdn.fantasyflightgames.com/filer_public/1b/04/1b0478ce-e9a8-42f5-a7cd-3d0cc82139e7/mc37_gambit_rules_insert.pdf |
| Storm Hero Pack rules insert (MC36) | 2 pages | https://images-cdn.fantasyflightgames.com/filer_public/93/59/9359dac0-92c6-4b91-b73e-1d53253601a0/mc36_storm_rules_insert.pdf |
| Phoenix Hero Pack rules insert (MC34) | 2 pages (p.2 blank) | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc34_phoenix_rules_insert.pdf |
| Wolverine Hero Pack rulesheet (MC35) | 1 page | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc35_wolverine_rules_insert.pdf |
| Cyclops Hero Pack rules insert (MC33) | 2 pages (p.2 blank) | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc33_cyclops_rules_insert.pdf |
| SP//dr Hero Pack rulesheet (MC31) | 2 pages (p.2 blank) | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc31_spdr_rulesheet.pdf |
| Spider-Ham Hero Pack rulesheet (MC30) | 2 pages | https://images-cdn.fantasyflightgames.com/filer_public/c0/cc/c0cc6521-0690-4a3e-8e74-384b9be95e2d/mc30_spider-ham_rulesheet.pdf |
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
| Drax Hero Pack rulesheet (MC19) | 1 page | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc19_drax_rulesheet.pdf |
| Star-Lord Hero Pack rulesheet (MC17) | 1 page | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc17_star-lord_rulesheet.pdf |
| The Galaxy's Most Wanted rulebook (MC16) | 24 pages (pp. 6, 7, 9, 11, 13, 16, 17 are images) | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc16_galaxys_most_wanted_rules_website-compressed.pdf |
| Venom Hero Pack rulesheet (MC20) | 2 pages | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc20_venom_rulesheet.pdf |
| Quicksilver Hero Pack rulesheet (MC14) | 2 pages | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc14_quicksilver_rulesheet.pdf |
| The Mad Titan's Shadow rulebook (MC21) | PDF created 2021-02-04, modified 2021-11-12, 28 pages (scenario pages with card art are partly images) | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc21_the_mad_titans_shadow_rulebook-compressed.pdf (campaign log: https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc21_the_mad_titans_shadow_rulebook-compressed-campaign_log.pdf) |
| Nebula Hero Pack rulesheet (MC22) | 2 pages | https://images-cdn.fantasyflightgames.com/filer_public/0b/0f/0b0f1475-6a14-4077-85e5-0353b097f94a/mc22_nebula_rulesheet.pdf |
| War Machine Hero Pack rulesheet (MC23) | 2 pages | https://images-cdn.fantasyflightgames.com/filer_public/53/e7/53e7fe04-5a82-4239-8ee9-9f65e6a6248d/mc23_war_machine_rulesheet.pdf |
| Valkyrie Hero Pack rulesheet (MC25) | rules insert | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc25_valkyrie_rulesheet.pdf |
| Vision Hero Pack rulesheet (MC26) | 2 pages | https://images-cdn.fantasyflightgames.com/filer_public/53/60/53601f73-fa92-4cb9-816c-4f8d904de281/mc26_vision_rulesheet.pdf |
| Sinister Motives rulebook (MC27) | 24 pages | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc27_sinister_motives_rules_v5-compressed.pdf |
| Nova Hero Pack rulesheet (MC28) | PDF dated 2021-02-04, 2 pages (p.2 blank) | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc28_nova_rulesheet.pdf |
| Ironheart Hero Pack rulesheet (MC29) | 2 pages (p.2 blank) | https://images-cdn.fantasyflightgames.com/filer_public/df/95/df950da6-c3b3-4edd-9f58-8d3bd192b755/mc29_ironheart_rulesheet.pdf |
| Mutant Genesis rulebook (MC32) | 24 pages; official campaign log fetched alongside it | https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc32_mutant_genesis_rulebook_v5-compressed.pdf (campaign log: https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc32_mutant_genesis_campaign_log.pdf) |
| MarvelCDB card data (scenario card text quoted in the product files) | public API, fetched 2026-10-22 | https://marvelcdb.com/api/public/cards/{gob,twc,trors,toafk,mts,sm}.json |

The FFG PDFs are all linked from https://www.fantasyflightgames.com/marvel-champions-the-card-game/ (and the v1.8 announcement
https://www.fantasyflightgames.com/en/news/2026/7/23/mission-updates/). The PDFs are not committed.

Precedence (RR "The Golden Rules"): card text / scenario rules > Rules Reference > Learn to Play.

## Files

Kree Fanatic: official [print-and-play encounter set](https://images-cdn.fantasyflightgames.com/filer_public/12/42/1242eccb-60ab-4a30-a099-a76f702cdbb5/ronan_modular_set.pdf).

| File | Contents |
|---|---|
| [hero-iceman.md](hero-iceman.md) | Iceman: Frostbite supply, activation timing, Sauron, published rulings and starter deck |
| [hero-jubilee.md](hero-jubilee.md) | Jubilee: distinct resources, Shopping Spree, Lost Child, Nanny, Arcade and starter deck |
| [hero-nightcrawler.md](hero-nightcrawler.md) | Nightcrawler: Bamf! defenses, Rogue base powers, Alliance, Crazy Gang and starter deck |
| [hero-magneto.md](hero-magneto.md) | Magneto: Magnetic Pull, current errata, Linked recruits, designer rulings and starter deck |
| [scenario-aoa-standard-iii.md](scenario-aoa-standard-iii.md) | Pursuit counters, nemesis Find, activation timing, and Drawing Near |
| [campaign-age-of-apocalypse.md](campaign-age-of-apocalypse.md) | Missions, Overseers, campaign rewards, expert campaign, and the Professor victory condition |
| [scenario-aoa-unus.md](scenario-aoa-unus.md) | Unus, Infinites, Dystopian Nightmare, and optional Gene Pool setup difficulty |
| [scenario-aoa-four-horsemen.md](scenario-aoa-four-horsemen.md) | Ordered Horsemen, active counter, zero hit points, and Hounds |
| [scenario-aoa-apocalypse.md](scenario-aoa-apocalypse.md) | Apocalypse's four stages, Prelates, Dark Riders, and side-scheme transformations |
| [scenario-aoa-dark-beast.md](scenario-aoa-dark-beast.md) | Dark Beast and the three Setting modular sets |
| [scenario-aoa-en-sabah-nur.md](scenario-aoa-en-sabah-nur.md) | Three-sided Apocalypse, Celestial Tech, and Clan Akkaba |
| [hero-bishop.md](hero-bishop.md) | Bishop: attack damage response, resource payment, Sidekick, Suit Up errata and starter deck |
| [hero-magik.md](hero-magik.md) | Magik: faceup deck, play permission, top-card timing and starter deck |
| [hero-deadpool.md](hero-deadpool.md) | Deadpool: fifth aspect, Dreadpool setup, printed icons and metagame boards, starter deck and pending digital inputs |
| [hero-x23.md](hero-x23.md) | X-23: Linked Specializations, Assault, damage costs, current FAQ and starter deck |
| [hero-psylocke.md](hero-psylocke.md) | Psylocke: double-sided weapons, distinct attack targets, printed stats, starter deck and official rulings |
| [hero-gambit.md](hero-gambit.md) | Gambit: charge counters, alter-ego thwarts, Psionic Shield errata and timing, starter deck |
| [hero-rogue.md](hero-rogue.md) | Rogue: Touched, copied traits, Karma, Med Lab, Reavers, current errata and rulings |
| [hero-storm.md](hero-storm.md) | Storm: Weather deck, Special abilities, Flash Freeze ruling, Possessed, starter deck and errata |
| [hero-phoenix.md](hero-phoenix.md) | Phoenix: Phoenix Force, controlled allies, keyword rules and FAQ, printed nemesis text, starter deck and owner rulings |
| [hero-wolverine.md](hero-wolverine.md) | Wolverine: damage-paid attacks, Claws timing, current errata, starter deck and owner rulings |
| [hero-cyclops.md](hero-cyclops.md) | Cyclops: Temporary, Field Commander, Optic Blast, printed starter deck and owner rulings |
| [hero-colossus.md](hero-colossus.md) | Colossus: tough status cards, discard responses, starter deck and current errata |
| [hero-shadowcat.md](hero-shadowcat.md) | Shadowcat: mass forms, defense timing, ignored restrictions, starter deck and recorded rulings |
| [scenario-mutant-genesis.md](scenario-mutant-genesis.md) | The five Mutant Genesis scenarios, modular sets, current Teamwork and scenario errata |
| [campaign-mutant-genesis.md](campaign-mutant-genesis.md) | Campaign roles, objectives, Future Past, rewards, expert campaign and owner rulings |
| [hero-spdr.md](hero-spdr.md) | SP//dr: separated identity, independent exhaustion, Sync Ratio, permanent, Suit errata, Noir FAQ and starter deck |
| [hero-spider-ham.md](hero-spider-ham.md) | Spider-Ham: toon counters, optional gestures, thwart timing, consequential damage, starter deck, Inheritors and recorded rulings |
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
| [scenario-kree-fanatic.md](scenario-kree-fanatic.md) | Print-and-play Ronan, per-player hit points, engagement timing and boost effects |
| [scenario-green-goblin.md](scenario-green-goblin.md) | Green Goblin pack: Risky Business (Norman Osborn / Green Goblin flipping, Criminal Enterprise / State of Madness, infamy and madness counters), Mutagen Formula (Goblin minion boost timing); scenario card text |
| [scenario-wrecking-crew.md](scenario-wrecking-crew.md) | Wrecking Crew pack, Breakout: four villains, the active villain, per-villain encounter decks and signature side schemes, setup, winning, expert/extreme difficulty; scenario card text |
| [campaign-rise-of-red-skull.md](campaign-rise-of-red-skull.md) | Rise of Red Skull: keywords as printed, campaign mode, campaign log, per-scenario setup/victory steps (Crossbones … Red Skull), Experimental Weapons deck, side-scheme deck, expert campaign (persistent damage, obligations in player decks); campaign card text (Hydra Campaign upgrades, Captive allies, expert obligations) |
| [scenario-kang.md](scenario-kang.md) | Once and Future Kang: setup, separate game areas (creating, playing in, joining), stage 2B, Kang stages, expert mode, modular sets, clarifications; scenario card text |
| [hero-ant-man.md](hero-ant-man.md) | Ant-Man pack: the three-sided identity card, Team-Up, form-change clarifications, rulings |
| [hero-angel.md](hero-angel.md) | Angel pack: three identity faces, printed acceleration icon, event timing, starter deck and published rulings |
| [hero-wasp.md](hero-wasp.md) | Wasp pack: the three-sided identity card, Team-Up, Piercing, divided thwart and attack, rulings |
| [hero-quicksilver.md](hero-quicksilver.md) | Quicksilver pack: Team-Up, Incite X, Piercing, the Super Speed FAQ, rulings (wild resources declared, exhausting as a target) |
| [hero-gamora.md](hero-gamora.md) | Gamora pack: the Skilled Tactician FAQ, starter deck, rulings |
| [hero-drax.md](hero-drax.md) | Drax pack: Piercing, starter deck, rulings (vengeance counters, Moondragon's minion attack) |
| [hero-star-lord.md](hero-star-lord.md) | Star-Lord pack: Hinder X, Piercing, Ranged, the Star-Lord, Mister Knife and Dive Bomb FAQ, Cosmo errata, rulings |
| [hero-venom.md](hero-venom.md) | Venom pack: Patrol, Ranged, the Enraged Symbiote nemesis-minion FAQ, Grasping Tendrils FAQ, starter deck, rulings |
| [hero-scarlet-witch.md](hero-scarlet-witch.md) | Scarlet Witch pack: Team-Up, counting boost icons, the Hex Bolt and Slipping Sanity FAQ, rulings |
| [scenario-galaxys-most-wanted.md](scenario-galaxys-most-wanted.md) | The Galaxy's Most Wanted: new keywords (Victory X, Patrol, Stalwart, Peril), the victory display, the amplify icon, the Ship Command, Power Stone and Badoon Headhunter sets, FAQ |
| [scenario-brotherhood-of-badoon.md](scenario-brotherhood-of-badoon.md) | GMW #1 Brotherhood of Badoon: Drang, Charge Up and the Badoon Ship, Band of Badoon; card text, readings, campaign instructions |
| [scenario-infiltrate-the-museum.md](scenario-infiltrate-the-museum.md) | GMW #2 Infiltrate the Museum: Collector, The Collection, Galactic Artifacts, Menagerie Medley; card text, readings, campaign instructions |
| [scenario-escape-the-museum.md](scenario-escape-the-museum.md) | GMW #3 Escape the Museum: Collector A1/B1, infinite hit points, the set-aside Milano; card text, readings, campaign instructions |
| [scenario-nebula.md](scenario-nebula.md) | GMW #4 Nebula: Techniques, Nebula's Ship and evasion counters, Space Pirates; card text, readings, campaign instructions |
| [scenario-ronan-the-accuser.md](scenario-ronan-the-accuser.md) | GMW #5 Ronan the Accuser: the Power Stone in play, Kree Militants; card text, readings, campaign instructions |
| [campaign-galaxys-most-wanted.md](campaign-galaxys-most-wanted.md) | The Galaxy's Most Wanted campaign: campaign log, units and The Market, expert campaign, each scenario's setup/victory steps, Market and Challenge card text, rulings |
| [hero-groot.md](hero-groot.md) | Groot (GMW): growth counters, Flora Colossus FAQ, Team-Up, starter deck, rulings |
| [scenario-ebony-maw.md](scenario-ebony-maw.md) | MTS #1 Ebony Maw: Spell environments and invocation counters, Black Order and Armies of Titan; the box's keyword page and amplify icon; card text, readings |
| [scenario-tower-defense.md](scenario-tower-defense.md) | MTS #2 Tower Defense: two main schemes, two villains and Focused Defense, Avengers Tower, modular difficulty; card text, readings |
| [scenario-thanos.md](scenario-thanos.md) | MTS #3 Thanos: the Infinity Gauntlet and Infinity Stone deck, Children of Thanos; card text, readings |
| [scenario-hela.md](scenario-hela.md) | MTS #4 Hela: Mystic and Wounded sides, Odin captive, the Hel side-scheme chain, Legions of Hel, Frost Giants; card text, readings |
| [scenario-loki.md](scenario-loki.md) | MTS #5 Loki: five Lokis, advancing and swapping at random, the victory condition, Enchantress; card text, readings |
| [campaign-mad-titans-shadow.md](campaign-mad-titans-shadow.md) | The Mad Titan's Shadow campaign: campaign log and pool, each scenario's setup/victory steps, expert campaign, campaign card text, rulings |
| [hero-spectrum.md](hero-spectrum.md) | Spectrum (MTS): additional forms and energy form upgrades, starter deck, Leadership and basic cards, rulings |
| [hero-adam-warlock.md](hero-adam-warlock.md) | Adam Warlock (MTS): Avatar of Life deck-building, Cosmic Entity events, starter deck, rulings |
| [hero-rocket-raccoon.md](hero-rocket-raccoon.md) | Rocket Raccoon (GMW): charge counters FAQ, excess damage, starter deck, rulings |
| [hero-nebula.md](hero-nebula.md) | Nebula: Techniques, printed Lethal Weapon stats, current errata and starter deck |
| [hero-war-machine.md](hero-war-machine.md) | War Machine: Alliance, ammo, current errata and starter deck |
| [hero-valkyrie.md](hero-valkyrie.md) | Valkyrie: Alliance, Hinder, Death-Glow, current errata and starter deck |
| [hero-vision.md](hero-vision.md) | Vision: additional mass forms, Alliance, piercing, stalwart and Machine Man errata |
| [encounter-the-hood-modulars.md](encounter-the-hood-modulars.md) | The Hood’s nine modular sets and optional Standard II / Expert II sets |
| [scenario-the-hood.md](scenario-the-hood.md) | The Hood: recruitment, Foul Play, steady; implementation notes and sources |
| [campaign-sinister-motives.md](campaign-sinister-motives.md) | Sinister Motives campaign, reputation, rewards and campaign cards |
| [scenario-sinister-motives.md](scenario-sinister-motives.md) | Sinister Motives scenarios, modular sets and recorded rulings |
| [hero-ghost-spider.md](hero-ghost-spider.md) | Ghost-Spider: Dizzying Reflexes, George Stacy and starter deck |
| [hero-miles-morales.md](hero-miles-morales.md) | Miles Morales: Specials, Requirement and starter deck |
| [hero-nova.md](hero-nova.md) | Nova: stacked tough, Requirement, wild resources, Ms. Marvel errata, starter deck and Armadillo modular set |
| [hero-ironheart.md](hero-ironheart.md) | Ironheart: progressing identity cards, Requirement, printed-card corrections, Go for Champions errata and FAQ, starter deck, Zzzax modular set and pending rulings |
| [hero-cable.md](hero-cable.md) | Cable: player side schemes, victory display, Temporal Leap, starter deck and official rulings |
| [hero-domino.md](hero-domino.md) | Domino: deck swaps, discard responses, printed resources, starter deck and official rulings |
| [campaign-next-evolution.md](campaign-next-evolution.md) | NeXt Evolution: five scenarios, modular sets, campaign setup/rewards and recorded rulings |

## Conventions

- Product files (`scenario-*.md`, `campaign-*.md`) contain insert/rulebook transcriptions with
  `<!-- GOB insert p.N -->`-style markers or explicitly marked implementation summaries with source-page links.
  Quoted card text comes from MarvelCDB and is labelled as such.

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

## Agents of S.H.I.E.L.D. and hero packs 51–54

- [hero-maria-hill.md](hero-maria-hill.md)
- [hero-nick-fury.md](hero-nick-fury.md)
- [campaign-agents-of-shield.md](campaign-agents-of-shield.md)
- [scenario-shield-agents.md](scenario-shield-agents.md)
- [hero-black-panther-shuri.md](hero-black-panther-shuri.md)
- [hero-silk.md](hero-silk.md)
- [hero-falcon.md](hero-falcon.md)
- [hero-winter-soldier.md](hero-winter-soldier.md)

- [Trickster Takeover](scenario-trickster-takeover.md): Enchantress, God of Lies
  (Single Group Mode), Trickster Magic; [FFG MC55 rulebook](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc55_rulebook-web.pdf).

## Civil War scenarios

- [Completion audit](civil-war-completion-audit.md): requirement-by-requirement evidence and final verification results.
- [Scenario construction](scenario-civil-war.md): cooperative and competitive setup, independent decks, physical inventories, rewards and save/reload.
- [Engine boundaries](encounter-civil-war-engine.md): team phases, victory conditions, shared mechanics and reward-card behavior.
- [Leader rules](encounter-civil-war-leaders.md): leader stages, signature cards and main-scheme interactions.

## New hero and player-card rules

| Product | Rules |
|---|---|
| Wonder Man | [Rules and rulings](hero-wonder-man.md) |
| Hercules | [Rules and rulings](hero-hercules.md) |
| Jessica Jones | [Rules and rulings](hero-jessica-jones.md) |
| Luke Cage | [Rules and rulings](hero-luke-cage.md) |
| Civil War player cards | [Rules and scope](player-civil-war.md) |
| Fear No Evil player cards | [Rules and scope](player-fear-no-evil.md) |

- [Civil War and Fear No Evil nemeses](encounter-civil-war-fear-no-evil-nemeses.md): Tigra, Hulkling, Daredevil and Echo obligations and nemesis interactions.
