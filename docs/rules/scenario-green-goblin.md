# The Green Goblin Scenario Pack: Risky Business and Mutagen Formula

> Source: *Marvel Champions: The Card Game* — *The Green Goblin* Scenario Pack (MC02) rules insert (FFG, 2019), transcribed
> from the official PDF: https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc02_green_goblin_rules_insert.pdf
> (8 pages, PDF dated 2019-05-17). Insert text is verbatim except for formatting; icons are written as in the other
> files (`[star]`, `[per player]`, …). `<!-- GOB insert p.N -->` gives the page.
>
> The insert has no setup section of its own: setup, the environments and the infamy/madness counters are defined by
> the cards. That card text is quoted in "Scenario cards" below from MarvelCDB
> (https://marvelcdb.com/api/public/cards/gob.json), **not** from the insert. Card codes are MarvelCDB codes.

Related RR text already in this folder: the FAQ entries for Norman Osborn (#1A), Green Goblin (#1B) and I See You (#30)
are in [faq-errata.md](faq-errata.md) ("Green Goblin Scenario Pack"); Environment, Flip, Indirect Damage and Double-Sided
Card are glossary entries.

## Summary (non-normative)

- **Risky Business**: the villain cards are double-sided, Norman Osborn (A) / Green Goblin (B), and the Criminal
  Enterprise / State of Madness environment is double-sided too. Setup (1A) puts Criminal Enterprise into play with
  2[per player] infamy counters. Norman cannot be damaged (damage removes infamy instead) and does not attack (he adds
  infamy instead). When the last infamy counter is removed, flip Norman and the environment: Green Goblin with State of
  Madness, which enters with 2[per player] madness counters; Goblin does not scheme (he removes madness instead).
  When the last madness counter is removed, flip back. Flipping keeps everything on the villain, and Green Goblin's
  "When Revealed" triggers on the flip. A defeated stage's successor enters on the same side.
- **Mutagen Formula**: a normal single-sided villain. Setup engages a Goblin Thrall with each player; Goblin Soldier /
  Goblin Thrall boost abilities put them into play engaged, and if that happens during villain-phase step 2 they
  activate.

## How to Use This Pack
<!-- GOB insert p.2 -->

To play a game with a scenario in this pack, choose which scenario—either Risky Business or Mutagen Formula—you would
like to play. The “Contents” section on stage 1A of each main scheme will instruct you on which villain and encounter
sets to use for the scenario. Then, follow the standard setup rules (see Rules Reference Appendix).

### Modular Encounter Sets

In addition to the Risky Business and Mutagen Formula scenarios, the Green Goblin Scenario Pack comes with four modular
encounter sets— A Mess of Things, Goblin Gimmicks, Power Drain, and Running Interference.

To add variety and unpredictability to future replays, a number of modular encounter sets can be added to (and/or
removed from) a scenario. When adding a modular set, players may either choose the set they would like to play against,
or (to increase unpredictability) they can select one at random from all available modular sets and shuffle the set into
the encounter deck without looking at it.

### This Document

For each scenario, this document contains story elements to set the stage, rules for new situations that come up in the
scenario, and strategy tips. However, this document is not required to play either scenario.

## Risky Business

### New Rules
<!-- GOB insert p.4 -->

#### When the Villain Changes Form

After the villain changes form, all attachment cards, status cards, boost cards, damage, and other game elements
associated with the villain remain as they are.

Changing form will trigger Green Goblin’s “When Revealed” ability.

#### When a Villain Stage is Defeated

After a villain stage is defeated, the next stage of the villain deck enters play on the same side as the just defeated
stage.

Note: In most situations, the next stage will enter play on the Green Goblin side, as the Green Goblin side will be the
one that was defeated.

#### When Norman Osborn Attacks

Norman Osborn (I)’s ability reads “**Forced Interrupt**: When Norman Osborn would attack, place 1 infamy counter on
Criminal Enterprise instead.”

Since Norman Osborn’s “**Forced Interrupt**“ ability places a counter instead of attacking, cards that trigger when the
villain attacks do not resolve because no attack activation was performed. Additionally, Norman Osborn does not have an
attack power value and, as such, his attack power cannot be increased, decreased, or modified.

#### When Green Goblin Schemes
<!-- GOB insert p.5 -->

Green Goblin (I)’s ability reads “**Forced Interrupt**: When Green Goblin would scheme, remove 1 madness counter from
State of Madness instead.”

Since Green Goblin’s “**Forced Interrupt**“ ability removes a counter instead of scheming, cards that trigger when the
villain schemes do not resolve because no scheme activation was performed. Additionally, Green Goblin does not have a
scheme power value and, as such, his scheme power cannot be increased, decreased, or modified.

#### When Revealed (Green Goblin) / When Revealed (Norman Osborn)

If an ability reads “When Revealed (Green Goblin)”, that ability triggers only if the Green Goblin side of the villain is
in play. If an ability reads “When Revealed (Norman Osborn)”, that ability triggers only if the Norman Osborn side of the
villain is in play.

### Strategy Tips
<!-- GOB insert p.5 -->

It’s advantageous to keep the number of infamy counters on Criminal Enterprise as low as possible, as there are multiple
cards in the encounter deck that increase in power relative to the number of infamy counters in play.

Additionally, flipping the villain is often only best to do when significant damage can be dealt to him in the same
round.

## Mutagen Formula

### Rules Clarification
<!-- GOB insert p.7 -->

#### Goblin Minions Activiation Timing

Goblin Soldier and Goblin Thrall minions have a boost ability that reads “**Boost**: Put [this minion] into play engaged
with you.”

If a Goblin Soldier or Goblin Thrall’s boost ability triggers during step 2 of the villain phase, that minion will
activate against the engaged player. This occurs because that minion is active by the time the minion activation point
of step 2 resolves.

### Strategy Tips
<!-- GOB insert p.7 -->

It’s usually best to defeat Goblin minions as quickly as possible because many of Green Goblin’s abilities get stronger
while there are more Goblin minions in play.

Additionally, Goblin Soldier and Goblin Thrall minions have a “Boost” ability that sometimes allows them to activate
before players have a chance to defeat them, meaning these minions can make surprise scheme or attack activations.

## Scenario cards (MarvelCDB card text, not the insert)

Setup and the scenario mechanics live on these cards. Text as MarvelCDB gives it (its typos kept, e.g. “enter play”,
“2 infamy counter”). For a main scheme, "A"/"B" is the side. Stats (hit points, threat) are in
`frontend/public/data/cards.json` / MarvelCDB and are not repeated here.

### Risky Business

- **Hostile Takeover** 1A — Main Scheme (02004): **Contents**: Norman Osborn (I) and Norman Osborn (II). *(Norman Osborn (II) and Norman Osborn (III) instead for expert mode.)* Risky Business and Standard encounter sets. One modular encounter set *(recommended: Goblin Gimmicks)*.<br>**Setup**: Put the Criminal Enterprise environment into play. Shuffle the encounter deck. Advance to stage 1B.
- **Hostile Takeover** 1B — Main Scheme (02004): **When Completed**: Place 1[per player] infamy counters on Criminal Enterprise. Then discard 1 card from each player's deck for each infamy counter on Criminal Enterprise.
- **Corporate Acquisition** 2A — Main Scheme (02005): **When Revealed**: Advance to stage 2B.
- **Corporate Acquisition** 2B — Main Scheme (02005): **If this stage is completed, the players lose the game.**
- **Criminal Enterprise** — Environment (02006a): Criminal Enterprise enter play with 2[per player] infamy counters on it. If there are no infamy counters here, flip Norman Osborn and Criminal Enterprise.
- **State of Madness** — Environment (02006b, back of Criminal Enterprise): State of Madness enter play with 2[per player] madness counters on it. If there are no madness counters here, flip Green Goblin and State of Madness.
- **Norman Osborn** I — Villain (02001a): [star] **Forced Interrupt**: When Norman Osborn would attack, place 1 infamy counter on Criminal Enterprise instead.<br>**Forced Interrupt**: When Norman Osborn would take any amount of damage, remove that many infamy counters from Criminal Enterprise instead.
- **Green Goblin** I — Villain (02001b, back of Norman Osborn I): **When Revealed**: Deal 3 indirect damage to each player in hero form.<br>[star] **Forced Interrupt**: When Green Goblin would scheme, remove 1 madness counter from State of Madness instead.
- **Norman Osborn** II — Villain (02002a): [star] **Forced Interrupt**: When Norman Osborn would attack, place 2 infamy counter on Criminal Enterprise instead.<br>**Forced Interrupt**: When Norman Osborn would take any amount of damage, remove that many infamy counters from Criminal Enterprise instead.
- **Green Goblin** II — Villain (02002b): **When Revealed**: Deal 3 indirect damage to each player.<br>[star] **Forced Interrupt**: When Green Goblin would scheme, remove 1 madness counter from State of Madness instead.
- **Norman Osborn** III — Villain (02003a): [star] **Forced Interrupt**: When Norman Osborn would attack, place 3 infamy counter on Criminal Enterprise instead.<br>**Forced Interrupt**: When Norman Osborn would take any amount of damage, remove that many infamy counters from Criminal Enterprise instead.
- **Green Goblin** III — Villain (02003b): **When Revealed**: Deal 4 damage to each player.<br>[star] **Forced Interrupt**: When Green Goblin would scheme, remove 2 madness counters from State of Madness instead.

### Mutagen Formula

- **Unleashing the Mutagen** 1A — Main Scheme (02017): **Contents**: Green Goblin (I) and Green Goblin (II). *(Green Goblin (II) and Green Goblin (III) instead for expert mode.)* Mutagen Formula and Standard encounter sets. One modular encounter set *(recommended: Goblin Gimmicks)*.<br>**Setup**: Put a Goblin Thrall minion into play engaged with each player. Shuffle the encounter deck. Advance to stage 1B.
- **Unleashing the Mutagen** 1B — Main Scheme (02017): **When Completed**: In player order, each player not engaged with a **Goblin** minion must discard 3 cards from the encounter deck and put the first **Goblin** minion they discarded this way into play engaged with them.
- **Mutagen Cloud** 2A — Main Scheme (02018): **When Revealed**: Advance to stage 2B.
- **Mutagen Cloud** 2B — Main Scheme (02018): X is equal to the number of **Goblin** enemies (including Green Goblin) in play.<br>**If this stage is completed, the players lose the game.**- **Green Goblin** I — Villain (02014): [star] **Forced Response**: After Green Goblin attacks and damages you, place 1 threat on the main scheme.
- **Green Goblin** II — Villain (02015): **When Revealed**: Deal 2 encounter cards to each player.<br>[star] **Forced Response**: After Green Goblin attacks and damages you, place 1 threat on the main scheme.
- **Green Goblin** III — Villain (02016): **When Revealed**: Deal 3 encounter cards to each player.<br>[star] **Forced Response**: After Green Goblin attacks and damages you, place 2 threat on the main scheme.

Not in the insert: the S.H.I.E.L.D. briefings (pp.3, 6; flavour only) and credits (p.8) are omitted.
