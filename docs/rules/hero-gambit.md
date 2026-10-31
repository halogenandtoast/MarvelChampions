# Gambit Hero Pack (MC37)

Source: [official Gambit rules insert](https://images-cdn.fantasyflightgames.com/filer_public/1b/04/1b0478ce-e9a8-42f5-a7cd-3d0cc82139e7/mc37_gambit_rules_insert.pdf), p.1. Page 2 is blank.

## Rules insert

<!-- MC37 insert p.1 -->

### Piercing

An attack with piercing discards any tough status cards from the target of the attack before dealing damage.

### Ranged

An attack with ranged ignores the retaliate keyword.

### Team-Up

The team-up keyword names two characters. To include a card with the team-up keyword in your deck, your chosen identity must match one of the named characters. Additionally, a card with team-up cannot be played unless both of the named characters (identity or ally) are in play.

### Villainous

When a minion with villainous activates, give it a facedown boost card from the top of the encounter deck. When you resolve that minion’s activation, turn the boost card faceup and apply its boost icons to the minion’s stats for the activation. If the boost card has a boost ability, resolve its effects. Discard the boost card after the activation.

## Implementation references

The following are implementation summaries of RR v1.8, not additional rules from the insert.

- **Charge counters:** ordinary counters on the identity, retained when changing form (RR Counter, p.13). Charge de Card is limited once per round. Throw de Card is an interrupt to playing an Attack-trait event, distinct from an interrupt to attacking. Its “up to” cost requires at least one counter (RR Cost, p.13). Its bonus applies to every damage instance the event deals (Attack, p.10). Royal Flush is one attack with three damage instances; its counter is placed after the play interrupt.
- **Thief Extraordinaire:** an alter-ego thwart ability. The encounter cards are looked at, not revealed. The chosen card is discarded as a cost and only printed boost icons plus applicable boost-icon modifiers count; a boost star is not a boost icon (Boost, p.11; Look, p.26; Costs and Timing).
- **Operative Skill:** the interrupt applies to basic and labeled identity thwarts, including alter-ego thwarts. It does not apply to ally thwarts or to removal without a thwart label (Thwart, p.44).
- **Defense:** Natural Agility adds its charge counter before counting counters for its DEF bonus. Guild Armor requires Gambit to have defended and taken no damage (Defend, pp.14–15).
- **Psionic Shield:** use the errata in [faq-errata.md](faq-errata.md), RR v1.8 p.68. The minion remains in play and heals all its damage; it does not leave and re-enter play. This replacement applies to any departure, not merely lethal damage.
- **Overkill:** RR v1.8 p.31 makes excess damage conditional on the minion being defeated.

## Starter deck

The prebuilt Justice deck contains 40 cards: the 15 signature cards (37002–37010), Bishop, Dazzler, three Operative Skill, three Stealth Strike, three Breaking and Entering, two Passion for Justice, Professor X, X-Mansion, Beauty and the Thief, three Hit and Run, three Mutant Education, and one each Energy, Genius and Strength (37011–37024). Guild Business is the obligation. Belladonna's five-card nemesis set is separate; Exodus is a six-card modular set. War Room and X-Men Instruction are extra deck-building cards.

## Rulings (from the project owner, 2026-10-31)

The owner requested online research and a decision based on the latest FAQ and available rules, rather than choosing a timing interpretation directly.

### Researched timing decision

[FFG designer answer, Alex, July 20, 2023, question 53](https://hallofheroeslcg.com/latest-ffg-rulings-post-rrg-1-5/) establishes that Biomechanical Upgrades resolves before Psionic Shield: healing the minion and discarding the former leaves Shield attached. It also confirms that Shield does not cause a new engagement or Quickstrike. RR v1.8's Shield errata agrees that the minion stays in play. No superseding Shield timing answer was found in the post-v1.7/v1.8 ruling archive.

Implementation therefore opens the leave-play replacement after defeat interrupts, before actual departure or attachment cleanup. If Shield replaces that departure, after-defeat responses do not resolve and overkill deals no damage: this latter conclusion is an inference from RR v1.8 Overkill's explicit requirement that the attack defeat the minion, together with Replacement Effect's cancellation of the replaced occurrence. Two Shields consume only one attachment for one replaced departure.

### Other designer clarifications

[FFG's archived Gambit rulings](https://hallofheroeslcg.com/official-ffg-rulings/#gambit) clarify that Molecular Acceleration interrupts spending, and Rogue's constant discount updates during payment. For example, with one existing charge counter, Molecular Acceleration's two resources and its new counter suffice to pay Rogue's reduced cost of two. The interrupt must therefore be available before payment completes, including when checking whether Rogue can be played.

The same archive confirms that Operative Skill adds one to Giant Wasp's total divided thwart, that Royal Flush's new counter is too late for that event's Throw de Card, and that a confused Remy still pays all of Thief Extraordinaire's costs. He chooses a scheme after looking at and discarding the encounter card.

## Verification (2026-10-31)

- Gambit-focused behavior suite: 138 examples, no failures.
- Full non-smoke suite, including coverage: 4,993 examples, no failures.
- Focused smoke run: 165 cases (115 relevant plus 50 sampled), no failures. Four game seeds per case and the five-second new-question watchdog were retained. Replay sample: `SMOKE_SAMPLE_SEED=376827031`, with `SMOKE_HEROES=gambit SMOKE_MODULARS=ExodusSet`.
- A fresh independent audit checked the pack against card text, RR v1.8 and the designer rulings above; confirmed gaps were corrected and verified.

Card-image importing is separate and was not run.
