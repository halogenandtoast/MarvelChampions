# Storm Hero Pack

Sources: [official MC36 rules insert](https://images-cdn.fantasyflightgames.com/filer_public/93/59/9359dac0-92c6-4b91-b73e-1d53253601a0/mc36_storm_rules_insert.pdf), p.1 (p.2 blank); the [printed starter deck list](https://hallofheroeslcg.com/wp-content/uploads/2022/11/zzz.jpg); Rules Reference v1.8 in this directory. This is an implementation summary; the glossary entries are canonical.

## Insert rules
<!-- MC36 insert p.1; implementation summary -->

Storm has a separate four-card Weather deck. Shuffle Clear Skies, Hurricane, Thunderstorm and Blizzard together and place that deck facedown beside her identity. During setup, choose one Weather support and put it into play. Its constant ability affects friendly and enemy characters alike. Weather Control and Weather Goddess exchange that support with a different support from the Weather deck despite permanent.

The insert also explains amplify, permanent, piercing and stalwart. Amplify adds boost icons when boost cards turn faceup during enemy activations. Permanent prevents defeat, departure and text-box blanking by cards outside the permanent card's set. Piercing attacks discard the target's tough statuses before damage. Stalwart prevents stun and confusion. Apply current RR v1.8 definitions from [A–C](glossary-a-c.md), [P–R](glossary-p-r.md), and [S–Z](glossary-s-z.md).

There is no insert FAQ.

## Rules used by the implementation

- **Weather swapping:** RR “Swap” (`glossary-s-z.md`, p.42). Because Weather supports have different titles, a swap leaves and enters play; attachments, counters and exhaustion do not transfer. The incoming Weather support enters ready. The old support goes into the Weather deck, never the player's discard pile. The hero-set exception to permanent permits the swap. The Weather deck is separate from the player deck and does not count toward deck size.
- **Weather Specials:** RR Special and Triggered Ability. A Special is resolved only when another effect instructs it. It is a separate ability; the preceding attack/thwart label does not carry into it. Use the support as the effect's source. Complete the Special before opening Storm's Cape's response window.
- **Global modifiers:** “Each character” includes every hero, alter-ego, ally, minion and villain, not merely the Storm player's cards. Stalwart immediately removes existing stunned/confused statuses and prevents new ones. Retaliate instances add together and only deal damage after the character survives the attack. Blizzard preserves traits while blanking the chosen non-Elite minion through the end of the round, even after Blizzard is swapped out.
- **Torrential Rain:** “Among” permits distributing three removed threat over legal schemes. It is one thwart ability; apply normal patrol/crisis restrictions. Its Hurricane Special is a separate removal ability and is not a thwart.
- **Flash Freeze:** Play at the initiation of a villain attack against its controller, before choosing defenders. Resolving a defense event makes that identity the defender. The phase-lasting effect updates as the game state changes, including minions entering play or becoming engaged later (RR Lasting Effects). Apply the reduction only while the affected enemy attacks Storm herself, per the owner ruling below.
- **Havok:** Only printed boost icons on the discarded card count. A boost star is not a boost icon; amplify only affects a card turned faceup as a boost during an enemy activation. Apply the extra ATK only for that attack, and the extra consequential damage only for that attack's consequential damage step.
- **Mirage:** Compare current enemy SCH against current Mirage THW. The comparison is strict. Stun immunity and stalwart still apply.
- **Leadership Skill:** An ally's basic attack/thwart opens the interrupt, including another player's ally. Three leadership counters are uses, so discard the upgrade after spending the last. Its +1 THW/+1 ATK lasts for that action only.
- **To Me, My X-Men!:** Search only the top five player-deck cards; put the chosen X-Men ally into play rather than play it from hand. Shuffle the searched deck. Track that instance until the end of the phase; returning to hand is conditional on it still being in play. A copy that left and re-entered play is a new instance.
- **Uncanny X-Men:** Check every controlled character, including the identity in its current form. A Mutant alter-ego without X-Men disables the discount. The hit-point bonus applies independently of the discount. Enforce one Team support per player.
- **Hangar Bay:** The ally must survive the defended attack; another player's ally is eligible. Use the completed defense window, not the declaration of defense.
- **Possessed:** Treat the ally as a minion, preserve its damage and statuses, blank its text except traits, use printed THW as SCH and printed ATK/hit points, and give it Controlled. It engages its former controller. Removing Possessed restores a surviving ally under its owner's control; if the host was defeated it goes to its owner's discard. See [Ally/Minion](glossary-a-c.md), [Ownership and Control](glossary-i-o.md), and existing Beguiled support. Select the lowest current THW among eligible allies; the revealing player breaks a tie. Minions do not take consequential damage.
- **Knife Fight:** Choose an enemy with the highest current ATK; the player breaks ties. Take non-attack damage equal to that enemy's ATK, then deal non-attack damage equal to the hero's current ATK. These effects are not attacks and do not cause retaliate. The chosen enemy is the target of the cost before the arrow. Do not pick a lower-ATK enemy because the highest cannot take damage.

## Current errata
<!-- RR v1.8 p.68 -->

Claustrophobia (#30) forces alter-ego form and prevents changing to **hero** form. It does not prohibit all form changes.

Possessed (#38) explicitly makes the attached ally engage its controller when it becomes a minion.

See [faq-errata.md](faq-errata.md), “Storm Hero Pack”. Local MarvelCDB already contains both corrections.

## Official designer clarifications

[Storm answers collected by Hall of Heroes](https://hallofheroeslcg.com/official-ffg-rulings/#storm):

- A Weather Special remains a support ability, not an action performed by Storm. The designer explicitly corrected an earlier contrary answer.
- Blizzard needs a non-Elite minion to choose. No eligible target means no Special resolves and no Cape response. An already blank eligible minion still permits resolution and Cape.
- Flash Freeze cannot be played after Storm starts defending another player's attack: the trigger belonged to the originally attacked player.
- Hurricane's Special is not a thwart and Thunderstorm's is not an attack; neither inherits the label of the event that called it.

The [August 14, 2025 answer](https://hallofheroeslcg.com/latest-ffg-rulings-post-rrg-1-6/) says Flash Freeze's reduction applies only when Storm herself is attacked, not when an ally defends. This contradicts the [2024 answer](https://hallofheroeslcg.com/latest-ffg-rulings-post-rrg-1-5/) allowing Storm-controlled allies to receive the reduction against later minion attacks. The project owner confirmed the later 2025 answer below.

## Starter deck

The printed list has a 40-card player deck, plus the four Weather supports outside it:

- Hero (15): Storm's Crown; Storm's Cape; Ororo's Garden; Weather Goddess (3); Torrential Rain (3); Lightning Bolt (2); Flash Freeze (2); Blast of Wind (2).
- Leadership (15): Havok; Mirage; Gentle; Pixie; Uncanny X-Men (3); Leadership Skill (3); “To Me, My X-Men!” (3); Effective Leadership (2).
- Basic (10): Forge; The X-Jet; Utopia; X-Mansion; Endurance (3); Energy; Genius; Strength.
- Weather: Clear Skies; Hurricane; Thunderstorm; Blizzard.
- Obligation: Claustrophobia.
- Nemesis: Callisto; Leader of the Morlocks; Switchblade; Knife Fight (2).

Hangar Bay (3) and the five-card Shadow King modular set are additional pack cards, outside the starter deck. Shadow King contains The Shadow King, Ruler of the Astral Plane, Possessed (2), and Astral Attack.

## Rulings (from the project owner, 2026-10-31)

Apply the August 14, 2025 designer answer to Flash Freeze: the -3 ATK applies only while the villain or minion attacks Storm herself. It does not apply when an ally defends a later attack. This supersedes the 2024 answer permitting Storm-controlled ally defenses. No rules simplifications are authorized.

- **Knife Fight after hero defeat (owner ruling, 2026-10-31):** Finish the reciprocal damage using the hero’s ATK immediately before defeat. The engine retains the ATK read before the incoming damage as a fallback if the identity is eliminated; if the identity survives, use its live ATK when dealing the reciprocal damage.

## Verification

Validated with 108 targeted examples, 4,852 non-smoke regression examples, and 165 focused smoke cases (four game seeds per case). The focused run includes Storm against every scenario on both difficulties, Shadow King against every scenario, and 50 sampled cases. Replay the sample with `SMOKE_SAMPLE_SEED=822955423` and focus on `SMOKE_HEROES=storm SMOKE_MODULARS=ShadowKingSet`. The five-second new-question watchdog remains unchanged.

An Expert Ultron multiplayer timeout led to narrowing Uncanny X-Men's trait queries and removing redundant per-message modifier scans. The original failing case and the complete focused selection passed after those fixes.
