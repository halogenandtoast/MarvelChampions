# Sinister Motives: Campaign Implementation Reference

This is a non-normative implementation summary. The [official English rulebook](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc27_sinister_motives_rules_v5-compressed.pdf) and current [errata](faq-errata.md) are authoritative. Card data below comes from the local MarvelCDB dataset; printed cards and errata take precedence. Project-owner decisions are recorded separately at the end.

## Campaign structure and deck restrictions

[Rulebook p. 4](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc27_sinister_motives_rules_v5-compressed.pdf#page=4)

Win the five scenarios in order: Sandman, Venom, Mysterio, the Sinister Six, then Venom Goblin. Identities remain fixed throughout. Ordinary deck customization, including aspect changes, is allowed between scenarios unless the optional expert deck restriction is selected. Standalone play uses the scenario rules without campaign instructions.

Apply campaign Setup after ordinary scenario setup. Apply Victory instructions only after a win. A loss normally permits a retry without an additional campaign penalty; the expert final-scenario exception appears below.

Venom (Eddie Brock, 27190), Symbiote Suit (27191), Osborn Tech (27147–27152), and Sinister Assault (27158–27163) enter campaign games only when campaign instructions authorize them.

## Shared log and reputation

[Rulebook p. 5](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc27_sinister_motives_rules_v5-compressed.pdf#page=5)

The group shares one reputation total and campaign log. Victory instructions record earned reputation and scenario-specific results. Add earned reputation to the track in order. Crossing an immediate-effect node resolves its reward or penalty once; marked setup effects recur in later scenarios. Continue counting beyond the printed track for an accurate optional final score. The thresholds, scoring conditions, and setup order are listed below.

## Expert campaign

[Rulebook p. 6](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc27_sinister_motives_rules_v5-compressed.pdf#page=6)

After a win, surviving players record remaining hit points, capped at their identity's printed base hit points. Those values become their starting hit points in the next scenario. Setup for scenarios 2–5 offers a return to printed hit points in exchange for facedown encounter cards; the price depends on the scenario.

An eliminated player skips that game's Victory instructions. They may rejoin at the next setup by taking its healing option. Instructions marked expert-only do not apply to a standard campaign. An optional expert rule locks aspect/basic deck composition for the campaign while allowing additions instructed by campaign rules.

## Reputation track
[Rulebook p. 22](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc27_sinister_motives_rules_v5-compressed.pdf#page=22); node connections checked against the page image.

White-box rewards and penalties resolve immediately when their node is first marked. Pink-box setup effects apply at the beginning of each remaining scenario. Resolve left-side setup effects from top to bottom, then right-side effects from top to bottom.

| Node | Left side | Right side |
|---|---|---|
| 1 | Deal 3 randomly selected Campaign – S.H.I.E.L.D. Tech upgrades to a player; they may choose 1 for their deck for the rest of the campaign. Record its title; return the others to the collection. Repeat for each player. These do not count toward deck size. | Choose 1 Osborn Tech attachment randomly and record its name. **Setup:** shuffle every recorded Osborn Tech card into the encounter deck. |
| 5 | During the Resolve Mulligans step of game setup, each player may take 1 additional mulligan. (Current RR errata replaces printed “step 13.”) | **Setup:** place 1 [per player] threat on the main scheme. |
| 9 | Each player chooses an aspect card from any aspect and adds its maximum number of copies by title for the rest of the campaign; record it as Aspect Advantage. These cards do not count toward deck size. | **Setup:** in player order each player searches encounter deck and discard for a minion and puts it into play engaged with themself, then shuffles. Each player who did not put a minion into play this way receives a facedown encounter card. |
| 13 | **Setup:** flip each player's Campaign – S.H.I.E.L.D. Tech upgrade to its Enhanced side. | Randomly choose and record another Osborn Tech attachment. |
| 17 | Each player chooses a card from their deck and records its title as Planning Ahead. **Setup:** each player searches deck and discard for 1 copy of that card, adds it to hand, then shuffles. | **Setup:** first player searches encounter deck and discard for a scenario-specific side scheme, reveals it, places 1 [per player] threat on it, then shuffles. |
| 21 | **Setup:** each player may put a Helicarrier (Core Set 92) from their collection into play under their control. | Randomly choose and record another Osborn Tech attachment. |
| 25 | **Setup:** each player may put a Symbiote Suit (Sinister Motives 191) from their collection into play under their control. | **Setup:** deal each player 1 facedown encounter card. |

### Conditions

- +X: victory points in the victory display.
- +1: no minions in play.
- +1: no side schemes in play.
- +1: no threat on the main scheme.
- +1: fewer than 1 [per player] acceleration tokens in play.
- +1: no defeated identities.

Reputation may exceed 25; continue counting for the final score.

## Per-scenario Campaign Instructions (summary)

The setup/victory boundaries below follow the visual two-column page layout, which differs from raw PDF text extraction order.

### Sandman
[Rulebook p. 9](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc27_sinister_motives_rules_v5-compressed.pdf#page=9)

Setup:

1. Put Public Outcry (174) into play, Standard Mode Only face for standard mode or Expert Mode Only face for expert mode.
2. Shuffle Smear Campaign (175) into the encounter deck.
3. Choose 1 random Community Service side scheme (176–180), and shuffle it into the encounter deck.
4. Expert campaign: place 2 additional sand counters on City Streets, then resolve Surging Sands.

Victory: calculate and gain reputation; record a Community Service side scheme in the victory display; in expert campaign, record remaining identity hit points.

### Venom
[Rulebook p. 11](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc27_sinister_motives_rules_v5-compressed.pdf#page=11)

Setup:

1. Put Public Outcry into play.
2. Shuffle Smear Campaign into the encounter deck.
3. Shuffle 1 random Community Service side scheme whose title is not recorded in the log into the encounter deck.
4. Resolve reputation setup effects on the left from top to bottom, then on the right from top to bottom.
5. Expert campaign: set each player's hit points to the recorded remaining value. Each player may deal themself 1 facedown encounter card to reset to printed hit points. Place 1 facedown boost card on each identity.

Victory: gain reputation; record Community Service in the victory display; expert campaign records remaining identity hit points.

### Mysterio
[Rulebook p. 13](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc27_sinister_motives_rules_v5-compressed.pdf#page=13)

Setup:

1. Put Venom (190) into play under the first player's control.
2. Put Public Outcry into play.
3. Shuffle Smear Campaign and Snitches Get Stitches (181) into the encounter deck.
4. Shuffle 1 random, unrecorded Community Service side scheme into the encounter deck.
5. Resolve reputation setup effects, left then right, top to bottom.
6. Expert campaign: restore recorded hit points; each player may deal themself 2 facedown encounter cards to heal to printed hit points. In player order, each player shuffles the top 2 encounter-deck cards into their deck.

Victory: gain reputation; record Community Service in the victory display; count Illusion cards in all player decks and record that number as Waking Nightmare; expert campaign records remaining identity hit points.

### The Sinister Six
[Rulebook p. 15](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc27_sinister_motives_rules_v5-compressed.pdf#page=15)

Setup:

1. Put Venom (190) into play under the first player's control.
2. Put Public Outcry into play.
3. Shuffle Smear Campaign and Snitches Get Stitches into the encounter deck.
4. Shuffle 1 random, unrecorded Community Service side scheme into the encounter deck.
5. Place threat on Light at the End equal to the recorded Waking Nightmare number.
6. Resolve reputation setup effects, left then right, top to bottom.
7. Expert campaign: restore recorded hit points; each player may deal themself 2 facedown encounter cards to heal to printed hit points.

Victory: gain reputation; record Community Service in the victory display; record each villain still in play under Last Ones Standing; expert campaign records remaining identity hit points.

### Venom Goblin
[Rulebook p. 17](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc27_sinister_motives_rules_v5-compressed.pdf#page=17)

Setup:

1. Put Public Outcry into play.
2. Shuffle Smear Campaign into the encounter deck.
3. Find each Sinister Assault minion (158–163) whose name matches a villain recorded in Last Ones Standing; shuffle those minions into the encounter deck.
4. Resolve reputation setup effects, left then right, top to bottom.
5. Expert campaign: restore recorded hit points; each player may deal themself 3 facedown encounter cards to heal to printed hit points. Place 1 [per player] additional threat on each main scheme. Losing this game loses the campaign.

Victory: win the campaign. Optionally gain reputation and record the total marked nodes as Final Reputation Score.

## Campaign-specific cards

The rulebook identifies cards 182–189 as Campaign – S.H.I.E.L.D. Tech player cards and cards 176–180 as Campaign – Community Service encounter cards; campaign-specific cards cannot be included outside this campaign.

### Bad Publicity

```text
27174a Public Outcry [environment]
  quantity=1  traits: Standard Mode Only
  <b><i>Standard Mode Only.</i></b>
  Victory 1. Uses (2[per_hero] notoriety counters).
  <b>Response</b>: After a minion or side scheme is defeated, remove 1 notoriety counter from here.

27174b Public Outcry [environment]
  quantity=1  traits: Expert Mode Only
  <b><i>Expert Mode Only.</i></b>
  Victory 1. Uses (3[per_hero] notoriety counters).
  <b>Response</b>: After a minion or side scheme is defeated, remove 1 notoriety counter from here.

27175 Smear Campaign [treachery]
  boost=3 quantity=1  traits:
  In expert mode, this card gains surge.
  <b>When Revealed</b>: If Public Outcry is in play, place 2 notoriety counters on it, then remove this card from the game. If Public Outcry is not in play, place 2 threat on the main scheme.
```

### Community Service

```text
27176 Back Alley Burglary [side_scheme]
  boost=3 baseThreat=2 quantity=1  traits:
  Surge.
  Victory 1. <i>(When defeated, add this to the victory display.)</i>
  <b>Forced Response</b>: After you thwart this scheme, choose to either spend a [physical] resource or discard 1 random card from your hand.

27177 Cat in a Tree [side_scheme]
  boost=3 baseThreat=3 quantity=1  traits:
  Surge.
  Victory 1. <i>(When defeated, add this to the victory display.)</i>
  As an additional cost to thwart this scheme, take 2 indirect damage.

27178 Henchmen Heist [side_scheme]
  boost=3 baseThreat=3 quantity=1  traits:
  Surge.
  Victory 1. <i>(When defeated, add this to the victory display.)</i>
  <b>Forced Response</b>: After any amount of threat is removed from this scheme, place an equal amount of threat on the main scheme.

27179 Off the Rails [side_scheme]
  boost=3 baseThreat=3 quantity=1  traits:
  Surge.
  Victory 1. <i>(When defeated, add this to the victory display.)</i>
  <b>Forced Interrupt</b>: When the villain phase begins, place 1 speed counter here. If there are at least 2 speed counters here, remove this card from the game and discard the top 3 cards of each player's deck.

27180 Rubble Rescue [side_scheme]
  boost=3 baseThreat=5 quantity=1  traits:
  Surge.
  Victory 1. <i>(When defeated, add this to the victory display.)</i>
  <b>Interrupt</b>: When a character makes a basic thwart against this side scheme, <i>(they may)</i> use their ATK instead of their THW.
```

### Snitches Get Stitches

```text
27181 Snitches Get Stitches [attachment]
  boost=3 quantity=1  traits:
  Victory -1.
  Attach to Venom (Eddie Brock). If you cannot this card gains surge.
  <b>Forced Interrupt</b>: When a villain attacks, it attacks Venom. If that attack defeats Venom, add Venom and this card to the victory display.
  <b>Action</b>: Exhaust Venom and spend 2 resources of the same type → discard this card.
```

### Shield Tech

```text
27182a Compact Darts [upgrade]
  quantity=1  traits: S, H, I, E, L, D, Tech
  Setup. Permanent.
  <b>Hero Response</b>: After your hero attacks, remove 1 dart counter from here → deal 1 damage to an enemy.
  <b>Alter-Ego Action</b>: Spend 1 resource of any type → place 3 dart counters here. (Limit once per round.)

27182b Compact Darts [upgrade]
  quantity=1  traits: Enhanced, S, H, I, E, L, D, Tech
  Permanent.
  <b>Hero Response</b>: After your hero attacks, remove 1 dart counter from here → deal 1 damage to up to 2 different enemies.
  <b>Alter-Ego Action</b>: Spend 1 resource of any type → place 3 dart counters here. (Limit once per round.)

27183a Impact-Dampening Suit [upgrade]
  quantity=1  traits: S, H, I, E, L, D, Tech
  Setup. Permanent.
  Your identity gets +2 hit points.
  <b>Hero Interrupt</b>: When the villain phase begins, spend 1 resource of any type → until the end of the phase, reduce the amount of damage your hero takes from each enemy attack by 1.

27183b Impact-Dampening Suit [upgrade]
  quantity=1  traits: Enhanced, S, H, I, E, L, D, Tech
  Permanent.
  Your identity gets +3 hit points.
  <b>Hero Interrupt</b>: When your hero would take any amount of damage from an enemy attack, discard the top card of your deck → prevent 1 of that damage.

27184a Laser Goggles [upgrade]
  quantity=1  traits: S, H, I, E, L, D, Tech
  Setup. Permanent.
  Your hero gets -1 THW.
  Your hero gets +1 ATK, and your hero's basic attacks gain overkill.

27184b Laser Goggles [upgrade]
  quantity=1  traits: Enhanced, S, H, I, E, L, D, Tech
  Permanent.
  Your hero gets -1 THW.
  Your hero gets +2 ATK, and your hero's basic attacks gain overkill and piercing.

27185a Propulsion Gauntlet [upgrade]
  quantity=1  traits: S, H, I, E, L, D, Tech
  Setup. Permanent.
  <b>Hero Action</b>: Exhaust Propulsion Gauntlet and take 2 indirect damage → ready your hero.

27185b Propulsion Gauntlet [upgrade]
  quantity=1  traits: Enhanced, S, H, I, E, L, D, Tech
  Permanent.
  <b>Hero Action</b>: Exhaust Propulsion Gauntlet and take 2 indirect damage → ready your hero. Your hero gets +1 THW, +1 ATK, and +1 DEF until the end of the phase.

27186a Retinal Display [upgrade]
  quantity=1  traits: S, H, I, E, L, D, Tech
  Setup. Permanent.
  Your hero's basic thwart power <i>(THW)</i> can only remove threat from the scheme with the most threat.
  Your hero gets +1 THW, and your hero's basic thwarts ignore the crisis icon <i>([crisis])</i>.

27186b Retinal Display [upgrade]
  quantity=1  traits: Enhanced, S, H, I, E, L, D, Tech
  Permanent.
  Your hero's basic thwart power <i>(THW)</i> can only remove threat from the scheme with the most threat.
  Your hero gets +2 THW, and your hero's basic thwarts ignore the crisis icon <i>([crisis])</i> and the patrol keyword.

27187a Shock Knuckles [upgrade]
  quantity=1  traits: S, H, I, E, L, D, Tech
  Setup. Permanent.
  <b>Hero Response</b>: After your hero makes a basic attack against an enemy, discard the top card of the encounter deck. If no boost icons <i>([boost])</i> were discarded this way, stun that enemy.

27187b Shock Knuckles [upgrade]
  quantity=1  traits: Enhanced, S, H, I, E, L, D, Tech
  Permanent.
  Your hero gets +1 ATK.
  <b>Hero Response</b>: After your hero makes a basic attack against an enemy, discard the top card of the encounter deck. If 1 or fewer boost icons <i>([boost])</i> were discarded this way, stun that enemy.

27188a Wave Bracers [upgrade]
  quantity=1  traits: S, H, I, E, L, D, Tech
  Setup. Permanent.
  Your hero gets -1 ATK.
  Your hero gets +1 DEF, and gains retaliate 1 and steady.

27188b Wave Bracers [upgrade]
  quantity=1  traits: Enhanced, S, H, I, E, L, D, Tech
  Permanent.
  Your hero gets -1 ATK.
  Your hero gets +2 DEF, and gains retaliate 1 and stalwart.

27189a Wrist Navigator [upgrade]
  quantity=1  traits: S, H, I, E, L, D, Tech
  Setup. Permanent.
  <b>Forced Response</b>: After a minion or side scheme enters play, attach Wrist Navigator to it.
  <b>Interrupt</b>: When the attached card is defeated, draw 1 card. <i>(Return this card to your play area.)</i>

27189b Wrist Navigator [upgrade]
  quantity=1  traits: Enhanced, S, H, I, E, L, D, Tech
  Permanent.
  <b>Forced Response</b>: After a minion or side scheme enters play, attach Wrist Navigator to it.
  <b>Interrupt</b>: When the attached card is defeated, draw 2 cards, then discard 1 card from your hand.
```


## Rulings (from the project owner, 2026-10-30)

- Floor a negative total reputation gain at zero. Keep prior reputation and marked milestones. Include signed victory values (including Victory −1) before applying this floor. The same floor applies to the optional final reputation gain. The owner may revisit this ruling if later rules evidence settles the edge case differently.
