# The Galaxy's Most Wanted, Scenario #2: Infiltrate the Museum (Collector, The Collection)

> Source: *Marvel Champions: The Card Game* — *The Galaxy's Most Wanted* campaign expansion (MC16) rulebook (FFG,
> 2021), pages 9-10 (page 9 is comic art) and the FAQ on page 21. Rulebook text is verbatim except for formatting;
> `<!-- TGMW rulebook p.N -->` gives the page. Card text is quoted from MarvelCDB
> (https://marvelcdb.com/api/public/cards/gmw.json) and checked against the printed cards; where they differ the
> printed card is given and followed (see "Printed cards and MarvelCDB").

Related text in this folder: the box's keywords, victory display and amplify icon in
[scenario-galaxys-most-wanted.md](scenario-galaxys-most-wanted.md); the RR glossary entries **Hinder X**, **Victory
X**, **Villainous**, **Patrol**, **Guard**, **Quickstrike**, **Indirect Damage**, **Surge**, **Permanent**, **In Play
and Out of Play**.

## Summary (non-normative)

```
Villain deck: Collector I, II (expert: II, III). Main scheme: The Grand Collection (1A/1B). Encounter deck:
  Infiltrate the Museum, Galactic Artifacts, Menagerie Medley (the modular set, replaceable), Standard (+ Expert).
The Collection: an out-of-play area shared by all players; cards in it are faceup, out of play.
Collector (every stage): a card leaving play for a discard pile goes into The Collection instead (III: then 1 threat
  on the main scheme). Only from play to a discard pile: not set aside, removed from the game, shuffled into a deck,
  returned to hand, nor discarded from a hand or a deck (FAQ).
1A Setup: the top card of each player's deck into The Collection.
1B: 10 per player threat, starts with 4 per player, +1 per player. Hero Action (once per round per player):
  exhaust your hero or spend 2 resources -> discard 1 card from The Collection to its owner's discard pile.
  The players lose with 5 per player cards in The Collection, or if this stage is completed.
```

## Scenario #2 - Infiltrate the Museum
<!-- TGMW rulebook p.10 -->

*Though the Badoon were formidable, the teamwork and coordination between the Avengers and Guardians was enough to
quell Drang's onslaught. After the battle, Gamora is able to extract critical information from Drang about why they
attacked Earth by using a few... persuasive... interrogation techniques.*

*As it turns out, Taneleer Tivan—better known as the Collector—heard about the Reality Stone on Earth. Already having
the Power Stone in his possession, and always looking to expand his collection, he hired the Badoon to steal Earth's
Stone. Deciding it best if the Collector has no Stones at all, you set out toward his museum.*

**Villain Deck**: Collector (I), Collector (II)

*Remove Collector (I) and add Collector (III) for expert mode.*

**Main Scheme Deck**: The Grand Collection (1A/1B)

**Encounter Deck**: Infiltrate the Museum, Galactic Artifacts, Menagerie Medley, and Standard encounter sets.

*The Menagerie Medley modular set can be removed from this scenario and/or added to other scenarios when using
scenario customization rules.*

## The Collection
<!-- TGMW rulebook p.10 -->

Throughout the course of the Infiltrate the Museum scenario, cards may be added to "The Collection". The Collection is
an out-of-play game area shared by all players and specific to this scenario. Cards in The Collection follow the
standard rules for out-of-play cards.

When creating space for The Collection area, ensure that The Collection is easily visible and reachable for all
players, as each player may have to interact with The Collection at multiple points during the scenario.

## Campaign Instructions
<!-- TGMW rulebook p.10 -->

### Setup:

- Reveal the Gallery of Splendor (179A) side scheme (use the reverse side for expert mode).
- Shuffle the Badoon Headhunter (183) minion into the encounter deck. Then, if there is at least 1 mark in the
  "Headhunter Defeated?" section of the campaign log, shuffle the On the Hunt (184) treachery *(from the Badoon
  Headhunter modular set)* into the encounter deck.
- **Expert Campaign Only**: Set each player's hit points to their remaining hit point value recorded in the campaign
  log for the previous scenario.
- **Expert Campaign Only**: Each player may subtract 1 unit from their respective "Unspent Units" box in the campaign
  log to heal their identity to its printed hit point value.
- **Expert Campaign Only**: In player order, each player must choose 1 card from their hand and put it faceup into
  The Collection.

### Victory:

In the campaign log:

- Record units in each player's respective "Unspent Units" box *(add these to units you already have)*:
  - Record 1 unit for each player.
  - Record a number of units for each player (to a maximum of 3 units) equal to the victory values on encounter
    cards in the victory display.
  - Record 1 unit for each player if there are 1[per_hero] or fewer cards in The Collection.
  - Record 1 unit for each player if there is no threat on the main scheme.
- In the "Cards in The Collection" section, record the title of each player card in The Collection.
- If Badoon Headhunter is in the victory display, mark the box beside "Scenario #2 - Infiltrate the Museum" in the
  "Headhunter Defeated?" section.
- **Expert Campaign Only**: Record each identity's remaining hit points.

The campaign steps are not implemented here; what they read is: `getCollection` (the cards in The Collection, a player
card's owner its `pcOwner`), the main scheme's threat, `getVictoryDisplay` / `getVictoryPoints`, and
`placeInCollection` for putting a card there (from a hand, deck, discard pile or play).

## Frequently asked questions
<!-- TGMW rulebook p.21 -->

**Q.** Collector's ability for scenario #2 (Infiltrate the Museum) says "When a card would be placed into a discard
pile from play..." Which card types does this apply to?
**A.** This applies to all card types that enter play. For encounter cards, this is attachments, environments, minions,
obligations, and side schemes. For player cards, this is allies, supports, and upgrades.

**Q.** In scenario #2 (Infiltrate the Museum), if a card would be set aside, removed from the game, shuffled into a
deck from play, or added to a player's hand from play, will Collector's "Forced Interrupt" ability trigger?
**A.** No. Collector's "Forced Interrupt" ability will only trigger if a card in play would be placed specifically
into a discard pile. If a card is moved from play to a game area other than a discard pile, Collector's ability will
not trigger. Additionally, if a card is moved from one out-of-play area into a discard pile (such as a card being
discarded from a player's hand or from the encounter deck), Collector's ability will not trigger.

## Scenario cards (MarvelCDB card text, checked against the printed cards)

Infiltrate the Museum:

- **16070 Collector (I)** (villain; Elder; SCH 2, ATK 1, 13[per_hero] HP). **Forced Interrupt**: When a card *(player
  or encounter)* would be placed into a discard pile from play, put it faceup into The Collection instead.
- **16071 Collector (II)** (SCH 3, ATK 2, 14[per_hero] HP). **When Revealed**: In player order, each player must choose
  to either put the top card of their deck faceup into The Collection or take 3 damage. **Forced Interrupt**: as I.
- **16072 Collector (III)** (SCH 4, ATK 3, 18[per_hero] HP). **When Revealed**: Put the top card of each player's deck
  faceup into The Collection. Place 1 threat on the main scheme for each card in The Collection. **Forced
  Interrupt**: When a card would be placed into a discard pile from play, put it faceup into The Collection instead,
  then place 1 threat on the main scheme.
- **16073A The Grand Collection** (main scheme 1A). **Contents**: Collector (I) and Collector (II). *(Collector (II)
  and Collector (III) instead for expert mode.)* Infiltrate the Museum, Galactic Artifacts, and Standard encounter
  sets. One modular encounter set *(Menagerie Medley)*. **Setup**: Create "The Collection" game area *(see insert for
  details)*. Put the top card of each player's deck faceup into The Collection.
- **16073B The Grand Collection** (1B; threat 10[per_hero], starting threat 4[per_hero], +1[per_hero]). **Hero
  Action**: Choose to either exhaust your hero or spend 2 resources of any type → discard 1 card from The Collection
  *(to its owner's discard pile)*. (Limit once per round per player.) **If there are at least 5[per_hero] cards in
  The Collection or if this stage is completed, the players lose the game.**
- **16074 Biogram Image** (attachment; Illusion; boost [star] + 1). Attach to Collector. **Forced Interrupt**: When
  Collector would take any amount of damage, put this card faceup into The Collection → prevent all of that damage,
  then place threat on the main scheme equal to the amount prevented this way. [star] **Boost**: After this
  activation ends, reveal this card.
- **16075 Monark Starstalker** (minion, unique; Bounty Hunter; SCH 2, ATK 2, 7 HP; boost 3). Villainous.
- **16076 Inconspicuous Box** (treachery ×2; boost [star]). **When Revealed**: Put the lowest cost card you control
  faceup into The Collection. If you cannot, this card gains surge. [star] **Boost**: If there are 3[per_hero] or
  fewer cards in The Collection, put the top card of your deck faceup into The Collection.
- **16077 View the Cosmos** (treachery ×2; boost 2). **When Revealed**: Choose one: Put the highest cost card from
  your hand faceup into The Collection; or Discard the highest cost card from your hand, then place threat on the main
  scheme equal to its printed cost.
- **16078 Stay Awhile** (treachery ×2; boost 1). **When Revealed (Alter-Ego)**: Choose to either spend
  [physical][physical] resources or put the top card of your deck faceup into The Collection. **When Revealed
  (Hero)**: Collector attacks you with +1 ATK. If you take any amount of damage from that attack, put the top card of
  your deck faceup into The Collection.
- **16079 Caught Off Guard** (treachery; boost 1). **When Revealed**: Discard an upgrade or support you control. If no
  cards were discarded this way, this card gains surge. (The core set's 01188, the same card.)

Galactic Artifacts (modular):

- **16122 Cloak of Hercules** (attachment, unique; Artifact; boost 2; printed **+3 ATK**). Attach to the enemy with the
  lowest ATK. **Hero Action**: Spend [physical][physical][physical] resources → discard this card.
- **16123 Obedience Potion** (attachment, unique; Artifact; boost 2). Attach to your identity. Attached character gets
  -1 THW, -1 ATK, and -1 DEF. **Hero Action**: Take 1 damage and spend [mental][mental] resources → discard this card.
  *(Any player can do this.)*
- **16124 The Beyonder's Blazer** (attachment, unique; Artifact; boost 3; amplify icon). Attach to the enemy with the
  highest SCH. **Hero Action**: Place 2 threat on the main scheme and spend 2 resources **of the same type** → discard
  this card.
- **16125 The Poison** (attachment, unique; Artifact; boost 2). Attach to your identity. **Forced Interrupt**: When your
  turn begins, place 1 poison counter here, then take 1 damage for each poison counter here. **Hero Action**: Spend 3
  resources of different types → discard this card. *(Any player can do this.)*
- **16126 Vandarian Power Wand** (attachment, unique; Artifact; boost 1; hazard icon). Attach to the enemy with the
  lowest SCH. **Hero Action**: Spend [energy][energy][energy] resources → discard this card.
- **16127 Hujahdarian Monarch Egg** (side scheme, unique; 3 threat; boost 1; acceleration icon). Hinder 2[per_hero].
  Victory 0. **When Defeated**: The player who defeated this scheme may ready their identity.
- **16128 Magical Teapot** (side scheme, unique; 3; boost 1; hazard icon). Hinder 2[per_hero]. Victory 0. **When
  Defeated**: The player who defeated this scheme may heal 4 damage from their identity.
- **16129 Philosopher's Stone** (side scheme, unique; 3; boost 1; amplify icon). Hinder 2[per_hero]. Victory 0.
  **When Defeated**: The player who defeated this scheme may draw 2 cards.
- **16130 Crystal Ball** (side scheme, unique; 3; boost 1; acceleration icon). Hinder 2[per_hero]. Victory 0. **When
  Defeated**: The player who defeated this scheme may play a card from their hand, reducing its resource cost by 3.

Menagerie Medley (modular):

- **16135 Psionic Ghost** (minion ×4; Ghost; SCH 2, ATK 2, 4 HP; boost [star]). **When Revealed**: You are confused.
  If you are already confused, take 1 damage. [star] **Boost**: Put Psionic Ghost into play engaged with you.
- **16136 Servant Bot** (minion ×3; Robot; SCH 1, ATK 1, 3 HP; no boost icons). Guard. Patrol.
- **16137 Starshark** (minion ×2; Creature, Elite; SCH 1, ATK 3[star], 7 HP; boost [star]). Quickstrike. [star]
  Starshark's attacks deal indirect damage. [star] **Boost**: Deal 1 damage to each character you control.

### Printed cards and MarvelCDB

MarvelCDB leaves out Cloak of Hercules's +3 ATK, The Beyonder's Blazer's "of the same type", the Vandarian Power Wand's
and Magical Teapot's hazard icons, and names two cards differently from the printed ones: "Vandarian Power Stone" is
printed **Vandarian Power Wand**, "Monarch Starstalker" is printed **Monark Starstalker**. The engine follows the
printed cards.

## How the engine plays it

- **The Collection** is the game's `gameCollection`, shown on the board as a pile beside the victory display. A card
  goes in by `PlaceInCollection` (lifted `placeInCollection`): from a hand, a deck or a discard pile it is simply taken
  from there; a card in play leaves play for it (`RemoveFromPlay`, its attachments and held cards going where they
  would), and is not discarded, so Collector does not answer it.
- **"Would be placed into a discard pile from play"** is a window (`WouldBeDiscardedFromPlay`, matched by
  `CardWouldBeDiscardedFromPlay`) opened as an ally, support, upgrade, minion, attachment, side scheme, environment or
  obligation in play leaves it for its discard pile; Collector's forced interrupt puts the card into The Collection in
  place of the discard. A treachery, an event, a boost card and a card discarded from a hand or a deck open no such
  window. An obligation revealed and resolved is discarded as a treachery is (it never entered play); one that stays
  in its player's area, discarded later, is in play.
- **Discarding from The Collection** (The Grand Collection's action) sends the card to its owner's discard pile, an
  encounter card to the encounter discard pile.
- **Indirect damage from an attack** (Starshark): the attack's damage past a hero's defense is assigned among the
  characters the attacked player controls (an ally defending: its controller's), each share dealt as the attack's
  damage; the defending character is the one attacked (RR Indirect Damage).
- **Biogram Image and a tough Collector**: a tough status card's forced interrupt has priority over other forced
  interrupts to the same "would take damage" (RR Simultaneous Timing Priority), so it prevents the damage first and
  Biogram Image has nothing left to answer: it stays attached, and no threat is placed. In the engine, no "when the
  villain would take damage" interrupt answers damage his tough status card prevents; one to his being attacked
  (Temporal Shield) still does.
- **"Attach to the enemy with the lowest ATK"** (Cloak of Hercules; highest SCH, The Beyonder's Blazer; lowest SCH,
  Vandarian Power Wand): with enemies tied, the first player picks among them, whoever revealed it (RR First Player:
  an encounter card with several eligible targets).

## Open rulings

- **The Grand Collection's "either" cost** is two equal abilities (the same card and index, so one "once per round per
  player" limit covers both): exhaust your hero, or spend 2 resources. The client shows the action twice, one per cost.
- **The Beyonder's Blazer's "2 resources of the same type"** is one ability per resource type (a wild resource being
  any), labeled with the type; the 2 threat, part of the cost, is placed as the ability resolves, before the card is
  discarded.
- **Facedown cards held under a support, upgrade or obligation** discarded with it are discarded plainly, not taken by
  Collector: facedown, they are not cards of a type that entered play (FAQ lists the types).
- **"The lowest cost card you control"** (Inconspicuous Box) is among the player's allies, supports and upgrades by
  printed cost, a permanent one (the Milano) not being able to leave play; "the highest cost card from your hand" (View
  the Cosmos) counts a card with no cost (a resource) as 0.
- **"When your turn begins"** (The Poison) answers the window the engine opens as a turn begins (after the turn
  begins, before the player acts).
- **Biogram Image's boost** reveals it once the activation's boost cards are cleared, from the encounter discard pile.
- **Starshark's indirect attack damage** is the attack's damage (`FromAttack`), so what answers attack damage answers
  each share.
- **Caught Off Guard** (both copies) leaves a permanent support out of its choice; with only permanent ones, nothing
  is discarded and it gains surge.

## Rulings (from the project owner, 2026-10-27)

- Inconspicuous Box ("lowest cost card you control") and View the Cosmos ("highest cost card from your hand"): the player whose cards they are breaks ties, not the first player.
