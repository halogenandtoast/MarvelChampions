# Phoenix Hero Pack

Source: [official MC34 Phoenix rules insert](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc34_phoenix_rules_insert.pdf), p.1 (p.2 blank). The current Rules Reference v1.8 takes precedence over the insert's older keyword summaries. Printed card images and the included starter-deck list supply the ability text missing from MarvelCDB.

## Insert rules

### Amplify icon

The amplify icon increases the number of boost icons on boost cards. When a boost card is turned faceup during an enemy activation, add one additional boost icon to that card for each amplify icon in play.

### Permanent

A card with permanent cannot be defeated, leave play, or have any part of its text box blanked. This rule does not apply to cards within the same set as the card with permanent (hero set, scenario set, or modular set).

Current RR additions (`glossary-p-r.md`): set permanent cards aside before setup step 1; they enter play through other cards' abilities and do not count toward deck size. Consume the World remains in play at zero threat because it cannot be defeated. Its printed ability removes its amplify icon while it has no threat.

### Steady

A character with steady can have one additional stunned status card and one additional confused status card. That character is not stunned unless they have two stunned status cards, and is not confused unless they have two confused status cards. After that character's activation is replaced by a status card effect, remove all status cards of that type from that character.

### Team-Up

The team-up keyword names two characters. To include a card with the team-up keyword in your deck, your chosen identity must match one of the named characters. Additionally, a card with team-up cannot be played unless both of the named characters (identity or ally) are in play.

### Villainous

When a minion with villainous activates, give it a facedown boost card from the top of the encounter deck. When you resolve that minion's activation, turn the boost card faceup and apply its boost icons to the minion's stats for the activation. If the boost card has a boost ability, resolve its effects. Discard the boost card after the activation.

## Insert FAQ

**If I attach Mind Control to a minion, what happens to attachments on that minion?** They remain attached. The minion loses its minion card type and gains the ally card type, so abilities on those attachments that affect the attached minion no longer apply.

## Implementation readings

Phoenix Force enters play Restrained with four power counters. Its permanent card is set aside before the player deck is formed. The hero's resource ability removes one counter as a cost and is limited once per phase. Removing the final counter triggers a forced flip to Unleashed. Placing a counter on Unleashed Force triggers a flip only if the resulting total is at least four. Counters persist across flips. The upgrade grants its side's trait to the identity in either form, while its THW/ATK changes apply to Phoenix.

Mind Control's text box blanking removes the host's printed abilities and keywords, not its title, traits or printed stats. The host becomes an ally with THW equal to its printed SCH, printed ATK and hit points, and one consequential damage for each basic thwart or attack. This is a change of card type rather than leaving and re-entering play. Preserve its damage, statuses and attached cards. When an encounter-owned controlled ally is discarded, it goes to the encounter discard pile, not a player's discard pile. If Mind Control leaves play while its host survives, its constant control/type-changing ability ceases: the card reverts to the scenario's control and original minion type (RR Ownership and Control, Constant Abilities).

Dark Phoenix's scheming threat goes to Consume the World when that scheme is in play; otherwise normal minion scheming places it on the main scheme. Consume the World starts with six threat. Its forced response checks after threat is placed, including its initial placement, and loses the game if the total is at least twelve. Its permanent keyword prevents defeat at zero threat.

Burning Hunger resolves its sentences in order. If Phoenix is Restrained when revealed and removing its final counter flips Force to Unleashed, this does not execute the earlier Unleashed branch retroactively. The Restrained branch gains surge. Revealing Dark Phoenix from the Unleashed branch also resolves her search for Consume the World.

## Official designer clarifications

[FFG designer answers collected by Hall of Heroes, Phoenix section](https://hallofheroeslcg.com/official-ffg-rulings/#phoenix), applied with current RR v1.8:

- Psychic Manipulation is a thwart performed by the playing identity. It cannot be played while crisis prevents main-scheme threat removal, while that player has an engaged Patrol minion, or while another effect forbids that removal. It replaces the activation's threat placement with removal, rather than merely preventing placement.
  RR v1.8's confused-status exception still allows a confused identity to pay for a thwart ability with no valid target. A confused player may therefore play Psychic Manipulation during an actual villain scheme even when crisis or Patrol blocks the main scheme: remove the player's confusion instead of installing the event's replacement, and let the villain scheme normally. A confused villain does not scheme and creates no triggering window. Printed trait, form, timing, and cost restrictions still apply.
- Mind Control blanks a host's when-defeated ability through its departure: the host and attached upgrade leave together. Its attachments remain attached as described in the insert. Where Mind Control and Beguiled conflict, the conversion that entered play last determines the card's current type; preserve its appropriate original printed THW or SCH.
- The older Fiery Rage answer about ending peril before Dark Phoenix activates is superseded by current RR v1.8 activation timing; apply the project-owner ruling below.
- Psychic Misdirection still counts as defending, and the original defending identity remains attacked. Retaliate damages the attacking enemy. If redirected damage with overkill defeats a minion, the excess goes to the villain.
- Telekinetic Shield and Forcefield Generator have the same timing for enemy attack damage. The affected player chooses their order; each must affect as much of that damage as its effect permits before the next can affect the remainder.

## Printed text omitted from MarvelCDB

### Burning Hunger — 34028

Give to the Jean Grey player.

**When Revealed:** If you have the **UNLEASHED** trait, search the encounter deck, discard pile, and set-aside area for Dark Phoenix and reveal her. Then, remove Burning Hunger from the game. If you have the **RESTRAINED** trait, remove 1 power counter from Phoenix Force and this card gains surge. Discard this card.

### Fiery Rage — 34031 (three copies)

Peril.

**When Revealed:** If Dark Phoenix is in play, she activates against you. If Dark Phoenix is not in play, place 1 threat on Consume the World and this card gains surge.

Printed images: [Burning Hunger](https://marvelcdb.com/bundles/cards/34028.png), [Fiery Rage](https://marvelcdb.com/bundles/cards/34031.png).

## Starter deck

The [included Phoenix deck list](https://hallofheroeslcg.com/wp-content/uploads/2022/09/jean.jpg) lists:

- Hero: Phoenix Force; Cyclops; White Hot Room; Phoenix Suit; Rise from the Ashes; Telekinetic Shield; Mental Paralysis; Mind Control; Telekinetic Attack (2); Psychic Blast (2); Telepathic Trickery (2); Phoenix Firebird (2).
- Justice: Banshee; Marvel Girl; Mission Training (3); Psychic Manipulation (3); Mutant Peacekeepers (3); Swift Retribution (3); Passion for Justice (2).
- Basic: Storm; Cerebro; Psychic Rapport; Down Time (3); Energy; Genius; Strength.
- Obligation: Burning Hunger.
- Nemesis: Dark Phoenix; Consume the World; Fiery Rage (3).

There are 41 player cards including the permanent Phoenix Force: the player deck has 40 cards after Force is set aside. Psychic Assault, Psychic Misdirection, Psychic Kicker and Soul Sisters are additional cards, outside the prebuilt deck.

## Rulings (from the project owner, 2026-10-30)

Use the official rules. No project-specific rules simplifications are authorized.

If an effect returns a Mind Controlled encounter minion to its owner's hand, put it in the encounter discard pile. The scenario has no hand; this resolves the otherwise unspecified destination.

## Rulings (from the project owner, 2026-10-31)

Apply Rules Reference v1.8 to Fiery Rage: peril remains active throughout the Dark Phoenix activation it causes. An effect that initiates an activation finishes resolving after that activation resolves. Other players cannot play defense events or trigger abilities to help the resolving player during that activation. This supersedes the older designer answer that ended peril before the activation.

When Passion for Justice is spent to play Mutant Peacekeepers, add 1 to the total threat-removal pool before distributing it among schemes. Do not add 1 to each scheme's assigned share. Each additional Passion for Justice adds another 1 to that pool. This ruling resolves the ambiguity between a divided total and separate threat-removal instances.
