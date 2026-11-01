# Age of Apocalypse: Unus and associated modular sets

Source: official [MC45 rulebook](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc45_age_of_apocalypse_rulebook.pdf), pp. 8–9; Rules Reference v1.8, especially Damage, Consequential Damage, Permanent, Setup, and Timing. Card text is in the local MarvelCDB data. Printed Gene Pool checked against its card image on 2026-11-01: its starting threat is **4 fixed**, not per player.

## Standalone setup

Use Unus I and II in standard mode, II and III in expert. Hunting Gene Traitors has an 11-per-player threshold, starts with no threat, and escalates by one per player. Completing it loses the game. The Unus and Infinites sets are mandatory; Dystopian Nightmare is the recommended interchangeable modular set. Use the chosen Standard set, plus the appropriate Expert set in expert difficulty.

Gene Pool has Permanent and Setup: it begins in play with four threat and cannot be defeated or leave play through ordinary effects. Hunting Gene Traitors adds one threat to Gene Pool after step one of each villain phase. Expert setup deals each player one facedown encounter card.

The rulebook permits optional *additional* Gene Pool threat to adjust difficulty. Its recommended additional amounts are zero skirmish, one standard, two expert, three heroic. These are optional additions, not replacements for its printed starting four. The digital setup offers zero through three extra threat; zero is the first/default choice. This optional setup adjustment is distinct from selecting the scenario's standard or expert villain stage.

## Gene Pool thresholds

At three threat Unus gains retaliate 1; at six he also gains stalwart; at nine he also gains one amplify icon. These are continuous conditions that disappear when threat falls below their thresholds. All three stages share these rules.

Infinite Soldier always has guard. It gains quickstrike at three Gene Pool threat, surge at six, and three extra hit points at nine. Keywords are evaluated while it enters play or is revealed, so quickstrike and surge apply immediately if the thresholds are already met.

After an ally is defeated for any reason other than consequential damage, Gene Pool gains three threat. Discarding an ally is not defeating it. An ally defeated by ability damage does count. Consequential defeat and its generic defeat notification describe the same defeat and must not cause Gene Pool's response.

## Unus cards

Prelate Sidearm attaches to Unus, adds one ATK, and places one Gene Pool threat after his attack defeats an ally. This is in addition to Gene Pool's own three-threat response. After a hero's basic attack against Unus, that attacking player may pay energy and physical to discard the Sidearm.

Prelate Armor attaches to Unus, adds one SCH, and gives him tough after he schemes. After a hero's basic attack against Unus, that player may pay mental and physical to discard the Armor. Attack events do not satisfy either equipment's basic-attack restriction.

Infinite Hunter deals three damage to an ally controlled by its revealing player. With no ally, it deals no damage to the identity instead. Its boost ability lets the player choose between two Gene Pool threat and a two-point ATK/SCH bonus for the activation.

Genetic Experiments attaches to an Infinite minion chosen by the revealing player, adds one ATK, one SCH, and two hit points, and places two threat on Gene Pool when that minion is defeated. Without an Infinite minion its revealed effect surges; its boost effect simply has no target. Its boost effect attaches the actual boost card, consuming that card from the boost area.

Infinite Prelate makes Unus activate against its revealing player. At three Gene Pool threat he receives tough; at six he also heals three; at nine the activation receives an additional boost card. The activation's extra boost is reserved before the activation begins so it can affect that activation; tough and healing resolve after the activation. A stunned/confused activation must not leave a reserved boost for a later activation.

Endless Ranks begins with four fixed threat and an acceleration icon. Defeating it adds three threat to Gene Pool.

## Infinites and Dystopian Nightmare

Culling the Weak places four Gene Pool threat when revealed, two as a boost.

Hunted remains in its revealing player's play area. Its owner may use its alter-ego action to discard a card from hand and discard the obligation.

War-Weary stuns the revealing player, or deals two damage instead if already stunned. Its boost ability does the same to the player against whom the enemy is activating.

Targeted for Extermination begins with three fixed threat and a crisis icon. The player whose card removed its final threat is confused when it is defeated, even if that player is not the active player.

## Implementation and owner rulings

No unresolved card-text ruling was identified. The owner approved an optional Gene Pool setup choice on 2026-11-01. The engine uses the stored game generator for encounter-deck randomness. Campaign setup/rewards are handled separately by the Age of Apocalypse campaign implementation.

## Rulings (from the project owner, 2026-11-01)

Expose the optional additional Gene Pool starting-threat choice. The printed setup is the default, with zero additional threat. The player may choose the optional increase during setup.
