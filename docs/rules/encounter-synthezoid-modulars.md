# Synthezoid Smackdown modular sets

The expansion has eight modular sets. S.H.I.E.L.D. Ops, Young Avengers and Thunderbolts are verified. The remaining five modulars, eight leader reward dependencies and native scenario integration remain unfinished. Existing leader/signature/main-scheme and shared Standard PvP handlers are documented alongside the Civil War foundations.

## S.H.I.E.L.D. Ops

S.H.I.E.L.D. Operator has Patrol and places two threat on the revealing player's main scheme through the normal threat windows. Its two physical copies and the three other cards form the five-card modular set. The definition generator now recognizes Patrol rather than dropping that printed keyword.

Strategic Overwatch gives the local enemy leader and local minions tough. Its boost gives the activating enemy tough. Rapid Response uses the existing Civil War handler because both printings have identical text: discard through a minion, then use its printed ATK for hero damage or printed SCH for main-scheme threat. The discarded minion is not revealed.

Homeland Security begins with three threat plus hinder one per player. Its first qualifying minion reveal each round gains surge through the existing reveal-window helper. The limit is shared among players on that board, resets at the actual round transition, and is not consumed by an opposing board's reveal. Surge deals a facedown encounter card and resolves after the original reveal (Rules Reference v1.8, Surge, p.42). Additional instances of a keyword without a number do not stack (Keywords, p.25).

Validation: ten behavior/inventory examples and the coverage gate pass together (190 examples). They include printed Patrol, physical quantities, threat, tough, boost handling, both Rapid Response identity forms, per-round limits across players and all three board-isolation cases. Randomized Rhino, Klaw and cooperative Civil War Iron Man checks pass with two seeds each (three cases, zero failures).

## Young Avengers

Patriot enters with Toughness and gives the local enemy leader tough. Hawkeye gains piercing and ranged from his text box and makes the revealing player discard a legal controlled upgrade. Stature has Guard and discards a legal controlled support. The player chooses among their own eligible cards; these effects do not gain surge when no eligible card exists.

Teenage Superheroes shares the exact reveal-repetition handler used by Avengers Assemble! and The Mighty Avengers. It resolves eligible minions one at a time in the revealing player's chosen order, uses that player for “you,” respects blanking and board boundaries, and surges if there is no ability to resolve. Under RR1.8, Surge itself is a When Revealed ability and is repeated too.

The Young Avengers side scheme has the same first-minion surge text as Homeland Security and reuses that handler with its own physical card and limit. It begins with three threat plus hinder one per player, and its limit is shared across teammates.

Eleven Young Avengers examples pass alongside S.H.I.E.L.D. Ops and coverage (202 examples total). Randomized Rhino, Klaw and cooperative Captain America checks pass with two seeds each (three cases, zero failures). Five other modular sets, the leader reward dependencies and native Synthezoid scenario integration remain unfinished.

## Thunderbolts

Atlas and Songbird attempt to stun or confuse the friendly leader, then apply the status to the revealing player if no new status was placed. Their boost abilities retain the callback needed to check the result. A steady leader can receive its second status; an already affected or stalwart leader triggers the fallback. These follow “Otherwise” in Rules Reference v1.8, p.31.

Penance deals two damage to the friendly leader. Without a friendly leader, the player chooses a controlled character for two damage when revealed or one damage as a boost. Damage prevented by tough still counts as damage dealt: the rules distinguish modification of damage taken from modification of damage dealt (Damage, p.12). Thus removing a friendly leader's tough does not also damage the player.

Justice Like Lightning reuses the identical Civil War printing's handler. The Thunderbolts starts with three threat plus hinder one per player, contributes a hazard icon, and blocks threat removal while a Thunderbolt minion is present on its own board. Opposing-board minions do not protect it.

Validation: the Thunderbolts, Defenders, S.H.I.E.L.D. Ops, Young Avengers and coverage checks pass together (256 examples, zero failures). Randomized Rhino, Klaw and cooperative Civil War Iron Man checks pass with two seeds each (three cases, zero failures).
