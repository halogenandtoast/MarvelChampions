# Synthezoid Smackdown modular sets

The expansion has eight modular sets. S.H.I.E.L.D. Ops, Young Avengers, Thunderbolts, Taskmaster, Deadly Duo, Scarlet Twins and Moon Knight are verified. The remaining modular, eight leader reward dependencies and native scenario integration remain unfinished. Existing leader/signature/main-scheme and shared Standard PvP handlers are documented alongside the Civil War foundations.

## S.H.I.E.L.D. Ops

S.H.I.E.L.D. Operator has Patrol and places two threat on the revealing player's main scheme through the normal threat windows. Its two physical copies and the three other cards form the five-card modular set. The definition generator now recognizes Patrol rather than dropping that printed keyword.

Strategic Overwatch gives the local enemy leader and local minions tough. Its boost gives the activating enemy tough. Rapid Response uses the existing Civil War handler because both printings have identical text: discard through a minion, then use its printed ATK for hero damage or printed SCH for main-scheme threat. The discarded minion is not revealed.

Homeland Security begins with three threat plus hinder one per player. Its first qualifying minion reveal each round gains surge through the existing reveal-window helper. The limit is shared among players on that board, resets at the actual round transition, and is not consumed by an opposing board's reveal. Surge deals a facedown encounter card and resolves after the original reveal (Rules Reference v1.8, Surge, p.42). Additional instances of a keyword without a number do not stack (Keywords, p.25).

Validation: ten behavior/inventory examples and the coverage gate pass together (190 examples). They include printed Patrol, physical quantities, threat, tough, boost handling, both Rapid Response identity forms, per-round limits across players and all three board-isolation cases. Randomized Rhino, Klaw and cooperative Civil War Iron Man checks pass with two seeds each (three cases, zero failures).

## Young Avengers

Patriot enters with Toughness and gives the local enemy leader tough. Hawkeye gains piercing and ranged from his text box and makes the revealing player discard a legal controlled upgrade. Stature has Guard and discards a legal controlled support. The player chooses among their own eligible cards; these effects do not gain surge when no eligible card exists.

Teenage Superheroes shares the exact reveal-repetition handler used by Avengers Assemble! and The Mighty Avengers. It resolves eligible minions one at a time in the revealing player's chosen order, uses that player for “you,” respects blanking and board boundaries, and surges if there is no ability to resolve. Under RR1.8, Surge itself is a When Revealed ability and is repeated too.

The Young Avengers side scheme has the same first-minion surge text as Homeland Security and reuses that handler with its own physical card and limit. It begins with three threat plus hinder one per player, and its limit is shared across teammates.

Eleven Young Avengers examples pass alongside S.H.I.E.L.D. Ops and coverage (202 examples total). Randomized Rhino, Klaw and cooperative Captain America checks pass with two seeds each (three cases, zero failures). Royal Guard, the leader reward dependencies and native Synthezoid scenario integration remain unfinished.

## Thunderbolts

Atlas and Songbird attempt to stun or confuse the friendly leader, then apply the status to the revealing player if no new status was placed. Their boost abilities retain the callback needed to check the result. A steady leader can receive its second status; an already affected or stalwart leader triggers the fallback. These follow “Otherwise” in Rules Reference v1.8, p.31.

Penance deals two damage to the friendly leader. Without a friendly leader, the player chooses a controlled character for two damage when revealed or one damage as a boost. Damage prevented by tough still counts as damage dealt: the rules distinguish modification of damage taken from modification of damage dealt (Damage, p.12). Thus removing a friendly leader's tough does not also damage the player.

Justice Like Lightning reuses the identical Civil War printing's handler. The Thunderbolts starts with three threat plus hinder one per player, contributes a hazard icon, and blocks threat removal while a Thunderbolt minion is present on its own board. Opposing-board minions do not protect it.

Validation: the Thunderbolts, Defenders, S.H.I.E.L.D. Ops, Young Avengers and coverage checks pass together (256 examples, zero failures). Randomized Rhino, Klaw and cooperative Civil War Iron Man checks pass with two seeds each (three cases, zero failures).

## Taskmaster

Taskmaster enters with Toughness, Retaliate 1 and Villainous, then activates against the revealing player. His Sword grants one ATK and piercing. His Shield grants one SCH and reduces damage taken by one per instance, using the same damage reduction mechanism as Thanos's Armor. Both attach to an eligible Taskmaster before falling back to the local enemy leader. Their Hero Actions cost mental and physical resources and do not exhaust the hero.

Mimicked Move makes Taskmaster attack the friendly leader with his villainous boost. If that attack cannot be initiated because Taskmaster or a friendly leader is absent, the local enemy leader instead receives tough and a facedown boost card. Its boost grants the activating enemy another boost card. Taskmaster's Academy has hinder one per player and an amplify icon; its defeat gives its own board's leader tough and a facedown boost card, including when an opponent removes the last threat.

Validation: fifteen Taskmaster behavior/inventory examples pass alongside Namor and coverage (217 examples, zero failures). Randomized Rhino, Klaw and cooperative Civil War Iron Man checks pass with two seeds each (three cases, zero failures).

## Deadly Duo

Jester and Jack O'Lantern discard the revealing player's top three cards and count distinct types among cards actually discarded. Jester assigns that much indirect damage in either identity form; Jack places that much threat on the local main scheme through the normal threat windows. Repeating their reveal abilities uses the new revealing player's deck, even if the minion is engaged with someone else.

Mad Jack's Platform reuses Resistance Fighter's identical reveal, boost and modifier behavior: the opposing team chooses a legal minion, the attachment grants four hit points and stalwart, and it surges if no legal host exists. Its boost can attach only to a minion engaged with the player resolving that boost and consumes the physical boost card when attached.

Jester's Yo-Yo compares the printed costs of legally discardable controlled allies, supports, upgrades and player side schemes. The player breaks ties among the most expensive cards; with none it surges. Its boost discards a controlled upgrade without that cost comparison. Deadly Duo has hinder one per player, and defeating it discards eight cards from each player's deck on its own board, regardless of who removed its last threat.

Validation: twenty Deadly Duo behavior/inventory examples pass alongside Hell’s Kitchen, Young Avengers, Heroes for Hire, Next Evolution modulars and coverage (298 examples, zero failures). Randomized Rhino, Klaw and cooperative Civil War Iron Man checks pass with two seeds each (three cases, zero failures). The shared controlled-card selector now includes controlled player side schemes, with a Yo-Yo regression test confirming discard to the owner without defeat.

## Scarlet Twins

Speed has Quickstrike and attempts to stun the revealing player. If no new stun is placed, that identity takes two ability damage through its normal damage windows. Wiccan has Incite 1 and attempts to confuse the player, placing two additional threat on the local main scheme only if no new confusion is placed. Steady's second status counts as a successful placement (Otherwise, RR1.8 p.31).

Superspeed discards a random hand card using the stored game generator and assigns indirect damage equal to its printed cost. Its boost instead lets the player choose one hand card and deals no damage. Superpowered Siblings starts with three threat plus hinder one per player and has an amplify icon; its defeat makes the player who removed the last threat discard a chosen hand card.

Spellcasting remains in its owner's play area until that player plays a card. Its forced interrupt cancels the pending card effects, retains payment and played-card timing, discards the card and then the obligation. Event cancellation preserves event-specific played windows and ordinary event cleanup; other canceled cards do not enter play. The cancellation helper checks that an effect remains pending before consuming a second copy. Rules Reference v1.8 Cancel (p.11) keeps costs paid and treats a canceled event as played. A search for newer Spellcasting-specific rulings found no exception.

Validation: nineteen Scarlet Twins behavior/inventory examples pass alongside Gamora and coverage (238 examples, zero failures). These include paid ally, support, upgrade and event cancellations, single discard, and Gamora’s response after a canceled attack event. Randomized Rhino, Klaw and cooperative Civil War Captain America checks pass with two seeds each (three cases, zero failures).

## Moon Knight

Moon Knight enters with Toughness and Retaliate 2, then resolves the topmost treachery's When Revealed ability from the local encounter discard pile. The physical treachery remains in that pile and is not discarded a second time. The shared replay helper now also resolves printed or gained surge at completion; it does not reveal the card again or apply reveal-only keywords such as Incite.

Crescent Dart attaches to an eligible Moon Knight, otherwise to the local enemy leader. Its forced interrupt grants piercing and ranged to the attached character's next attack and discards the Dart after that attack ends. It supports both enemy and converted-ally hosts. Fist of Khonshu exhausts the revealing identity and deals two ability damage; its boost exhausts only.

Khonshu's Avatar starts with three threat plus hinder one per player and has a hazard icon. Its defeat discards through a treachery in its own encounter deck, discards that found card once, then resolves its When Revealed ability. The replay uses the player who defeated the scheme for “you.”

Validation: eleven Moon Knight behavior/inventory examples pass alongside Paladin, Wonder Man aspect cards and coverage (233 examples, zero failures). These include printed and gained surge, discard-pile identity preservation, piercing and ranged, and the defeat search. Randomized Rhino, Klaw and cooperative Civil War Captain America checks pass with two seeds each (three cases, zero failures).
