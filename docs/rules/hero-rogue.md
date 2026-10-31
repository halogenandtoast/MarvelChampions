# Rogue hero pack

The [official English rules insert](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc38_rogue_rules_insert.pdf), [Rules Reference v1.8](README.md), and [current errata](faq-errata.md#rogue-hero-pack) are authoritative. The insert's Teamwork text predates the current Rules Reference.

## Keywords

- **Stalwart:** a character cannot be stunned or confused. Gaining stalwart removes existing stun and confuse status cards (current RR).
- **Team-Up:** the deck's identity must match one of the two named characters. Playing the card requires both named characters, as identities or allies, in play.
- **Teamwork (trait):** the printed insert makes all matching teamwork minions activate after a new matching minion enters play and engages a player, provided another matching minion is in play. Current RR v1.8 supersedes this: only the entering minion activates. Resolve its When Revealed ability before Teamwork; merely changing engagement is not entering play.
- **Villainous:** deal the activating minion a facedown encounter boost card. Turn it faceup during that activation, apply its boost icons and resolve its boost ability, then discard it.

## Insert clarification and FAQ

**What does “find Touched” mean?** Search every game area where Touched could be found, including play, set-aside, player deck, discard pile, and encounter deck. Current RR Find excludes the victory display, removed-from-game cards, and facedown encounter cards in in-play areas. Avoid unnecessary searches if its location is known.

**What happens to attachments when Karma takes control of a minion?** They remain attached. The host loses the minion type and gains the ally type; abilities on those attachments that affect the attached minion no longer apply.

## Current errata

- **Anna Marie:** Setup and Withdrawn both instruct the player to find Touched and set it aside.
- **Rogue:** Skin Contact finds Touched and attaches it to another character. Rogue gains that character's traits until the round ends; once per round.
- **Energy Transfer:** find Touched and attach it to a character other than Rogue and deal 2 damage to that character as the cost; heal 2 damage from Rogue, ready her, and gain that character's traits until the round ends.
- **Mystique's Manipulations:** the defeating player searches the encounter deck and discard for Misled and shuffles it into their own deck.
- **Bonebreaker:** its engagement ability is a Forced Response.

## Rules used by the implementation

- Energy Transfer **deals** damage as a cost. RR Costs says this cost is paid even if some or all damage is prevented. This differs from **taking** damage as a cost, which requires the full amount to be taken. Tough does not prevent paying a deal-damage cost.
- Superpower Adaptation uses the character's owner, preserving the owner of any transferred event. The event returns to its owner's discard when played or discarded. RR Classifications defines the three relevant groups as identity-specific, aspect, and basic; the aspect classification includes all aspects.
- Karma changes the host's card type and control without making it leave and re-enter play. Preserve damage, statuses, and attachments. Its printed text is blank; its THW is printed SCH and it takes 2 consequential damage for either basic power. The lasting control effect ceases when Karma leaves play, restoring a surviving host as a minion; blanking Karma after resolving its response does not cancel that lasting effect.
- Misled retains its encounter identity in player zones. Its forced response triggers whenever it enters hand, and normal player-phase discarding can send it to the encounter discard.
- Med Lab captures only allies defeated by consequential damage; its later action plays its held ally while paying the ordinary cost, with that ally entering exhausted.

## Starter deck

Rogue's sixteen signature cards include Touched, which starts set aside. Protection cards: Iceman ×1, Karma ×1, Armor ×1, Unflappable ×3, Judoka Skill ×3, Preemptive Strike ×3, Not Today! ×3, Defensive Energy ×2. Basic cards: Moira MacTaggert ×1, X-Gene ×3, Beauty and the Thief ×1, Energy ×1, Genius ×1, Strength ×1. Total: 41 cards including Touched (40 remain in the ordinary deck/hand after setup). Touched counts toward deck size. Med Lab is an additional deckbuilding card, outside this starter deck. The insert provides strategy advice rather than an enumerated deck list; these quantities follow the pack's card data.

## Rulings (from the project owner, 2026-10-31)

Use the current Rules Reference and errata. The specific readings above derive from those rules; they are not separate project-owner rulings.

## Designer clarifications checked during implementation

[FFG designer answers, October 9, 2026 and January 17, 2026](https://hallofheroeslcg.com/latest-ffg-rulings-post-rrg-1-7/) clarify that Rogue's lasting copied traits update when the target's traits change and stop when it leaves play. Copying uses the target's current form, including alter-ego traits. Alter-egos are characters under RR v1.8; Touched's hero-only stalwart bonus still requires hero form. Bulletproof Belle prevents damage taken: a piercing attack that deals positive damage after defense still discards its newly granted tough.

[Earlier FFG answers, questions 297, 300, 306 and 309](https://hallofheroeslcg.com/latest-ffg-rulings/) clarify that Energy Transfer cannot attach Touched to its current host, and grants no copied traits if its cost defeats the host. Healing and readying still resolve. Touched counts toward deck size. Med Lab can retrieve a consequentially defeated ally from the discard pile, victory display, or The Collection.

[FFG answer, March 22, 2024, question 154](https://hallofheroeslcg.com/latest-ffg-rulings-post-rrg-1-5/) confirms that playing an ally through Med Lab counts as playing it from hand for its triggered abilities.
