# Psylocke Hero Pack

Sources: [FFG MC41 rules insert](https://images-cdn.fantasyflightgames.com/filer_public/b3/41/b341e3d0-01ef-471d-b0d3-657ae05757c6/mc41_psylocke_rules_insert-web.pdf), p.1 (p.2 blank); Rules Reference v1.8; printed cards and starter-deck card photographed in the [Psylocke card gallery](https://hallofheroeslcg.com/psylocke-betsy-braddock/).

## Insert rules

<!-- MC41 insert p.1; nonverbatim implementation summary -->

Player side schemes are played during their controller's turn by paying their cost and placing their starting threat. Their collective limit is one for games starting with one or two players, two otherwise. If exceeded, the first player chooses schemes to discard until within the limit. Defeated schemes with victory go into the victory display. Piercing removes tough before attack damage; ranged ignores retaliate. Team-up requires an eligible identity when building a deck and both named characters in play when playing the event.

The insert explains that Psylocke's weapon faces determine her bonuses and event effects. Resource abilities can flip the weapons while paying for an event, before its effects resolve.

## Rules Reference interactions

These are nonverbatim implementation summaries; the linked glossary entries are canonical.

- [Flip and double-sided cards](glossary-d-h.md): swapping the two upgrade faces preserves exhaustion, counters and attachments. Only the face currently showing supplies its characteristics and abilities. Permanent weapons are put into play during setup and are excluded when determining deck size.
- [Basic powers and attacks](glossary-a-c.md): Psylocke's optional interrupt occurs before resolving the chosen basic power. Recompute the affected stat and basic-attack keywords after the flip. A stunned basic attack or confused basic thwart is replaced by removing the status; this does not count as using that basic power (also the Quicksilver FAQ).
- [For each](glossary-d-h.md): Mental Detection combines its additional threat removal into one instance. Flurry of Blades and Telepathic Suggestion make individual choices and resolve each before asking the next. The same target may be selected repeatedly if still eligible. An attack event is one attack despite multiple damage instances; damage bonuses apply to each independent instance.
- [Wild resources](glossary-s-z.md): outside generating resources to pay a cost, wild remains wild. It does not count as mental for Chimera or Telekinetic Dragon, or match another named type for Psionic Illusion. Wild itself is a resource type that may be named.
- [Restricted](glossary-p-r.md): only the Katana face has restricted. Permanent prevents choosing either Psi-Energy card for a discard; flipping into an excess of restricted cards can require discarding another restricted card.

## Published official correspondence

FFG Game Rules Specialist Alex Werner's answers are preserved in the [post-1.5 collection](https://hallofheroeslcg.com/latest-ffg-rulings-post-rrg-1-5/):

- Ruling 95, October 5, 2023: Flurry is one attack; each distinct target retaliates once. Psionic Illusion checks each newly selected target, not repeated selections of a previously checked target.
- Ruling 197, May 30, 2024: Flurry also attacks enemies chosen only for confusion, so those enemies retaliate.
- Ruling 215, June 20, 2024: overkill damage to a villain qualifies for Upside the Head and Cypher's response.
- Ruling 224, July 11, 2024: Float Like a Butterfly checks confusion at attack initiation; confusing the target during that attack does not enable it retroactively.

The [post-1.6 collection](https://hallofheroeslcg.com/latest-ffg-rulings-post-rrg-1-6/) adds:

- April 17, 2025: Betsy's player reveals Body Swapped even when another player draws it.
- July 25, 2025: Soaring Hearts requires Angel's Angel face; Archangel and alter-ego faces do not satisfy that named character.
- September 25, 2025: Directed Force works with Relentless Assault's conditional overkill. Against Marked, it can trigger when the marked target is selected later in a multiple-target attack, and boosts only the damage against that target.

## Printed-card checks

The [Captain Britain card](https://hallofheroeslcg.com/wp-content/uploads/2023/10/41012.jpg) has 3 THW and 3 ATK, each with **two** consequential damage. His reduction makes that one after thwarting a side scheme or attacking a minion. [Pete Wisdom](https://hallofheroeslcg.com/wp-content/uploads/2023/10/41018.jpg) has 3 THW with **two** consequential damage and 2 ATK with one. Angel, Cypher and the Leadership Domino have one consequential damage for each basic power.

[Interdimensional Plunder](https://hallofheroeslcg.com/wp-content/uploads/2023/10/41027.jpg) starts with a flat **2** threat, not 2 per player, and has a crisis icon. Chimera has 1 SCH, 1 ATK, 5 HP and three boost icons. No Psylocke-specific erratum appears in the local RR v1.8 FAQ/errata transcription.

## Starter deck

The [printed deck card](https://hallofheroeslcg.com/wp-content/uploads/2023/10/photo-oct-07-2023-4-42-12-pm.jpg) lists seventeen signature cards, twelve Justice cards and thirteen Basic cards. After the two permanent weapons enter play, forty cards remain in the deck.

Signature: Psi-Knife/Psi-Katana (2), Angel, Flurry of Blades (3), Mental Detection (3), Psionic Redirect (2), Telepathic Suggestion (2), Training Regimen, Martial Arts Training, Psionic Training, Weapons Training.

Justice: Captain Britain, Cypher, Concussive Blow (3), Upside the Head (3), Lay the Trap, Float Like a Butterfly (3).

Basic: Pete Wisdom, Directed Force (3), Soaring Hearts, The Power of the Mind (3), IPAC, X-Bunker, Telepathy (3).

Obligation: Body Swapped. Nemesis: Chimera, Interdimensional Plunder, Psionic Illusion, Telekinetic Dragon (2).

## Rulings (from the project owner, 2026-11-01)

The owner directed: “Search online and do what is most likely to follow the rules.”

**Implementation inference:** Psionic Illusion redirects the currently chosen target, including a Flurry confusion-only choice. Each later “choose” instruction remains a fresh choice, evaluated against the updated game state. Once a failed check discards Illusion, it does not redirect those subsequent choices. This follows the official distinct-target ruling and RR's independently resolved “for each” choices. No published example explicitly covers a failed check followed by another selection of the original enemy; this interpretation preserves every remaining choice instruction rather than imposing a continuing replacement effect after the attachment leaves play.
