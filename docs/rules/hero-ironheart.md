# Ironheart Hero Pack (MC29)

Sources: [official MC29 rulesheet, p.1](https://images-cdn.fantasyflightgames.com/filer_public/df/95/df950da6-c3b3-4edd-9f58-8d3bd192b755/mc29_ironheart_rulesheet.pdf#page=1)
(p.2 is blank); local MarvelCDB data, codes 29001–29040; printed card scans linked
below; Rules Reference v1.8. This document is an implementation summary, rather
than a verbatim transcription. Card text is in `frontend/public/data/cards.json`.

## Progressing identity cards

<!-- Ironheart rulesheet p.1 -->

Begin with version 1 of Riri Williams in play and versions 2 and 3 set aside.
Progress counters enable the hero-side abilities that swap to the next version.
All three identity cards share one hit point dial. A swap preserves damage and
transfers every game element on or attached to the identity, including counters,
tokens, status cards and attached player or encounter cards. If any version is
defeated, every version is defeated and the player is eliminated.

<!-- RR v1.8 "Swap", p.42 -->

Under the current [swap rules](glossary-s-z.md), swapping cards with the same title
does not count as entering or leaving play. The replacement retains the previous
card's ready or exhausted state and current hit point dial value. Level Up readies
Ironheart before swapping her. Each progressed hero retains the matching version
of Riri Williams on its alter-ego side.

## Requirement (resources)

<!-- Ironheart rulesheet p.1; RR v1.8 "Requirement (Resources)", p.37 -->

Each listed resource must be spent while paying the card's cost. Cost reductions
do not waive these required expenditures; ignoring the resource cost cannot
satisfy Requirement. Go All Out requires energy, Push Ahead requires mental, and
R&D Facility requires two mental resources.

## Printed card corrections

The [printed version 1 Ironheart](https://marvelcdb.com/bundles/cards/29001a.png)
has a unique icon before its title. The local MarvelCDB record incorrectly marks
29001a as non-unique. Its printed statistics are THW 1, ATK 2, DEF 3, 10 hit points
and hand size 4. [Riri Williams](https://marvelcdb.com/bundles/cards/29001b.png)
has REC 3, 10 hit points and hand size 6.

[Cyborg Tech](https://marvelcdb.com/bundles/cards/29031.png) has a printed +1 ATK
modifier in its left margin, in addition to the text-box ability granting +3 hit
points and retaliate 1. The text-only MarvelCDB description omits the margin
modifier. All three benefits must be represented.

## Go for Champions errata and FAQ

<!-- RR v1.8 Appendix V, "Ironheart Hero Pack", p.68 -->

The current errata makes removing Go for Champions from the game a cost of its
Hero Action. After that cost, every Champion character in play cannot take damage
until the end of the round. The local MarvelCDB text already includes this change.

<!-- RR v1.8 Appendix IV, "Ironheart Hero Pack", p.63 -->

A character protected by this effect cannot pay a cost requiring it to take
damage. The [FAQ and errata](faq-errata.md) and [Prevent](glossary-p-r.md) distinguish
taking damage from dealing damage; preventing damage can invalidate a taking
damage cost.

## Timing notes

Photon Beam's increased progress requires an actual enemy defeat caused by its
attack. Fly Over's increased progress requires actually removing the last threat
from the chosen scheme. These results must account for interrupts, prevention and
replacement effects. A villain stage can be defeated even though another stage
then enters play (RR "Defeat"). Stun or confuse cancels the entire corresponding
labeled attack or thwart ability, including its progress-counter effect (RR
"Labeled Abilities").

Tony Stark A.I. adds the selected card to hand; it does not draw it. An
[archived FFG response](https://hallofheroeslcg.com/official-ffg-rulings/)
confirms this distinction for encounter cards in a player's deck during the
Mysterio scenario. That archive predates Rules Reference v1.5; current rules and
errata take precedence.

## Starter deck

The local pack quantities for 29004–29027 form the forty-card Leadership starter
deck: fifteen signature cards, seventeen Leadership cards and eight basic cards.
There are no additional core Energy, Genius or Strength cards in this list.

The signature cards are Brawn, Fly Over ×2, Photon Beam ×3, New and Improved ×2,
Sector Scan, Stroke of Genius ×2, Ronnie Williams, Tony Stark A.I., Photon Blasters
and Propulsion Jets.

The Leadership cards are Cloud 9, Falcon, Patriot, Go All Out ×3, Push Ahead ×3,
Morale Boost ×3, R&D Facility ×3 and The Power of Leadership ×2. The basic cards
are Agent 13, Snowguard, Vivian, Go for Champions, Helicarrier and Ingenuity ×3.

A Minor Setback is the obligation. The five-card nemesis set contains Rule by
Force, Lucia von Bardas, Cyborg Tech and Political Retribution ×2. Neither belongs
in the forty-card player deck. Bombshell, Wasp and Pinpoint are additional
deck-building cards.

## Zzzax modular encounter set

The seven-card set contains Feedback Loop, Zzzax, Haywire ×2, Air Static and
Zzzap! ×2. It is separate from Ironheart's nemesis set and may be included when
constructing a scenario's encounter deck.

## Rulings (from the project owner, 2026-10-30)

- **Child Prodigy:** progressing to a new identity version permits another use
  that round, following the archived FFG ruling. Each version has its own
  once-per-round ability limit. Ordinary changes between hero and alter-ego do
  not reset that version's limit.
- **Bombshell and guard:** divide damage evenly among the enemies she can legally
  attack. A villain protected by guard is excluded from that division.
- **Bombshell and zero allocations:** only enemies allocated at least one damage
  count as attacked. Zero allocations open no attack windows and do not trigger
  retaliate. Tough and damage prevention do not change a positive allocation.
