# Nebula Hero Pack

## Sources

Official [Nebula rulesheet (MC22)](https://images-cdn.fantasyflightgames.com/filer_public/0b/0f/0b0f1475-6a14-4077-85e5-0353b097f94a/mc22_nebula_rulesheet.pdf), first page; Rules Reference v1.8 pp.62,67, transcribed in [faq-errata.md](faq-errata.md). Card data: local MarvelCDB pack `nebu`.

## Rulesheet rules (paraphrase)

The insert introduces Piercing, Stalwart and Team-Up. Piercing removes the target's tough statuses before an attack deals damage. Stalwart prevents both stun and confuse. A Team-Up card names two characters: the chosen identity must be one of them to include the card, and playing it requires both named characters as heroes or allies in play. Current definitions are in the Rules Reference glossary; follow those where later wording refines the insert.

The prebuilt deck is Nebula/Justice. The insert directs players to use the cards before the divider; the divider reverse, rather than the insert, lists deck quantities.

## Official FAQ and errata

RR v1.8 p.62: Old Rivals genuinely causes Gamora to attack. Her controller may use attack-triggered abilities. An attacking Gamora ally takes consequential damage, including when this treachery caused the attack.

RR v1.8 p.67: Eros resolves a separate minion choice and confuse for each mental resource paid. Old Rivals' Gamora hero/ally attack instruction is operative rules text. Its ATK is resolved against the player without exhausting Gamora. Cosmo's errata permits naming a card type and discarding from a player deck or encounter deck.

## Printed card corrections

Lethal Weapon (22030) has **+1 ATK** printed along its left edge. The local MarvelCDB JSON omits this modifier. Verified against [the printed card image](https://marvelcdb.com/bundles/cards/22030.png).

## Timing to preserve in implementation

Combat Protocols resolves the Technique Specials first, then discards the upgrades resolved in that manner. The passive abilities remain available during the Special sequence until the later discard instruction. Effects that put a Technique into play, including Combat Ready, do not play the card and therefore do not trigger Cybernetic Upgrades.

## Rulings (from the project owner, 2026-10-29)

Combat Protocols keeps a Technique when its Special cannot resolve or cannot change the game state. In particular, do not discard Unyielding Persistence for an attempted Special while Nebula already has tough, and do not discard a Technique whose Special has no eligible target. Track successful resolution rather than merely selected upgrades.

## Friendly Gamora attack

Old Rivals tells a Gamora hero or ally to resolve her ATK against the player, without exhausting. This is a genuine player-character attack: emit its attack timing windows, honor stun and tough, apply attack damage reduction and retaliate, and deal ally consequential damage after responses. It does not turn Gamora into an enemy or initiate an enemy activation. RR v1.8 “Defend, Defense” permits basic defense during an enemy attack; this friendly ATK resolution has no enemy-activation basic-defense step. An interrupt whose own damage trigger is met can still be played; defense-labeled abilities may be triggered outside an enemy attack without making the hero a defender (RR p.15).

## Printed starter deck verification

The [printed divider](https://hallofheroeslcg.com/wp-content/uploads/2021/09/nebula-starter-deck.jpg) has two copies of The Power of Justice and one Daughters of Thanos. The local MarvelCDB pack quantities instead report one and two respectively; starter construction follows the divider. The corrected starter has 40 cards.

## Referential abilities

RR v1.8 “Referential Ability” prioritizes cards associated with the same identity before other player/encounter cards sharing a title. Self-Preservation and Lethal Weapon therefore refer to Nebula's associated Gamora ally or nemesis minion, including that ally while treated as a minion. A separate Gamora hero does not take that priority. Old Rivals explicitly includes Gamora's hero and ally forms.

The [FFG clarification of June 25, 2026](https://hallofheroeslcg.com/latest-ffg-rulings-post-rrg-1-7/) distinguishes Old Rivals: its opening instruction specifically references the Gamora nemesis minion; its next instruction explicitly covers the hero or ally. A Beguiled ally is neither that specific minion nor currently an ally.
