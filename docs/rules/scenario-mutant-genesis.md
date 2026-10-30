# Mutant Genesis: scenarios and modular sets

Source: [official MC32 campaign expansion rulebook](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc32_mutant_genesis_rulebook_v5-compressed.pdf), 24 pages, fetched 2026-10-30. This file is a **non-normative implementation summary**; the box rulebook, current Rules Reference v1.8 and its errata are authoritative. The printed card texts are available in `frontend/public/data/cards.json`, pack `mut_gen`.

## Keywords and current rules
<!-- MG rulebook p.3; RR v1.8 pp.42–44 -->

The box uses piercing, permanent, steady, stalwart, teamwork, villainous and victory. All except teamwork already have engine counterparts. Toughness grants tough on entry; gaining toughness after entry does not itself grant tough.

**Teamwork update:** The printed box rulebook says all matching teamwork minions activate after an entering teamwork minion engages. RR v1.8 changes this to only the entering minion activating if another minion sharing its specified trait is in play. Resolve teamwork after that minion's When Revealed abilities. Changing engagement of an already-in-play minion does not retrigger teamwork. See [current Teamwork entry](glossary-s-z.md#teamwork-trait).

## Sabretooth
<!-- MG rulebook p.7 -->

Standard uses Sabretooth I/II; Expert II/III. Main schemes are Stalked by Sabretooth then The Injured Senator. Recommended modulars are Brotherhood and Mystique, both replaceable outside campaign-specific requirements.

Find the Senator begins in play with Robert Kelly attached, in play under no player's control; his text is blank while attached. Defeating that scheme detaches Kelly, gives him to the first player, advances the main scheme, and flips the scheme into Protect the Senator. Kelly thereafter follows the first player and does not count against the ally limit. Undefended enemy attack damage against his controller goes to Kelly. His departure from play loses the scenario. Player card effects cannot heal him, but Medical Emergency's encounter effect can.

After villain-phase threat placement, Stalked by Sabretooth deals Kelly two damage, increasing to three at six threat per player. The second main scheme loses the game when completed. Sabretooth heals after each completed activation using boost icons of the discarded encounter card; an attack replaced by stun or scheme replaced by confusion is not an activation.

The rulebook permits optional setup damage to Kelly to tune difficulty. Recommendations are zero for skirmish, one standard, two expert, three heroic; those are recommendations, not mandatory setup.

## Mystique modular set
<!-- MG rulebook p.7 -->

Infiltration and Shapeshifter Surprise shuffle into the resolving player's deck and surge. Drawing one counts toward cards drawn: it stays in hand and is not replaced by a player card or dealt as a facedown encounter card. Their forced responses trigger whenever they enter the hand, including effects other than drawing. They can be discarded as any other card in hand when instructed or at the end of the player phase. Discarding them from a player's deck/hand puts them in the encounter discard pile. They supply no printed resources.

Mystique's attack and scheme values equal the villain's current values. Players cannot attack the villain while she is in play.

## Project Wideawake
<!-- MG rulebook p.9 -->

Standard uses Sentinel I/II; Expert II/III. Zero Tolerance is required, Sentinels recommended and replaceable. Set the four Captive allies aside. Reveal Operation Zero Tolerance and Mutants at the Mall. OZT becomes permanent while Night of the Sentinels is in play.

When Mutant Detected makes the villain and all engaged minions attack, resolve the villain first, then let the resolving player order the minions (RR v1.8 Activation, p.6).

OZT captures allies defeated by enemy attacks; reaching three more facedown cards than players loses the game. It does not capture allies defeated by consequential damage, other damage or discard effects. Main-scheme threat repeatedly converts five threat per player into one top-deck card from the first player placed facedown under OZT.

The optional setup difficulty tuning places player top-deck cards under OZT: recommendations zero/one/two/three for skirmish/standard/expert/heroic. They are not mandatory amounts.

Defeating Abduction Protocols puts one random set-aside Captive ally into play under the defeating player's control. Use stored game randomness. Every captive's entry must be recorded for campaign rewards even if the ally later leaves play.

**Mutants at the Mall errata:** Discard only other **ally** versions of Jubilee, not every card sharing her name. See [RR errata](faq-errata.md#mutants-at-the-mall-88a).

## Master Mold
<!-- MG rulebook p.11 -->

Standard uses Master Mold I/II; Expert II/III. Sentinels is required, Zero Tolerance recommended and replaceable. Main schemes are The Sentinel Factory and Master Mold's Agenda. The printed setup puts the Magneto ally (32172b) under the first player's control in standalone and campaign play. The preceding campaign side-scheme result does not disable Magneto's printed setup.

When Master Mold schemes, discard until a Sentinel minion is found and put it into play engaged with the activating player before threat is placed. Do not give the normal boost card for this activation; already-dealt boost cards remain subject to the usual rules. A confused scheme does not run this interrupt. The main schemes grant Sentinel minions guard and place a Sentinel minion engaged with each player on reveal. Master Mold's Agenda first shuffles the encounter discard pile into its deck.

## Mansion Attack
<!-- MG rulebook p.15 -->

Four different villains form a random deck: Avalanche, Blob, Pyro and Toad, A sides standard/B sides expert. Only one villain is in play. Brotherhood is required and Mystique recommended/replaceable. The four location schemes also form a random ordered deck behind The Brotherhood Strikes. Their A faces are deliberately indistinguishable; do not expose the order to players.

The victory target is two defeated villains standard or three expert (one skirmish/four heroic). Save the School handles defeated villains, deals each player a facedown encounter card before revealing the next villain, and replaces a same-title minion by the new villain activating against the minion's former engaged player. A newly appearing villain does not inherit the previous character's damage, status cards or attachments.

The Brotherhood Strikes itself enters the victory display. Completing two location schemes loses the game, hence the printed condition of three main schemes total in that display. Location modifiers apply to all matching characters, including enemies. The Basketball Court grants toughness; it does not give every existing ally/minion a tough status card immediately.

## Magneto
<!-- MG rulebook p.17 -->

Standard uses Magneto I/II; Expert II/III. Acolytes is recommended/replaceable. Orbital Decay begins set aside, Boarding Party revealed. The side-scheme chain prevents more than six, twelve and eighteen sustained damage per player to Magneto until those objectives are defeated. These limits cap **total sustained damage**, not individual damage packets or maximum hit points.

Defeating Boarding Party flips/reveals Sabotage Master Mold. Defeating Sabotage Master Mold reveals Orbital Decay and adds Sabotage to the victory display. Defeating Orbital Decay flips/reveals Physical Strain, which is permanent and removes steady from Magneto. The campaign side scheme Magneto's Fortress is independent of this chain.

**Magnet-counter errata:** Asteroid M, Factory Online and The Rule of Magnus all remove three magnet counters **before** discarding until/revealing a Magnetic card. This avoids a revealed Magnetic card recursively counting the three counters already spent. Cards that add magnets can trigger the threshold again after the corrected removal. See [RR errata](faq-errata.md#asteroid-m-141b).

Magnetic Bubble redirects an entire damage packet to itself, then discards at eight damage; excess does not spill onto Magneto. Magneto's Helmet/Armor removal requires a hero's **basic** attack, not an attack event or an ally attack. Their individual stun/confuse immunities do not grant blanket stalwart.

## Acolytes and Future Past
<!-- MG rulebook pp.3,5 -->

Zeal for the Cause resolves the defeat abilities of engaged Acolytes without defeating or discarding those minions. Their effects must therefore be callable separately from the defeat/removal pipeline. Teamwork uses the current rule above.

Future Past can be used as a standalone modular set. In campaign mode its cards form the separate shuffled Future Past deck. Campaign objectives shuffle its top card into the current encounter deck. Victory cards leave the campaign permanently at victory; surviving cards are recorded for later scenarios. See [campaign rules](campaign-mutant-genesis.md).

Nimrod can take no more than three actual damage across an entire phase, after prevention/tough. Healing during that phase does not restore the spent damage allowance. Reset that allowance at each phase boundary.

## Rulings (from the project owner, 2026-10-30)

- Use the current Rules Reference v1.8 Teamwork wording: only the newly entering minion activates. It supersedes the older printed campaign-box wording.

- Sentinel Mark VIII can attach a Sentinel attachment whose normal attach instructions
  name the villain. Its printed stat-box ATK/SCH modifiers affect the attached Mark VIII;
  explicit text-box effects saying “the villain” continue to affect the villain. In
  particular, Gauntlet Beam grants the minion +1 ATK but grants the villain piercing and
  ranged; Learning A.I. grants the minion +1 SCH but grants the villain retaliate 1;
  Adaptive Armor grants the villain +8 hit points and grants the minion no hit points.
