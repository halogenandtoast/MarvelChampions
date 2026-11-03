# Civil War engine boundaries

Source: Civil War rulebook, pages 14–17 and FAQ, and Synthezoid Smackdown rulesheet. The official source URLs are recorded in `README.md`.

Competitive games have two independent boards. Registration acts first, followed by resistance, in both the player and villain phases. Each team has its own first-player token and uses its original team size for per-player values. Each team builds the encounter scenario played by the opposing team: its friendly leader therefore occupies the other board.

Ordinary selectors, abilities, continuous effects, and phase events remain on their own board. Explicit references can cross the table. Unique friendly cards are checked only against teammates and their friendly leader. Enemy cards do not prevent friendly unique cards from entering play.

Private searches belong to the player making the decision. Opponents and spectators must not receive the searched cards or their card-labelled choices. The original team roster is retained after elimination so pending enemy-team decisions still have an owner.

The initial implementation provides typed team setup, independent first-player tokens, board boundaries, save/load metadata, public team summaries, and native definitions for 238 encounter card faces. Definitions alone do not make the scenarios playable; scenario registration waits for their rules handlers and tests.

Validation: 41 team-boundary examples, 2,144 hero examples, and 70 targeted encounter/public-view regression examples pass. Competitive round sequencing, equal-phase victory resolution, and elimination boundaries are implemented and covered by 46 team examples. Individual Civil War card behavior remains subsequent implementation work.

## Leader attacks

A leader attacking another leader ignores guard and allows no declared defender. It receives its normal boost card from its own encounter deck. The defending leader's team's first player resolves the boost as “you”; the previous active player is restored after the whole activation. Stun cancels the attack normally. Damage, tough, retaliate, ranged, and attack windows use the ordinary attack pipeline. A newly triggered activation waits for the current activation and its responses to finish.

The competitive FAQ confirms that leader attacks trigger Energy Channel. The official [Civil War FAQ](https://images-cdn.fantasyflightgames.com/filer_public/22/9f/229f60bc-eb77-493d-b0d4-132608bd4ba4/mc56_rulebook-web_1.pdf) and [Synthezoid Smackdown rulesheet](https://images-cdn.fantasyflightgames.com/filer_public/02/6c/026cbed2-a8e6-402d-aa4e-9afcae102b68/mc57_rulebook-web.pdf) were checked alongside the local Rules Reference activation timing.

The leader and signature-attachment implementation, including current FFG clarifications, is documented in [Civil War leaders](encounter-civil-war-leaders.md).

## Main-scheme display

The public view supplies the current target threat and printed acceleration resolved for the scheme's own team. The client uses these values so competitive games do not count opponents as additional players, and target modifiers such as Under Surveillance appear on the board. Target reductions are applied after increases and cannot reduce the target below zero. Acceleration tokens remain separately displayed. A setup question selects the A face of an explicitly stored B-face code, while preserving the legacy unsuffixed-code convention.

## Finishing a board and completing its phase

A finished board takes no further player turns. The current turn closes through its ordinary end-of-turn timing, queued teammates' turns are skipped, and the team completes readying and hand refill before the opposing team starts its reply. Finished players are not offered optional end-of-phase discards; mandatory hand-limit discards still apply. Skipped turns are not narrated as turns that began.

Villain-phase work is queried when each step actually runs. A main scheme completed by the threat step stops that board's activations, encounter-card deals, and reveals. Finishing during an activation also drops remaining phase activations and their queued minion-order prompts. This does not suppress explicit cross-board leader effects or required enemy-team decisions. Hazard icons are counted at the dealing step, after activations. A board that was already finished enters no new villain-phase mechanics, while the phase clock still permits the opposing reply.

Validation: the full unit suite passes 8,215 examples and the focused Civil War suite passes 290. The selected Rhino, Klaw, and Risky Business smoke run passes 25 games. Regressions cover a four-player winning attack with an unplayed teammate, cleanup on the finished board, main-scheme completion before activations, and a final leader defeated by Energy Barrier during a minion attack.


## Minions attacking leaders

Explicit minion attacks against enemies now use the ordinary minion activation and attack steps. The current attack is stored from its declaration through its end, so attack interrupts can grant keywords or prevent damage and then expire normally. Enemy targets can be on the opposite board and do not prompt a player to defend. Damage and retaliate use the shared character-attack pipeline. Villainous boosts and any already attached boost cards resolve normally; the nominated player resolves boost “you,” and the previous active player is restored afterward. This path also serves existing Moondragon and Headpool effects. The legacy continuation message remains readable for older saves.

## Creation flow requirements

Civil War has a dedicated entry and creation flow for its two opposing teams. Registration and Resistance each require at least one hero seat; the cooperative single-team scenario picker is not the competitive creation path. Hero seats and controlling accounts are separate: a single person may occupy and control seats on both teams, including a two-seat game played entirely by that person. Multiple accounts may instead share the table. The lobby must show team membership, seat ownership and deck selection explicitly, retain both teams through seating and reconnects, and validate team rosters before dealing. Account count must never be used as the multiplayer requirement.

## Defeated minions becoming boost cards

Dangerous Recruits chooses the enemy leader as the defeated minion's destination. Its interrupt records that destination; the minion's printed When Defeated effects and would-leave-play interrupts still resolve before departure, as required by Rules Reference v1.8, page 48. The departure preserves the physical card ID, discards the host's upgrades, attachments, tucked cards and stored boosts, and gives only the host to the leader. An interrupt cancelling the defeat also clears this pending destination. Effects on the other team's board do not trigger the scheme.

A player-backed minion, including an Ultron drone or an ally treated as a minion, keeps its original player card and owner in saved game state while represented as a boost card. Its printed boost icons are counted normally, without activating player-card text, and it returns to its owner's discard pile when discarded. This applies the printed destination instruction with the ordinary ownership and boost-discard rules (Rules Reference v1.8, pages 11 and 31). A search of recent FFG rulings found no specific Dangerous Recruits exception.

Validation: Dangerous Recruits is selectable with all five card faces implemented. The focused run passes 387 Civil War examples and 167 coverage examples. The full unit suite passes 8,316 examples; randomized Rhino and Klaw checks with this modular set pass with two seeds. Regressions cover printed defeat effects, attachment and stored-boost cleanup, owner-preserving save/load for drones, board boundaries, Weapons Runner returning from boost, and both interrupt orders when Loki cancels his defeat.

## Martial Law

S.H.I.E.L.D. Soldier takes the topmost S.H.I.E.L.D. minion from the revealing player's encounter discard pile. This puts the card into play without revealing it, so another Soldier's When Revealed does not chain. Its boost likewise puts that physical card into play without revealing it.

Rapid Response discards through the first minion, including that matching minion, then reads its out-of-play ATK or SCH. Reading these values constructs no in-play entity and applies no in-play modifiers. Its hero damage uses the normal non-attack damage interrupt windows; its alter-ego threat uses the revealing player's main scheme. With no discarded minion there is no further effect. This follows the ordinary discard, in-play/out-of-play, and put-into-play rules in Rules Reference v1.8; a recent rulings search found no specific exception for this card.

S.H.I.E.L.D. Patrol's guard is a continuous modifier for minions on its own board, including minions entering after the scheme. Leaving play or blanking the scheme ends that modifier. Its hinder and amplify use the ordinary keyword and icon handling.

Validation: Martial Law is selectable with all three card faces implemented (five physical cards). Thirteen behavior examples cover discard order, no reveal chaining, boost entry, separate encounter piles, both identity forms, tough and stun, no matching minion, out-of-play stats, hinder, amplify, continuous guard, board boundaries, and blanking. The focused run passes 400 Civil War examples and 168 coverage examples; the full unit suite passes 8,330 examples. Randomized Rhino, Klaw, and Ultron checks with Martial Law pass with two seeds.

## Cape-Killer

Cape-Killer's attack interrupt checks the character it is attacking when the attack begins. An Unregistered identity grants +2 ATK to that attack through the ordinary stored attack change; the bonus expires with the attack. A direct attack against an ally does not qualify, while an ally defending an attack that already began against an Unregistered identity does not undo the bonus (Rules Reference v1.8, page 8).

Bring Them In exhausts ready Unregistered identities on the revealing player's board. If none can be exhausted, including when all are already exhausted, its otherwise search resolves. Exhausting at least one identity prevents that fallback (Rules Reference v1.8, page 31). The opponent privately chooses a matching Unregistered Super from the receiving team's deck and discard; the card is dealt facedown without revealing it, and that team's deck is shuffled. With no match, no empty question is asked. The first player makes that choice in cooperative play. A recent rulings search found no specific exception for these cards.

Arrest Order deals only the topmost S.H.I.E.L.D. minion in the defeating player's encounter discard pile as a facedown encounter card. It does not reveal or put that minion into play.

Validation: Cape-Killer is selectable with all four card faces implemented (five physical cards), including the previously implemented Unregistered Super. Fourteen new behavior examples cover Quickstrike, attack-only bonuses, direct ally attacks versus later defense, stun, full and partial exhaustion, searches from both piles, facedown deals, opponent choices, and board boundaries. The focused run passes 414 Civil War examples and 169 coverage examples. Randomized Rhino, Klaw, and Ultron checks with Cape-Killer pass with two seeds.

## Heroes for Hire

Colleen Wing discards an eligible ally controlled by the revealing player; Shang-Chi does the same for an upgrade. That player chooses while registered, and the enemy team chooses while the identity has Unregistered. Cards that cannot be discarded or cannot leave play through abilities are excluded. The existing opponent-choice helper supplies the first-player choice in cooperative play and the opposing first player in competitive play.

Both printings of Bounty Hunting use the same handler. A registered identity chooses its hand discard; an Unregistered identity uses the engine's stored-generator random discard. Threat counts printed resource icons on the discarded card, including both icons on a double-resource card, without evaluating resource-generating abilities. Threat goes to the revealing player's main scheme through the normal threat-placement windows. An empty hand produces no discard question or threat.

System Shock cannot be chosen for this discard, but may be discarded at random, even when it is the only card in hand. This follows the [FFG ruling of August 8, 2024](https://hallofheroeslcg.com/latest-ffg-rulings-post-rrg-1-6/). Regression tests exercise both paths and restore the saved game, queue, and generator to verify that a random discard repeats.

The Heroes for Hire side scheme starts with three threat plus hinder 1 per player and its hazard icon. Its defeat effect gives the opposing team the Unregistered Super search for the defeating player, using only that player's team's encounter piles and dealing the selected card facedown.

Validation: Heroes for Hire is selectable with all five card faces implemented. The same Bounty Hunting handler also implements its Paladin printing. Twenty-four new behavior examples cover both discard choosers, missing and protected targets, single and double resource icons, conditional resource generation, empty hands, chosen versus random System Shock discards, generator-preserving reload, hinder, hazard, and the defeat search. The focused run passes 438 Civil War examples and 170 coverage examples. Randomized Rhino, Klaw, and Ultron checks with Heroes for Hire pass with two seeds.

## Paladin

Paladin uses the normal toughness and Quickstrike timing, then discards a card from the revealing player's hand. The player chooses unless their identity is Unregistered, in which case the existing stored-generator random discard applies.

Paladin's Pistol attaches to the encounter Paladin on the revealing player's board, including that same encounter card controlled as an ally. Otherwise it attaches to the enemy leader. Attachment restrictions are checked before selecting that branch; with no legal host, the attachment is discarded without an empty question. The referential-ability and control rules retain the identity of the encounter Paladin when its type changes (Rules Reference v1.8, pages 31 and 36).

The pistol supplies +2 ATK while attached. Its attack interrupt grants ranged and overkill only to the current attack, with the attachment discarded after that attack resolves. It works for minion attacks, leader attacks across boards, and attacks by the controlled ally. A scheme activation or an attack replaced by stun does not spend it. Enemy attacks use their stored attack keywords and end step; the ally's queued landing step supplies keyword flags and completes its damage, retaliation, attack windows, and consequential damage before the scheduled discard.

The viewer's initial “Attack to Paladin” typo was corrected to “Attach to Paladin” after inspecting the existing local printed image. The generator preserves that correction without downloading any image.

Validation: Paladin is selectable with all four card faces implemented (five physical cards), including the previously implemented Bounty Hunting and Government Contractor. Sixteen new behavior examples cover toughness, Quickstrike, chosen and random discards, both host choices and attachment restrictions, board boundaries, one-attack keywords and expiry, ranged versus retaliate, overkill past an ally, scheme and stun preservation, cross-board leader attacks, and control changes before or after attachment. The focused run passes 454 Civil War examples and 171 coverage examples. Randomized Rhino, Klaw, and Ultron checks with Paladin pass with two seeds. All eight Registration modular sets in the Civil War box are now implemented and selectable; Synthezoid Smackdown's additional modules remain separate work.
