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

## Atlanteans

Atlantean Guard discards the revealing player's top three cards. Atlanteans counts distinct card types actually discarded, placing that much threat on that player's main scheme in alter-ego form or dealing that much indirect damage in hero form. Its “for each” clause has no choose instruction and therefore creates one damage instance (Rules Reference v1.8, page 20); tough can prevent the whole packet assigned to a character.

Atlantis Attacks starts at three fixed threat plus hinder 1 per player. It discards the revealing player's top 1-per-player cards, using its own team's size in competition, and places one acceleration token on itself for each distinct type discarded. These tokens increase that board's villain-phase threat placement and leave with the side scheme.

The engine now stores the revealing identity on side schemes before resolving their reveal effects, including reveals of an already-present side scheme. Existing saves default this optional identity to unknown. This avoids depending on the active player when another seat reveals a card.

The shared player-deck discard callback now reports Shapeshifter cards that actually moved to the encounter discard pile. Those cards count toward Atlanteans' distinct types, but are not duplicated in the player's discard. Mysterio's replaced discards remain excluded because those cards are dealt as encounter cards instead of discarded.

Validation: Atlanteans is selectable with all three card faces implemented (five physical cards). Fourteen behavior examples cover player-deck targeting, distinct types, short decks, split indirect damage, tough, actual Shapeshifter discards versus Mysterio replacements, side-scheme acceleration and cleanup, four-seat team counts, legacy saves, and repeat reveals. The focused run passes 468 Civil War examples and 172 coverage examples. The full unit suite passes 8,402 examples on retry after an initial process segmentation fault before any test result. Randomized Rhino, Klaw, and Ultron checks with Atlanteans pass with two seeds.

## Defenders

Luke Cage and Jessica Jones try to give the friendly leader a stunned or confused status respectively, falling back to the revealing identity if no status is given. The post-status check handles an already full status allowance, stalwart, and a steady leader receiving its second card. Their boost abilities use the player resolving the boost and retain the callback through that boost's resolution. Cooperative games have no friendly leader, so the identity receives the status directly.

Protect the Innocent gives tough to the enemy leader and each minion on that board. Its surge condition checks whether the enemy leader was already tough before the effect. A tough friendly leader or minion on the other board does not satisfy that condition.

The two Street Defenders printings share Justice Like Lightning's activation handler because their rules text is identical. Each local minion activates against its engaged player, with an activation-order choice when needed. Stun or confusion resolves the activation instruction through its status replacement rather than causing the otherwise leader activation. If no minion can activate, the enemy leader activates without a newly dealt boost card. A villainous minion still receives its ordinary boost.

The Defenders shares S.H.I.E.L.D. Patrol's continuous guard handler and has its own printed acceleration icon. Guard applies to local minions and ends when the scheme leaves play or its text is blanked. The ordinary hinder handler places its additional threat. A search of recent FFG rulings found no card-specific exception; the [July 9, 2026 FFG ruling](https://hallofheroeslcg.com/latest-ffg-rulings-post-rrg-1-7/) also confirms that “enemy leader” means the villain when these modules are used outside a Civil War scenario.

Validation: Defenders is selectable with all five card faces implemented. Its Hell’s Kitchen printing of Street Defenders is also implemented. Twenty-six new behavior examples cover cooperative and competitive status targets, full status allowances, stalwart and steady, boost callbacks, tough and surge, board boundaries, both activation printings, status replacements, hinder, guard, and acceleration. The focused run passes 494 Civil War examples and 173 coverage examples. Randomized Rhino, Klaw, and Ultron checks with Defenders pass with two seeds.

## Hell's Kitchen

Daredevil's When Revealed choice belongs to the enemy team's first player, but its boost choice belongs to the player resolving the boost. Both choices contain only that player's ready identity and ready controlled allies. An exhausted character cannot be exhausted again (Rules Reference v1.8, page 19); with no eligible character, the effect finishes without an empty question.

Iron Fist attacks the friendly leader on the opposing board through the ordinary minion attack pipeline. In cooperative play there is no friendly leader, so Iron Fist activates against the revealing player, attacking a hero or scheming against an alter-ego. Its boost schedules the enemy leader's attack after the current activation, without dealing an additional boost card, as for Excessive Force. In cooperative play the boost has no friendly-leader target.

Resistance Fighter's normal attachment instruction lets the opposing team choose a local minion. If none can receive encounter attachments, it is discarded and deals a surge card. Its boost instead lets the resolving player choose a minion engaged with that player. Attaching consumes the physical boost card, so the same card becomes the attachment and is not also discarded when boosts clear. With no eligible engaged minion, the boost is discarded normally without surge. Only the attached minion gains four hit points and stalwart.

Defend Hell's Kitchen prevents all threat removal while a Defender minion is on its own board. The restriction is continuous: removing the last Defender ends it immediately, and a Defender on the other board does not impose it. Hinder and its hazard icon use the existing side-scheme engine. Street Defenders shares the tested activation handler used by Justice Like Lightning and the Defenders printing.

These effects follow the ordinary exhaustion, attachment, boost, activation and otherwise rules. A search of recent published FFG rulings found no card-specific exception for this set.

Validation: all five Hell's Kitchen card faces are implemented and the set is selectable. Fifteen new behavior examples cover exhaustion and choice ownership, no eligible characters, cooperative fallback activations, cross-board attacks, deferred no-boost attacks, attachment surge, physical boost-to-attachment movement, engagement restrictions, stalwart, hinder, continuous threat protection, and board boundaries. The focused run passes 509 Civil War behavior examples and 174 coverage examples. The compiled native registry now contains 125 of the 169 Civil War non-player faces (74%); this excludes Synthezoid and does not imply completion of competitive scenario setup or the creation UI.

Randomized Rhino, Klaw, and Ultron checks with Hell's Kitchen pass with two seeds each.

## Cloak and Dagger

Cloak lets the revealing player choose a scheme on their own board to receive two threat, then removes two threat from the friendly leader's main scheme on the other board in competitive play. Dagger heals the local enemy leader and lets the revealing player choose a controlled character to take two non-attack damage. These instructions resolve independently, including when the leader is already at full health. Character damage uses the ordinary damage windows.

Their boosts check for the partner minion on their own board. With a partner present, the physical boost card is revealed as an encounter card, resolving its When Revealed effect and leaving no discarded duplicate. Without the partner, Cloak places one threat on a chosen local scheme and Dagger deals one damage to a chosen controlled character.

Darkforce attaches to the local enemy leader and gives them tough. Its forced response uses the amount of threat the attached leader actually placed during its scheme activation, after threat interrupts, and removes that amount from the friendly leader's main scheme. It is then discarded, including in cooperative play where no friendly leader exists. Confusion replacing a scheme, a scheme placing zero threat, and a scheme placing threat on a signature side scheme do not spend it. This uses the existing scheme-activation receipt rather than reading the current threat total, which could include separate boost effects or later responses.

Lightforce heals three damage from the local enemy leader and deals three indirect damage to the revealing player. The side scheme uses hinder and, when defeated, heals and toughens the leader on its own board. A blanked scheme has no printed defeat effect. Healing is capped at maximum hit points under Rules Reference v1.8, page 22; the native healing pipeline enforces that cap. A search of recent published rulings found no card-specific exception for this set.

Validation: all five Cloak and Dagger card faces are implemented and the set is selectable. Seventeen behavior examples cover healing and damage, controlled-ally choices, opposite-board threat removal, partner detection and boost-to-reveal movement, confusion and zero-threat replacements, Darkforce's transferred amount and discard, signature side schemes, hinder, and a cross-board source defeating the side scheme. The focused run passes 526 Civil War behavior examples and 175 coverage examples. The compiled native registry contains 130 of the 169 Civil War non-player faces (77%).

The full unit suite passes 8,463 examples after the shared scheme-result change.

Randomized Rhino, Klaw, and Ultron checks with Cloak and Dagger pass with two seeds each.

## Namor

Namor enters with toughness, stalwart and villainous, and immediately activates against the revealing player. His activation uses that player's form and the ordinary villainous boost pipeline. Imperius Rex activates the local Namor when present, otherwise the local enemy leader; its boost gives the activating enemy another boost card, which is flipped and counted during the same activation, including for a villainous minion.

Neptune's Trident and Horn of Proteus attach to the local encounter Namor first, including Namor already controlled as an ally, otherwise to the local enemy leader. They respect restrictions on receiving encounter attachments. The Trident supplies +1 ATK and piercing to its host's attacks. The Horn supplies +1 SCH and gives its host tough when that enemy schemes. The minion engine now opens its existing “when this minion schemes” window after would-scheme interrupts and before boosts; an activation replaced by confusion does not open it. Each equipment card can be discarded through its printed Hero Action by paying its two specified resources. These use the ordinary attachment, resource-payment and keyword rules (Rules Reference v1.8, pages 8, 37 and 47).

Ruler of Atlantis continuously prevents damage to Atlantis minions on its own board. It does not protect other traits or the opposing board, and its protection ends when the scheme leaves play or is blanked. Its hinder and amplify use the existing side-scheme engine. A search of published rulings found no Namor-specific exception to these rules.

Validation: all five Namor card faces are implemented and the set is selectable. Nineteen behavior examples cover both revealing forms, toughness and stalwart, villainous boosts, equipment placement and bonuses, piercing, the Horn's minion and leader scheme interrupts, confusion, Hero Action resource costs, Imperius Rex's activation targets and extra boosts for both enemy types, continuous damage protection, hinder, and board boundaries. The focused run passes 545 Civil War behavior examples and 176 coverage examples. The compiled native registry contains 135 of the 169 Civil War non-player faces (80%).

The full unit suite passes 8,483 examples after adding the minion's when-scheme window.

Randomized Rhino, Klaw, and Ultron checks with Namor pass with two seeds each.

## Spider-Man modular set

Tangled Up attaches to the friendly leader on the opposite board when possible. Otherwise it attaches to the revealing player's hero; with no eligible leader or hero it is discarded. It replaces one attack, scheme, or thwart by its attached character, discarding itself instead. Status-card replacements resolve first, so a stun cancelling an attack does not also spend Tangled Up. Multiple copies replace one power at a time, with the player choosing the copy to discard. A paid attack or thwart event still pays its cost and is discarded, while its effect is replaced. Basic powers and Grapnel Launcher's extra basic thwart use the same replacement. Blanking suppresses the printed replacement.

Leader attacks use the generic would-attack window for any target, including another leader. Replacing an attack removes its queued attack windows and activation work, retaining cleanup. Forced abilities on a leader or its attachments also recognize that leader's explicitly named scheme windows across boards. This extends the existing cross-board attack permission without exposing unrelated abilities from the other board.

Spider-Man attacks each distinct character named by an attached Tangled Up once, then searches only if none of these attacks began. A prevented activation does not count as an attack; an attack whose damage is stopped still counts. The whole sequence waits until a current activation finishes. The enemy team's required search uses only the revealing team's encounter deck and discard pile, deals the selected physical card facedown, and shuffles that deck.

Interpretation of the competitive-area rule: Spider-Man's explicit reference to characters with the named Tangled Up attachment includes a friendly leader on the other board. This is treated as a specified cross-board target rather than an ordinary unrestricted character selector. Searches, boost-condition checks, and other ordinary selectors remain local. The Civil War competitive-area rule and the Rules Reference replacement rules were checked; a search of published rulings found no specific clarification for Spider-Man or Tangled Up.

Spectacular places two threat on the revealing player's main scheme and, in competitive play, removes two from the friendly leader's main scheme. Neighborhood Hero begins with two threat and moves up to two available threat from the friendly leader's main scheme. This is movement, using the existing direct movement steps rather than separate removal and placement abilities. If that move cannot at least partially resolve, its otherwise effect places two threat per player, including when a competitive source has zero threat. These choices follow the Move and Otherwise entries of Rules Reference v1.8, pages 30–31.

Validation: all four Spider-Man card faces are implemented (five physical cards) and the modular set is selectable. Twenty-one new behavior examples cover cooperative and competitive hosts, no eligible host, basic and event replacements, resource costs, status priority, multiple copies, boost conditions and physical-card movement, cross-board leader attacks and schemes, named tangled targets, prevented attacks and fallback searches, Grapnel Launcher, threat movement and partial/zero availability, and team boundaries. The focused run passes 566 Civil War behavior examples and 177 coverage examples. The compiled native registry contains 139 of the 169 Civil War non-player faces (82%).

The full unit suite passes 8,505 examples after the shared replacement and cross-board scheme-window changes.

Randomized Rhino, Klaw, and Ultron checks with the Spider-Man modular set pass with two seeds each.

## New and Secret Avengers: minions and side schemes

Falcon, Hercules, Goliath, Black Panther, and Spectrum use native Teamwork (Avenger). Their printed When Defeated effects use the shared physical minion-to-boost departure path. That path now recognizes the pending defeated departure after the printed effect starts, as well as the earlier defeat-interrupt case used by Dangerous Recruits. The minion stays in play through its printed defeat effect and would-leave interrupts, then becomes the local enemy leader's boost card with the same physical ID. Its attachments and previously stored boosts discard separately. Blanking suppresses the printed destination, and overlapping Dangerous Recruits and printed destinations do not duplicate the card or leave pending markers. This follows Rules Reference v1.8, page 48; the official [Civil War preview](https://drafts.fantasyflightgames.com/en/news/2025/6/26/civil-war-1/) also describes these minions becoming boost cards after defeat.

Falcon's boost confuses the resolving player, Hercules stuns them, and Goliath gives the local enemy leader tough. If the relevant status was already present, that physical boost card gains two boost icons. Black Panther exhausts the resolving player's ready upgrades, while Spectrum lets that player choose one ready controlled character to exhaust. Empty exhaustion choices finish without asking a question.

New Avengers shuffles only minions from its own encounter discard pile into its own deck. Secret Avengers lets the enemy team search its own board's top five encounter cards and give the selected physical card to the local leader as a facedown boost, then shuffles that deck. The side scheme's board determines the piles and leader even if an opposing player removes its last threat. Both side schemes start at three threat and use Hinder 1 per player; New Avengers also has a hazard icon.

Validation: 23 new behavior examples cover all five printed defeat destinations and later boost discard, each boost effect, Teamwork and board boundaries, physical identity, attachment and stored-boost cleanup, blanking, overlapping defeat effects, side-scheme threat and pile handling, and an opponent defeating Secret Avengers. The focused run passes 589 Civil War behavior examples and 177 coverage examples. These seven native faces do not yet make either modular set selectable: both Freedom Fighters printings and Switching Sides remain to be implemented.

The compiled native registry contains 146 of 169 Civil War non-player faces (86%). The full unit suite passes 8,528 examples after extending the shared defeated-minion destination handler.

Randomized Rhino, Klaw, and Ultron regression checks with Dangerous Recruits pass with two seeds each, exercising the existing caller of the shared defeat-to-boost path. The incomplete New and Secret Avengers sets are not used for randomized scenario checks yet.

## Freedom Fighters

Both printings resolve the printed boost abilities of local, unblanked minions. The revealed treachery includes all such minions on the revealing player's board and gains surge if there are none. Its boost includes only minions engaged with the player resolving that boost and does not gain surge when there are none. The resolving player chooses the order. Each later choice rechecks the remaining minions, so a minion leaving play during an earlier ability is not offered again.

Resolving this text does not flip the minion, apply its boost icons, or discard it as a boost card. The engine keeps a separate boost view of the physical minion. Its ordinary state continues to receive game messages, while boost callbacks use the instructed player as “you” without changing the real activation's player or other boost cards' interpretation. A callback shared by the ordinary and boost handlers resolves once. Multiple resolutions keep independent boost views, including Frost Giant's reactive effect through the current activation's damage. These scopes clear with that activation; with no activation they finish after the effect, leaving no watcher for a future attack.

Putting an already-present minion into play again does nothing, preserving its damage and engagement. Revealing an existing minion uses the existing in-play reveal path without a fresh entry. Shuffling a live minion invokes its ordinary leave-play windows and attachment cleanup before adding its physical card to the encounter deck, so it cannot remain in both places. Text referring to the activating enemy has no such target outside an activation, while the rest of that boost ability still resolves. No flip-triggered windows or extra boost-icon count are introduced by Freedom Fighters itself.

The scoped views are saved with an outstanding question and survive a Game JSON round trip. Older saves default to no scopes. The player override is confined to the boost view's queries and remains unset in stored game state. A search of published rulings found no specific Freedom Fighters exception; this implementation follows its instruction to resolve boost abilities and the ordinary ability, put-into-play, reveal, movement and activation rules.

Validation: 24 new behavior examples cover both printings, revealing and engaged-player scopes, blanking and surge, copied boost text versus When Revealed, no re-entry, physical shuffle and reveal, ordinary and boost-only callbacks, delayed and repeated reactive effects, no activating-enemy target outside an activation, extra boosts during a real activation, team boundaries, and current/older save compatibility. The focused run passes 613 Civil War behavior examples and 178 coverage examples; the full unit suite passes 8,553 examples. New Avengers is now selectable with all five faces implemented. Secret Avengers has four of five faces implemented, with Switching Sides remaining. The compiled registry contains 148 of 169 Civil War non-player faces (88%), including 71 of its 72 modular faces.

Randomized Rhino, Klaw, and Ultron checks with the complete New Avengers set pass with two seeds each.


## Switching Sides

Switching Sides discards an eligible ally controlled by the revealing player, including an encounter minion currently controlled as an ally. Its surge condition records the actual after-leaving-play window. An ally protected from discard or departure is not offered, and an ally that does not leave play causes surge. Discarding is not defeating.

In competitive play, eligible opposing players are offered payment individually, starting with that team's first player. Declining passes the offer to the next teammate. The amount is the ally's printed cost; spending resources equal to that number does not constitute paying the ally's play cost, so conditional aspect resources do not double. A zero printed cost permits free entry. A card without a printed payable cost cannot be claimed this way.

The returning ally keeps its physical card and original owner, with its controller changed to the payer. It leaves play before returning, so damage, statuses, exhaustion and attachments are cleared through ordinary departure and entry handling. Its later discard returns to its owner's discard pile. Player zones remove cards by physical ID when putting them into play, including when controller metadata has changed. Put-into-play restrictions and friendly uniqueness apply; play costs and play-only restrictions do not. These decisions follow the Rules Reference entries for discard, leaving play, ownership/control, cost and putting into play, plus Civil War's team and uniqueness rules.


Validation: fourteen behavior examples cover cooperative discard and surge, protected allies, optional opposing payment, multiple opposing seats, affordability and uniqueness, free allies, owner and physical identity, fresh entry state, subsequent owner discard, controlled encounter minions, conditional resource values, and attachment cleanup. The Civil War/coverage gate passes 805 examples before the final attachment example; that example passes in the targeted fourteen-example run and in the full unit run of 8,568 examples. The frontend production build passes. The viewer spelling correction for Switching Sides is preserved by the card generator.

Secret Avengers is now selectable, completing all sixteen Civil War modular sets and their seventy-two native card faces. The compiled registry contains 149 of 169 Civil War non-player faces (88%). The remaining twenty are sixteen setup faces and four Standard PvP faces. This card coverage does not imply finished scenario setup or multiplayer creation.

Randomized Rhino, Klaw and Ultron checks with Secret Avengers pass with two seeds each (three smoke examples).
