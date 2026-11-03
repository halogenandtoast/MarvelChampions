# Civil War leaders and encounter cards

The native implementation covers the six leaders' 24 stages, 21 attachment faces, 17 treacheries, six leader side schemes, and twenty active main-scheme faces across Civil War and Synthezoid Smackdown. Unregistered Super and Government Contractor are also implemented because Hunting Rebel Heroes depends on them. The four Civil War scenarios now have native cooperative and competitive construction; see [scenario setup](scenario-civil-war.md). The dedicated competitive creation/lobby flow is implemented and covered by browser and live API checks. Final Civil War verification passes. She-Hulk and Vision now have native cooperative and competitive registration; final Synthezoid verification is tracked in the [acceptance audit](synthezoid-completion-audit.md).

## Leaders and combat

Normal games use stages I–II; expert games use III–IV. Advancing to the final stage deals each player on that board an encounter card and prevents further damage to the leader for the phase. In competitive play, final defeat records the board's result and retains the leader through the opposing team's reply.

The handlers cover Iron Man's capped Tech-based SCH, Captain Marvel's defeat counters, Captain America's Shield response, Spider-Woman's Finesse attack bonus, She-Hulk's form-change damage, and Vision's mass-form flip after the threat step. Vision's form flip is a Forced Response in a shared window after step 1 of the villain phase, so it can be ordered with main-scheme responses. In competitive games, only the active board participates in this phase window. Iron Man's setup search and shuffle explicitly use his encounter deck even when the opponent makes the choice.

Retaliate shares the attack's Forced Response occurrence with encounter responses. Temporary retaliate from Gauntlets of Hercules lasts through attack completion. Defense response windows and delayed defense damage join that occurrence, preserving forced-before-optional timing. Riposte can be ordered alongside an enemy's Forced Response. Cap's Shield does not return when an attack deals no damage.

ATK-based event damage is tracked separately from basic attacks and flat damage. Counter-Punch and basic-attack overkill can transfer the Shield. As One! counts only when the hero contributes ATK through its exhaustion payment.

## Attachments and damage

Iron Man's five equipment cards supply their printed stats, keywords, traits, tough statuses, resource payments, and boost effects. Mark V Armor's “would … instead” text is implemented as a damage replacement despite its Forced Response heading. Damage surviving prevention goes entirely onto the Armor; at five or more damage it is discarded without spilling excess onto Iron Man. Tough resolves first. No specific ruling changing that heading was found.

Cosmic Flight caps attack damage above three after prevention. It remains attached when tough prevents the damage, and a second copy remains after the first caps the attack. Interrupt windows refresh from the pending damage so later interrupts see the updated amount. Contaminant Immunity replaces a possible incoming stun or confusion with its discard and a tough status. Impossible or redundant statuses do not open its interrupt window; narration follows the replacement opportunity.

Mass Increase responds to hero, ally, and leader attacks while Dense is attached. It stuns the attacker and prevents the attack's remaining damage, including other targets of one attack event. Its lasting effect survives the attachment's departure, persists across save/load, and expires when the attack ends.

Energy Channel adds and clears counters even if its counterattack has lost its target. Other counter additions, such as Alpha Flight Station, do not independently trigger its attack response. Finesse's three-counter limit applies to every placement source. Superhuman Strength and Super-Soldier Serum grant overkill through attack completion and then leave play.

The removable helmets require three resources of the same type. Solar Gem and Vision's Cape use their printed resource pairs and flip the existing mass-form attachment. Density Control changes the form before the activation and remains attached when the requested form is already showing. Mass-form flips are narrated.

## Treacheries

The form-dependent treacheries implement their individual discard, damage, attack, scheme, status, and counter effects. Repulsor Blast counts individual printed energy and wild resources. Energy Absorption heals from the updated counter total. Shield effects reattach the actual Shield and detach its previous host; Shield Toss damages controlled characters together.

Heroic Strike tracks the characters its particular attack actually damages and stuns surviving damaged characters after completion. Tough prevents its stun, while overkill can stun the identity behind a defeated ally. Venom Blast uses the skill counters actually removed for its attack: ranged at one, piercing at two, and overkill at three. Explicit attack tokens distinguish deferred and nested activations.

Solar Beam checks the form when its activation begins, after Density Control, and modifies actual ATK or SCH for that activation. Superdense Strike can choose an exhausted character when Dense's damage can affect it: the v1.8 Target entry allows a target affected by any part of an ability. Its independent Intangible flip resolves even when no character can exhaust. Phase Disruption offers the opponent a legal upgrade, discards instead of exhausting while Intangible is showing, and resolves its independent Dense flip without an upgrade target.

Gamma Slam discards a highest-cost ally or support and places threat equal to its printed cost. A reporting continuation follows actual placement through threat interrupts and replacements. No eligible card, a zero-cost discard, and complete prevention cause surge; partial prevention leaving one threat does not. Queue edits and narration still see the ordinary placement. Surge deals a facedown encounter card.

## Side schemes and competitive responses

All six side schemes start with three threat per player on their board. Stark Tower lets the opponent search that board's deck and discard pile, reveals the chosen attachment for that team, and shuffles that deck. Alpha Flight Station adds three energy. Fearless Determination returns the Shield and activates against the player who defeated it; its amplify icon remains active during its defeat effect.

Self-propelled Glide uses named references to Spider-Woman and Finesse and offers no zero-counter interrupt. These references survive saving and work when Finesse enters later. Its prevention still applies during cross-table leader attacks after every hero on its physical board is eliminated. Competitive forced responses across boards require a participating leader explicitly named by the current window, preserving ordinary board isolation.

Legal Practice prevents heroes and allies from removing its threat. After changing form in alter-ego, a player may exhaust their identity to remove three threat. An alter-ego thwart event is also legal. Just Passing Through confuses the player who defeated it, stunning instead if the confusion attempt adds no status card.

## Main schemes

The first-stage schemes start at zero threat for a solo board and gain hinder 2 per player for larger teams. All first stages have a seven-per-player target; the implemented second stages have a nine-per-player target. Cut Off Support and Open Rebellion place threat before a treachery resolves, with a separate once-per-phase allowance for each player. Homeland Security and Rallying Call give minions Guard. Enforce the Law and Going Underground give the enemy leader Steady; a single stun permits an attack, two cancel it and are both removed.

Public Outrage and Protect Secret Identities discard three cards from each player's own deck in the shared window after the threat step. No Going Back and Expose Overreach make allies enter exhausted. Secret Avengers and Mighty Avengers increase the enemy leader's actual ATK by one for a defended attack and two for an undefended attack, ending with the attack. A completed final stage loses a cooperative game; a competitive stage records its board's result for the opponent's reply.

Negative Zone Prison captures the defeated ally's original physical card after an enemy attack. A controlled encounter minion remains an encounter card beneath it. Captured cards leave their previous discard pile, remain facedown in the public view, and survive saving. Each capture reduces the target by one and immediately checks whether the existing threat completes the scheme. Ally defeat matchers consult the departing ally's snapshot after it leaves play, preserving its controller, traits, and board.

Guerilla Warfare responds after a status card is actually placed on its named enemy leader. Each of Steady's two stun cards counts separately. Redundant statuses and statuses blocked by Stalwart do not count. Contaminant Immunity's replacement tough counts once, without counting the prevented stun. During a cross-board leader attack, a status placed on a participating leader can trigger its physical board's Guerilla Warfare even while that board is inactive.

S.H.I.E.L.D. Recruits and Gathering Support search the encounter deck for minions; The Initiative and Neighborhood Protectors also search the discard pile for side schemes. The opponent assigns one eligible card to each player, with all selections made before dealing the cards facedown. The search pool is the original searched area, so a refill caused by dealing cannot extend a deck-only search into the discard pile. Searches, dealing, and shuffling stay on the receiving team's encounter deck. An interrupted search preserves its assignments through saving.

Superhero Jailbreak discards one encounter card for each player before putting the discarded minions into play. The first player chooses both discard order and minion-entry order under the v1.8 “Each Player” entry. Decks refill between different players' discards. Entry does not reveal the minions, so surge does not resolve; Quickstrike still applies. If a refill causes the same physical minion to be discarded twice, putting it into play once does not create a second copy or reassign an already-present minion.

Searched facedown cards open the ordinary would-be-dealt window. Aerial Recon can prevent a searched card's deal, leaving it in the searched area. Its cancellation also recognizes an explicitly selected encounter deck.

Hunting Rebel Heroes gives heroes Unregistered and responds when another revealed card grants that trait to heroes, including heroes who were already Unregistered. Each Unregistered Super remains in its revealing player's play area, grants the trait to that identity, and contributes its printed acceleration icon. Only its owner can use the alter-ego action, paying an identity-specific card from hand as a cost. Government Contractor starts with three fixed threat plus hinder 1 per player and grants every identity the trait.

Trait-granting reveals record their affected heroes by the revealed card's physical ID. This avoids unrelated forced prompts, preserves independent copies, and excludes alter-egos and the opposite board. Putting Government Contractor into play without revealing it grants the trait without triggering Hunting Rebel Heroes. Saved main schemes load both the former optional leader metadata and the newer record metadata. Jailbreak's entry-order choices display the corresponding discarded minion beside its recipient.

## Rules checked

The local v1.8 entries for Target, Interrupt, Replacement Effect, Damage, Tough, and Retaliate govern these interactions. Searches for newer card-specific rulings found no exception to the defensive or targeting rules above.

[FFG rulings collected by Hall of Heroes](https://hallofheroeslcg.com/latest-ffg-rulings-post-rrg-1-7/), checked November 3, 2026:

- December 17, 2025: ATK-based attack abilities can transfer Cap's Shield; flat damage abilities cannot.
- January 11, 2026: ATK overkill can transfer the Shield. Captain Marvel can gain two counters by defeating an ally and identity in one overkill attack. Opposing Civil War factions' modular sets cannot be mixed in cooperative play.
- January 26 and June 2, 2026: Energy Channel always adds its counter and clears four or more counters even after the attacker leaves play; its attempted counterattack then has no target.

The official [Rules Reference update](https://www.fantasyflightgames.com/mission-updates/) explains retaliate's Forced Response timing. Competitive leader attacks and reply phases are documented in [the engine notes](encounter-civil-war-engine.md).

## Handler verification before scenario integration

The full unit run passes 8,212 examples; the focused Civil War run passes 287. The selected Rhino, Klaw, and Risky Business smoke run passes 25 examples with Spider-Man and Black Panther, including the searched-card deal window and duplicate-entry guard. Coverage includes every implemented main-scheme face, captured player and encounter cards, hidden public data, immediate completion after a target reduction, status prevention and replacement, legacy saved schemes, cross-board responses, and the leader attachment and treachery cases described above.
