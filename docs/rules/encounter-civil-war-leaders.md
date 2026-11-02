# Civil War leaders and encounter cards

The native implementation covers the six leaders' 24 stages, 21 attachment faces, 17 treacheries, and six leader side schemes across Civil War and Synthezoid Smackdown. Scenario registration remains pending main schemes, modular sets, and complete scenario setup.

## Leaders and combat

Normal games use stages I–II; expert games use III–IV. Advancing to the final stage deals each player on that board an encounter card and prevents further damage to the leader for the phase. In competitive play, final defeat records the board's result and retains the leader through the opposing team's reply.

The handlers cover Iron Man's capped Tech-based SCH, Captain Marvel's defeat counters, Captain America's Shield response, Spider-Woman's Finesse attack bonus, She-Hulk's form-change damage, and Vision's mass-form flip after the threat step. Iron Man's setup search and shuffle explicitly use his encounter deck even when the opponent makes the choice.

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

## Rules checked

The local v1.8 entries for Target, Interrupt, Replacement Effect, Damage, Tough, and Retaliate govern these interactions. Searches for newer card-specific rulings found no exception to the defensive or targeting rules above.

[FFG rulings collected by Hall of Heroes](https://hallofheroeslcg.com/latest-ffg-rulings-post-rrg-1-7/), checked November 3, 2026:

- December 17, 2025: ATK-based attack abilities can transfer Cap's Shield; flat damage abilities cannot.
- January 11, 2026: ATK overkill can transfer the Shield. Captain Marvel can gain two counters by defeating an ally and identity in one overkill attack. Opposing Civil War factions' modular sets cannot be mixed in cooperative play.
- January 26 and June 2, 2026: Energy Channel always adds its counter and clears four or more counters even after the attacker leaves play; its attempted counterattack then has no target.

The official [Rules Reference update](https://www.fantasyflightgames.com/mission-updates/) explains retaliate's Forced Response timing. Competitive leader attacks and reply phases are documented in [the engine notes](encounter-civil-war-engine.md).

## Verification

The full unit run passes 8,128 examples. The focused Civil War run passes 205 examples, including the additional Iron Man setup-deck regression. Coverage includes resource costs, every leader attachment and treachery, side-scheme defeat effects, actual threat prevention, attack attribution, saved defense choices, cross-board responses after elimination, and independent encounter-deck searches.
