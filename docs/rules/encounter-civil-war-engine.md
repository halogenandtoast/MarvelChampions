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
