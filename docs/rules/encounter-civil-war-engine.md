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
