# Civil War scenario construction

Sources: the Civil War rulebook, Custom Scenario Creation (pages 4–6), Competitive Mode and FAQ; Rules Reference v1.8, Setup (Appendix II), Modes of Play, and Ownership and Control. The official rulebook URLs are listed in [README.md](README.md). The official [Civil War preview](https://drafts.fantasyflightgames.com/en/news/2025/6/26/civil-war-1/) also describes exchanging scenarios and the two Standard PvP printings. A search of recent published rulings found no exception to the ordinary Expert-set instruction when using Expert mode with competitive scenarios.

## Cooperative scenarios

Iron Man, Captain Marvel, Captain America and Spider-Woman have native scenario entries. Their recommended modular sets and main schemes follow the preconstructed scenarios in the rulebook. Normal mode uses stages I–II and Expert mode uses III–IV plus the Expert encounter set. Cooperative games use the ordinary Standard encounter set, with the existing alternate Standard/Expert options available.

A custom cooperative scenario chooses three or four different modular sets and a first- and second-stage main scheme from its leader's faction. Conventional modular sets are allowed; mixing Registration and Resistance modulars is prohibited. Setup starts on the selected main scheme's A face, finds the leader's own side scheme, flips to the gameplay face, places its initial threat and resolves its When Revealed effect. Stage 2 uses the selected second scheme on that board.

The initial leader then resolves its printed Setup or When Revealed ability exactly once. Most stage III leaders retain a Setup ability; Iron Man III has When Revealed instead. Captain America's Shield genuinely attaches and triggers tough. The second board's leader reveal is scoped to that board rather than being omitted because the first board already has a main scheme.

## Competitive scenarios

Competitive games require two or four hero seats, split evenly between Registration and Resistance. Account ownership is independent of seat count: the same account may control both teams. Each faction builds the scenario faced by its opponent. The Registration board therefore contains the Resistance leader, main scheme and modulars, and vice versa.

Each encounter deck contains its leader's ten-card encounter set, three or four chosen faction modules, that faction's five-card Standard PvP printing and the obligations of the heroes facing that deck. Expert mode adds the Expert set to each deck. The two PvP printings are constructed separately; alternate-printing definitions have no bulk-gather quantity, which must not suppress the receiving team's physical cards or merge both printings into one deck.

Choosing Sides is found during stage 1A setup and revealed on its own board. The corresponding leader reward pool is set aside for that leader's teammates, on the opposing board. Each pool has four physical, initially unowned player cards. Now It's Personal gives each eligible player two actual cards from their own leader's shared pool, preserving physical identity and assigning ownership when the cards are taken.

Main schemes with setup-time When Revealed searches can deal encounter cards before the first player turn. Physical inventory checks include these facedown cards rather than assuming every generated card remains in the encounter deck.

## Independent setup and ownership

Identity setup precedes leader creation. Temporary, team-specific piles keep each hero's obligation separate until its actual leader deck exists. Those cards and set-aside nemesis cards retain the correct deck as their physical home. Later shuffling or setting aside cards for a particular encounter deck records that home without overwriting an existing one. A transferred card can consequently return to its original encounter discard pile.

Pool-aspect setup runs independently for each team's encounter deck. Dreadpool and its set-aside cards belong only to a deck whose teammates use the Pool aspect. The native scenario configuration and a partially completed two-board setup survive a Game JSON round trip.

All sixteen Civil War modulars are registered in the encounter-card catalogue used for deck gathering, as well as having native handlers. Each modular contributes five distinct physical cards. This inventory check is separate from handler coverage: a handler registration alone does not prove that random play can draw the card.

## Validation

Forty-eight scenario and physical-inventory examples cover all four leaders in Normal and Expert cooperative play, all competitive leader pairings, two- and four-seat per-player values, independent piles and obligations, nemesis homes, unowned reward pools and actual reward claiming, setup-time facedown cards, Pool isolation, custom scheme advancement, configuration rejection and native save/reload. The Civil War/coverage gate passes 976 examples, or 981 when combined with the defense regressions. The full unit suite passes 8,739 examples.

Randomized competitive play exposed an automatic-retaliate actor bug when Black Panther defended for a teammate. The synthetic response was recorded against the active player instead of its resolving defender, so its per-window limit never registered. Automatic keyword responses now resolve as the player whose response window is being processed. A focused defense example verifies one retaliate and normal attack completion. The original failing four-seat Normal case passes with two seeds after this correction. A matrix with four workers passes 23 of 24 cases; the remaining four-seat Spider-Woman Normal case exceeds the question watchdog during turn-option calculation, then passes both seeds in isolation. The complete two-worker matrix also passes 23 of 24 cases: Spider-Woman Normal now passes, while Iron Man versus Captain America at four seats in Expert mode exceeds the same watchdog. That Expert case subsequently passes both seeds in isolation (126.8 seconds). A fully green matrix remains an outstanding verification condition; the evidence does not establish whether the timeouts are solely load-sensitive.

The dedicated multiplayer creation/lobby flow and final Civil War completion audit remain separate required work; native scenario registration alone does not establish that flow is complete.
