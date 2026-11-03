# Fear No Evil encounter implementation

Sources: [Fear No Evil rulebook](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc60_rulebook-web.pdf), supplied MC60 PDF, printed card scans bundled with this repository, and Rules Reference v1.8 under this directory. Ordinary player cards and existing hero nemesis sets are outside this work.

This expansion is in progress. Disasters, Cops, The Owl, Tombstone, and Tracksuit Mafia have runtime handlers. Drive, the villain encounter sets, six scenarios, and campaign remain to be implemented. Generated card definitions alone are metadata and do not establish implementation.

Printed statistics missing from the imported card data are recorded in `data/card-text/street-encounter-stats.json`. The enrichment and definition generators read this overlay, so rebuilding the data preserves the corrections. Runtime handlers use the same printed statistics.

Disasters begin with two civilian counters and leave play after the last is rescued. Cops replace their scheming with attacks against the villain. Police Cordon's guard and patrol apply to Police minions. Wanted remains in play as an obligation until its action resolves.

Mister Fish changes the normal entry engagement to the identity with the fewest remaining hit points. Flight Serum and the two Tombstone attachments retain their printed numerical bonuses if their host changes type; abilities referring specifically to the attached enemy or minion require that type. Beetle chooses a minion-compatible attachment and attaches it specifically to himself. Under Rules Reference “Attach To,” an ability naming the specific host does not resolve the attachment's original host-selection instruction. Thus Beetle does not resolve that instruction's separate tough-status effect.

Tracksuit Mafia tucks actual encounter cards faceup, preserving physical card IDs and keeping them out of all encounter piles. Cards tucked under it are visible in the public board, survive saving, and are discarded when the side scheme leaves play. Revealing a tucked minion uses a distinct reveal source, so it does not recursively trigger the side scheme's response to revealing a Tracksuit from the encounter deck. Ivan's acceleration tokens belong to his minion entity and leave play with him.

Behavior tests are in `test/Marvel/Encounter/FearNoEvilSpec.hs`. Remaining content and broader verification are required before declaring the expansion complete.

Before implementing Kingpin, apply the [FFG ruling dated September 10, 2026](https://hallofheroeslcg.com/latest-ffg-rulings-post-rrg-1-7/): The King's Gambit 1B hero action has no per-phase limit and resolves as much as possible. Shuffling an empty discard pile still shuffles the encounter deck and can satisfy that part of its instruction. Do not require the full printed threat-removal amount or a nonempty discard pile before offering the action. This specific official clarification governs its “then” handling.

The same official ruling collection (September 14, 2026) confirms that Tracksuit Mafia's tucked-card reveal is outside the encounter deck and cannot chain into further tucked-card reveals. The September 22 clarification also says to preserve Change of Venue's intended non-surge boost resolution; FFG announced an erratum distinguishing its non-surge When Revealed ability. Apply that exception when implementing Protection Racket, rather than granting a surge from its boost.
