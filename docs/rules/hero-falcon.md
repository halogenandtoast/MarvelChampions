# Falcon hero pack

Source: [FFG Falcon rulesheet](https://images-cdn.fantasyflightgames.com/filer_public/e1/2c/e12cf5b2-ef0d-40e9-8347-8cb35c48ee53/mc53_rulesheet-web.pdf), MC53, pages 1–2. The supplied September 2026 CDN URL was unavailable through the browser; this is the official original publication.

## Rulesheet additions

Per-player resource costs multiply the printed number by the number of players who began the scenario. Draw Their Fire costs **1 per player**, despite the local MarvelCDB dump omitting the multiplier.

Alliance permits other players to contribute toward a card's costs. Find searches eligible areas within the current game. Linked cards begin set aside when their enabling card is in a deck, and are excluded from deck-size calculations. Tucked cards remain faceup and out of play, and are discarded when their host leaves play. A victory card enters the victory display when defeated; victory also applies when its last all-purpose counter is removed. Villainous minions receive and resolve boost cards during activations.

Eagle-Eyed follows the completed effects of the Aerial card that triggered it; it cannot rearrange that card's own resolution. Exact boost-icon counts open the normal counting window and include modifiers such as The Next Evolution. Original star symbols are counted separately: they are not boost icons and cannot be changed by Chaos Control. Up, Up, and Away specifies printed icons, so its comparison uses the original card symbols.

## Rulings researched autonomously, 2026-11-02

The user instructed the implementation team to resolve questions from official sources without awaiting approval.

[Official FFG answers, collected by Hall of Heroes](https://hallofheroeslcg.com/latest-ffg-rulings-post-rrg-1-7/): March 19, 2026 clarifies that Redwing cannot initiate against a visible zero-icon encounter top. A facedown top permits initiation with a valid damage/threat target, even if its identity was previously known. January 26 confirms Sam's once-per-round limit persists through form changes. January 17 and March 6 specify that Aerial Evacuation prevents damage taken, follows piercing, and prevents overkill reaching Sam when saving an ally.

## Implementation map

All definitions are in `Marvel.FalconWinter.Cards`; card modules use the `Fw53…` prefix. Shared engine support handles encounter-deal interrupts, assigned-boost exchange, defense without exhaustion, and Tech-Pac's combined-thwart exhaustion cost. The linked shield is card 53034 and is enabled by title upgrade 53023.
