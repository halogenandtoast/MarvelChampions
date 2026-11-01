# Bishop — Age of Apocalypse

Sources: [official MC45 rulebook](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc45_age_of_apocalypse_rulebook-compressed.pdf), especially the printed starter deck on p.23; card records 45001–45029 in `frontend/public/data/cards.json`; Rules Reference v1.8, including its Age of Apocalypse erratum for Suit Up in `faq-errata.md`.

Card text takes precedence over the rulebook's introductory strategy prose. In particular, Energy Absorption responds to **damage from an attack**, although the introductory paragraph loosely describes taking damage.

## Bishop and resources

Energy Absorption is optional, responds after actual attack damage is taken, discards that many cards from the deck, and adds each discarded **resource card** to hand. A resource card is the resource card type; a resource icon printed on an event, ally, or upgrade does not make that card a resource card. Tough and prevention reduce damage actually taken, so they reduce the number discarded. Damage from overkill is damage from an attack. Nonattack damage does not trigger it.

Lucas Bishop's Temporally Displaced responds after changing to alter-ego form and retrieves a Temporal card already in the discard pile. It cannot retrieve a non-Temporal card.

Malcolm and Randall discard a resource card as their once-per-phase action cost. The bonus healing requires the specified **printed** physical or energy icon; a wild icon does not substitute for a printed physical or energy icon. Stored Energy has two printed icons, energy and physical.

Bishop's Rifle and Uniform count resource **cards** in hand, independently of the number of icons on each card. The Rifle's activated ability is an attack and gains ranged. Uniform's response follows completion of Energy Absorption, so the resources just returned to hand count toward its healing.

Super-Charged counts the resource icons on the discarded resource card, places charge counters, and can be discarded when Bishop makes a basic attack. The bonus is twice its charge counters, capped at eight. Multiple copies are separate upgrades.

Concussive Blast and Command Authority care whether a **resource card was spent to pay for that event**, not merely whether resources were generated. Resource-card provenance is kept in the payment receipt; discarded cards used as nonresource costs do not count as spent resource cards.

Energy Conversion shuffles every resource card in the player's discard pile into the deck, preserving other discard cards, then limits damage the player takes from that particular attack to three. It does not protect against a later attack or nonattack damage during the attack. Its defense label follows the usual defense-event rules.

## Leadership and Basic cards

Sidekick attaches to an identity-specific ally its player controls, adds two hit points, and heals it after that player's basic recovery. It has a maximum of one per deck. Side-by-Side readies the sidekick **as a cost**, which requires an exhausted ally that can legally ready and pays additional ready costs. Its effect then readies the hero and either heals both or gives both +1 THW and +1 ATK until the phase ends.

Suit Up follows RR v1.8 errata: search for an ally and an upgrade that can attach to **an ally**. They need not be compatible with each other, and no ally needs to be in play. Searching does not play the upgrade, so current attachment-slot occupancy does not restrict the search. Its implementation classifies the printed attachment type using the implemented upgrade catalogue and `sameCard`, including reprints. This includes upgrades that can attach to characters (Honorary Avenger, Honorary Guardian, Warrior of the Great Web, Honorary X-Men, X-Force Recruit); hero-only upgrades are excluded.

Legion checks the printed resource symbols on the discarded card. A wild does not count as energy, mental, or physical. If more than one of those symbols appears, each applicable effect resolves. His response can follow any basic power: attack, thwart, or defense.

## Starter deck

Bishop identity 45001A/B; Malcolm; Randall; Bishop's Rifle; Bishop's Uniform; Super-Charged ×2; Concussive Blast ×2; Command Authority ×2; Energy Conversion ×2; Stored Energy ×3. These are the fifteen signature cards.

Leadership: Cable; X-23; Team Training ×3; Advanced Suit ×3; Sidekick; Side-by-Side ×3; Suit Up ×3; Lead from the Front ×3; The Power of Leadership ×2.

Basic: Legion; Marrow; Energy; Genius; Strength. Total: 40 cards. Obligation: Fear the Future. Nemesis set: Trevor Fitzroy; Portal Through Time; Bantam; Temporal Trickery ×2.

## Nemesis set

Trevor Fitzroy has quickstrike and responds when his attack defeats an ally. He places two threat on Portal Through Time if present; otherwise he finds and reveals it. Bantam uses the same portal effect when revealed. Portal Through Time gives the first Temporal card revealed each phase surge. Temporal Trickery discards a hand card with the most printed resource icons, choosing among ties, and places that many threat on every scheme. An empty hand discards nothing and adds no threat.

Fear the Future optionally changes Bishop to alter ego, then offers exhausting Lucas to remove the obligation or discarding the obligation and all resource cards in hand. Its second option gains surge if it discards no resource cards.

## Rulings

No new project-owner ruling has been required. Published card text and RR v1.8 settle the implemented interactions above. The rules document does not replace behavior tests or the independent card audit.
