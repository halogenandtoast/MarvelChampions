# Winter Soldier hero pack

Source: [FFG Winter Soldier rulesheet](https://images-cdn.fantasyflightgames.com/filer_public/89/02/8902454a-f85a-4d2d-a8ee-971c21b03f48/mc54_rulesheet-web.pdf), MC54, pages 1–2. The supplied September 2026 CDN URL was unavailable through the browser; this is the official original publication.

## Rulesheet additions

Piercing removes the attack target's tough status cards before damage is dealt. Ranged attacks ignore retaliate. Steady permits a second stun and confusion card; two matching cards replace an activation and are then removed together. Team-Up cards require an appropriate named identity for deck inclusion and both characters in play to be played. Defeated victory cards enter the victory display. Villainous minions use boost cards during activations.

Cybernetic Arm increases every damage instance of the Attack event it funds: the rulesheet explicitly illustrates Firepower dealing four damage for each exhausted Weapon when funded by the arm.

The starter deck uses **one** Super-Soldiers (54022) and **one** Winter, Widow, Soldier, Spy (54023). The pack contains two of each, but each is limited to one per deck. Using all printed quantities would incorrectly produce 42 cards.

## Rulings researched autonomously, 2026-11-02

The user instructed the implementation team to resolve questions from official sources without awaiting approval.

[Official FFG answers, collected by Hall of Heroes](https://hallofheroeslcg.com/latest-ffg-rulings-post-rrg-1-7/): January 17, 2026 establishes that when Black Widow redirects Arm Block's attack to an A.I.M. Grunt, its prevention still applies to Black Widow's original triggering attack. March 6 confirms Arm Block prevents damage taken; a boost condition checking damage dealt can still occur.

[FFG's Winter Soldier announcement](https://www.fantasyflightgames.com/en/news/2025/4/17/armed-and-dangerous-1/) confirms the arm's wild resource can be declared as energy to satisfy Electrical Discharge. The actual declared resource type is used by the engine.

## Implementation map

Definitions are in `Marvel.FalconWinter.Cards`; card modules use the `Fw54…` prefix. Cybernetic Arm creates a lasting effect on the funded event, with its own marker for the conditional clauses of Arm Block and Metal Punch. Rifle and Bambino keyword effects expire with the current basic attack.

Encased in Ice remains an attachment except when an attack targets it as a minion. Damage beyond its three hit points moves to its host. Under RR v1.8 Move and the Vibranium Suit FAQ, moved damage is dealt to the destination: tough and applicable damage prevention work normally. It is not another attack. Bambino gains restricted on the upgrade itself while attached to an identity. Black Widow’s return-event response requires playing her from hand.
