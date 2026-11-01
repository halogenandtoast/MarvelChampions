# Mister Sinister

Implementation uses the NeXt Evolution scenario rules and Rules Reference 1.8. The scenario removes a random stage 2 before play, begins at an empty stage 1, and advances through the two surviving stage 2 schemes before stage 3. Each stage 2 introduces its associated power set; all three sets begin separately set aside.

Official scenario rulebook: https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc40_next_evolution_rulebook-web.pdf

The latest published designer-ruling archive was checked, including its post-1.8 entries: https://hallofheroeslcg.com/latest-ffg-rulings-post-rrg-1-7/

The July 25, 2025 designer ruling explicitly allows Sinister Ends to attack Hope Summers while she is a minion, and allows players to defend that attack. The attack changes its character target while retaining its activation and target player. Its damage and surviving-target retaliate use the ordinary damage pipeline. Source: https://hallofheroeslcg.com/latest-ffg-rulings-post-rrg-1-6/

The May 10, 2024 designer ruling says replacing an existing status with Vapors of Valtorr does not give Mister Sinister a new status; the forced response therefore checks whether the total status count increased. The October 26, 2023 Out of Reach ruling prohibits nonattack damage even from an Aerial character: only an attack by an Aerial character, an Aerial attack ability/card, or a ranged attack can damage him. Source: https://hallofheroeslcg.com/latest-ffg-rulings-post-rrg-1-5/

High Ground (40154) is a treachery on its printed card, although upstream MarvelCDB metadata calls it an attachment. Both the local card data and the generator override correct that type. Printed scan: https://marvelcdb.com/bundles/cards/40154.png

Enemy damage now has two distinct interrupt points: damage being dealt before tough/reduction, and damage actually being taken after those checks. Explicit serializable continuations preserve those phases through cancellation, reduction, redirection, and simultaneous damage. This also covers retaliate damage sent directly to minion/villain messages.
