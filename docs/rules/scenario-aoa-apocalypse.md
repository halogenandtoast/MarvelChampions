# Apocalypse — Age of Apocalypse scenario 3

Sources: official MC45 *Age of Apocalypse* rulebook, pp.13–14; cards 45101–45117 and Prelate faces 45179B–45183B; Rules Reference v1.8. Card text and current rules take precedence over MarvelCDB metadata.

## Setup

Standard play begins with Apocalypse II. Expert begins with Apocalypse III. Stage I is an optional easier starting point; stage IV is reached through main-scheme completion. The required encounter sets are Apocalypse and Prelates. Dark Riders and Infinites are the recommended customizable modular sets, alongside the selected Standard set.

Set unused physical villain cards, all five Prelates, and The Tyrant's Throne aside. Reveal Heart of the Empire. The first player reveals a randomly selected set-aside Prelate. Prelate random selection uses the stored game generator, and revealing a Prelate removes that physical card from the set-aside pool.

## Apocalypse and the main scheme

The Age of Apocalypse's threat threshold X is **the numeral** printed in the current Apocalypse stage's hit-point value: 8, 9, 10, or 11. Do not multiply this threshold by the number of players. Apocalypse's actual starting hit points are multiplied by player count.

When the main scheme would complete at stages I–III, its Forced Interrupt removes all threat, ignoring crisis, then reveals the next Apocalypse stage. Threat in excess of the threshold does not remain. Each new stage uses its own starting hit points and printed keywords. Stage IV has stalwart, toughness, and overkill on its attacks; completing the main scheme there loses the game.

Without No Longer Worthy attached, when Apocalypse would be defeated the main scheme replaces that defeat: discard all attachments from him, heal all his damage, and remove X threat from the main scheme, ignoring crisis. He remains on the same stage. No villain-defeated window or victory should occur for this replaced defeat.

With No Longer Worthy attached, ignore that replacement and win when Apocalypse is defeated. Revealing No Longer Worthy attaches it to Apocalypse and heals five hit points per player. He cannot take damage while a Prelate remains in play.

## The side-scheme chain

Heart of the Empire, The Towering Citadel, and The Tyrant's Throne cannot lose threat while any Prelate is in play. Their printed starting threat is respectively two, three, and four; these values are fixed rather than per player.

Defeating each scheme causes the first player to reveal another randomly selected set-aside Prelate and deals every other player a facedown encounter card. Heart then flips to The Towering Citadel, retaining its physical card ID and receiving the Citadel's starting threat. The Citadel reveals The Tyrant's Throne and is removed from the game. The Throne flips to No Longer Worthy and reveals that attachment. Transforming the physical Throne must not place its old side in the encounter discard pile or removed-from-game pool.

The Apocalypse Solution is separate from this chain. Its defeat discards the top X encounter cards, using the current stage's printed scalar hit-point numeral.

## Other encounter cards

Cyberpathy gives +1 SCH and places one threat on every side scheme after Apocalypse schemes. Biomorphing gives +1 ATK and overkill. Molecular Control gives retaliate 1 and stalwart. Their boost abilities attach the actual boost card to Apocalypse, consuming it from the boost area so it is not later discarded as a boost.

The Fittest attaches to a minion tied for the highest printed hit points, with the revealing player choosing ties. It gives tough, +1 ATK, +1 SCH, and +5 hit points. With no eligible minion, it surges.

Wolf Among Sheep activates the Prelate against the revealing player, or Apocalypse if no Prelate is in play. Its boost similarly toughens the Prelate, or Apocalypse if absent.

## Prelates

All five Prelates have five hit points per player, toughness, Victory 3, and continuously engage the first player, including after the first-player token passes. Defeating a Prelate does not strike its Overseer reverse from the campaign log.

Mister Sinister additionally has retaliate 1 and villainous. Shadow King selects a controlled ally with the highest modified THW after attacking; the attacked player chooses either to discard it or place main-scheme threat equal to that THW. Abyss attaches the attacked player's top deck card facedown after activating and gains two hit points per attached card. Those cards are hidden information and must be discarded with Abyss when he leaves play. Sugar Man's attacks gain piercing; an attack that defeats a character heals five damage from him. Mikhail Rasputin deals one nonattack identity damage when initiating an attack against that player.

## Dark Riders

Gauntlet, Barrage, Hard-Drive, Tusk, and Psynapse have Teamwork (Dark Riders). Their attack responses respectively discard a controlled upgrade; deal one damage to every controlled character; place one threat on every scheme; stun the attacked player; or confuse that player. Tusk and Psynapse's boosts apply their corresponding status to the player receiving the activation.

The Dark Riders side scheme has Hinder 1 per player and grants toughness to Dark Riders minions. Its reveal discards encounter cards until a Dark Riders minion is found and reveals that actual card. Toughness applies as the minion enters; it does not retroactively give tough to every Dark Rider already in play.

## Rulings

No new project-owner ruling has been required. The threshold's scalar interpretation is explicit in the printed main-scheme text. Current automated tests cover standard/expert setup, stage progression, defeat replacement, the side-scheme chain and physical-card transformation, attachment boosts, each Prelate, and Dark Riders effects.
