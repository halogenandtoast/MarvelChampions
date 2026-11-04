# Shadowland encounters and campaign

Sources: [Shadowland rulebook and FAQ](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/10/MC65-Rulebook.pdf), supplied MC65 PDF, printed card scans bundled with this repository, and Rules Reference v1.8 under this directory. The implementation covers encounter content and campaign setup; ordinary player cards already exist.

The three scenarios are Shadows in the Night (`65001a`), Shadow Labyrinth (`65038a`), and Heart of Shadow (`65060a`). Each supports normal and expert play. The selected Dark Protégé is stored in scenario state, so saving and undo preserve the random choice. Selection excludes matching player identities and protégés removed by the campaign. The chosen villain's five linked cards are explicitly assembled from the printed many-to-many links.

Position cards retain their actual faces. A position change happens at most once each phase. The street action's “then” clause requires both the full threat placement and a newly applied stun; already being stunned does not pay that part of the action. Matching positions give characters +1 ATK; different positions reduce attack damage and prevent main-scheme threat removal as printed.

Shadow Labyrinth has a separate trap deck. Its cards return to its discard pile, and refilling it adds no acceleration token. Cards mixed in from the ordinary encounter deck retain their ordinary discard destination unless they are traps. Surge deals an encounter card from the ordinary deck. The starting trap deck has eight physical cards, including one Whisper. Shifting Corridors consumes passage counters instead of attacking or scheming. Damage to it removes main-scheme threat. Removing the labyrinth's final threat advances to the chosen villain.

Whisper temporarily converts the selected physical villain card into a minion with six hit points, guard, villainous, and its printed abilities. Defeating it returns that same physical card to the set-aside area. Entering the Chamber removes an in-play Whisper minion before introducing the chosen villain. The engine's minion and villain registries both recognize these six physical cards.

Trap choices open an explicit window before their effects or costs. Nervous Jitters can therefore interrupt Avoid or Disarm, and the two trap side schemes react to the chosen option. Encounter-type guessing includes all eight encounter card types. Iron Monk prevents Avoid. Creeping Corruption is included as a required scenario/campaign component: five counters flip its first face, and five on its second face defeat its owner.

Teamwork checks happen after a defeated ninja leaves play. Its replacement therefore cannot count the departed ninja toward Teamwork. Defeated ninja replacement effects still finish before their original minion's normal departure where required by the printed text.

The campaign records removed protégés, each hero's history of ending games overpowering, selected prepared cards, and expert hit points. Scenario setup restores these records, applies the printed corruption and extra encounter effects, and includes Black Flower where required. An expert finale defeat permits retrying. Repeated records of the same prepared card are deduplicated, interpreting “one copy of each card” as distinct card titles/codes; this is an implementation interpretation of the printed instruction.

Focused behavior tests live in `test/Marvel/Encounter/ShadowlandSpec.hs` and `test/Marvel/Campaign/ShadowlandSpec.hs`. `SmokeSpec` also includes the three scenarios at both difficulties and campaign setup at both campaign modes. These checks establish exercised behavior, not exhaustive proof of every card interaction.

Trap-deck reveals carry a distinct, serializable separate-deck source. Encounter-deck-only reactions do not trigger when a mixed-in minion is revealed there. A behavior test checks that Tracksuit Mafia keeps its tucked minion for a trap-deck reveal, while a subsequent ordinary encounter-deck reveal releases it. Another checks that surge still deals from the ordinary encounter deck.
