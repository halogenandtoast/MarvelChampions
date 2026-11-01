# Deadpool Hero Pack (MC44)

## Sources

- [Official Deadpool rules insert](https://images-cdn.fantasyflightgames.com/filer_public/8a/98/8a987c42-a835-4fa7-b0b7-11f466e0faa4/mc44_deadpool_rules_insert-web.pdf).
- Rules Reference v1.8: local glossary, timing and FAQ/errata files.
- [Official Mission Updates](https://www.fantasyflightgames.com/mission-updates/): Armed to the Teeth swap clarification.
- Printed card graphics: [Blackout](https://marvelcdb.com/bundles/cards/44053.png), [Rock, Paper, Scissors](https://marvelcdb.com/bundles/cards/44056.png), [Tic-Tac-Toe](https://marvelcdb.com/bundles/cards/44057.png).

## Insert implementation summary (non-normative)

<!-- MC44 insert pp.1–2; use current RR where older insert wording differs. -->

The 'Pool aspect is a fifth aspect. When at least one player chooses it, include exactly one Crisis of Infinite Deadpools in the encounter deck and set aside the remainder of the Dreadpool encounter set. This setup depends on the chosen aspect, independently of hero identity. Merely including an off-aspect 'Pool card does not cause this setup (RR FAQ p.64).

Adam Warlock may replace one of his four aspects with 'Pool. Other identities that choose multiple aspects may choose 'Pool normally. Acceleration tokens and acceleration icons are distinct; effects counting one never count the other.

Armed to the Teeth searches outside the current game for an aspect WEAPON upgrade. Identity-specific and campaign cards do not qualify. Its swap preserves exhaustion, per the official v1.8 Mission Updates explanation. Apply current swap and attachment rules rather than treating a swap as playing a card.

'Pool allies have no consequential damage pips. Blanking their text does not add pips. Handwritten edits and restriction boxes on printed 'Pool cards apply. Break Time's printed cost is **3 per player**, multiplied by the starting player count, and Alliance permits contributions from other players.

The insert also covers existing rules for piercing, team-up, player side schemes, amplify, Alliance, ranged and victory. Use the current RR entries for those mechanics.

## Printed metadata and diagrams

The local card dump includes icons omitted by the pack summary script:

| Card | Icons |
|---|---|
| Dogpool, Kidpool, Bob, Agent of Hydra | 1 acceleration each |
| Headpool, Lady Deadpool | 1 amplify each |
| Negasonic Teenage Warhead, Pandapool | 1 hazard each |
| Live Dangerously | 1 crisis, 1 acceleration, 1 amplify, 1 hazard |
| Ambush, Distraction | 1 crisis each |
| Involuntary Procedures | 1 hazard |
| Dreadful Deeds | 1 amplify |

Live Dangerously starts with **3 threat per player**. Its Victory value is zero.

Blackout has six resource spaces: two energy, two mental and two physical. All six must be filled.

Tic-Tac-Toe has nine separately tracked spaces:

| | | |
|---|---|---|
| Energy | Energy | Energy |
| Mental | Mental | Mental |
| Physical | Physical | Physical |

Any row, column or diagonal wins. Deal all damage stored on the card when resolving its completion, including tokens outside the winning line. Wild resources may pay as a chosen matching resource type.

Rock, Paper, Scissors uses printed resources: energy beats mental, mental beats physical, physical beats energy, and wild beats each non-wild type. Ties lose, including wild versus wild. A qualifying pair of printed icons suffices when cards have multiple printed resources.

## Timing and current rules

- A cost requiring a player to **take** damage must be fully taken. A cost requiring damage to be **dealt** can be paid even if prevented (RR Costs). This distinguishes Maximum Effort / Yoo-Hoo from Negasonic Teenage Warhead.
- Maximum Effort and Yoo-Hoo permit damage up to remaining hit points, including lethal damage. Resolve costs and their interrupts before the paid effect. A change to alter-ego through Deadpool's replacement does not invalidate an already initiated hero event.
- Damage prevention changes damage taken, not damage dealt (RR Damage p.14). Metacidal Tendencies does not place an acceleration token merely because tough or prevention stopped all damage taken.
- Moving damage heals the source and deals damage to the destination; moving threat off a scheme removes threat (RR Move). Metagame moves must preserve those interactions.
- Deadpool's defeat replacement does not work while his printed text is blank. Git Gud can compete with it in the same defeat window; follow forced interrupt ordering.
- Metaknowledge cancels the entire revealed encounter card. Negasonic Teenage Warhead and Get in Front of Me cancel only When Revealed effects.
- This Card is Fire triggers from each copy in hand after its owner's turn ends, including in alter-ego form.
- Involuntary Procedures checks its ten-threat threshold after its own damage-triggered threat placement.
- Apply the RR p.69 'Pool-ized erratum: the converted ally engages its controller. Its printed THW becomes SCH; its other card types are lost. Preserve attachments according to the current type-change rules.
- I Got This resolves each icon's effect once if that icon is present, regardless of multiplicity. Effects asking for a count count icons, excluding acceleration tokens.
- 'Pool Inspection explicitly ignores crisis for its initial main-scheme removal. Its later removal from each scheme follows ordinary crisis restrictions.
- Git Gud's discount applies to a first game or a forgotten prior result, as clarified by the insert.
- Per-player resource costs use the starting player count wherever the cost is referenced, including War and cost-based searches; discounts apply after multiplication. Player elimination does not reduce that count.
- A Self resource retrieved by another player through Plot Convenience uses its current holder's damage for its multiplier, while retaining its original owner for discard and ownership rules.

## Starter deck

The printed forty-card deck comprises all fifteen signature cards plus the twenty-five printed copies of 44013–44031. Maximum Effort, Yoo-Hoo, Deadpool's Katana and This Card is Fire have two signature copies each; other signature cards have one. The aspect portion has three copies each of Barely a Scratch, I Got This and Healing Factor, and one each of its other cards. Identity and obligation are outside the forty cards.

## Rulings (from the project owner, 2026-11-01)

Use explicit in-game player prompts for whether Wade talked during the player phase, the controller's previous-game result, and whole minutes spent on Break Time. Store the answers in game state so undo and reload remain consistent.

For Armed to the Teeth, treat the implemented card catalogue as the player's available collection. Offer implemented aspect WEAPON upgrades outside the current game.
