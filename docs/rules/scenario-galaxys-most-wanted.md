# The Galaxy's Most Wanted: new rules, Ship Command, Power Stone and Badoon Headhunter

> Source: *Marvel Champions: The Card Game* — *The Galaxy's Most Wanted* campaign expansion (MC16) rulebook (FFG,
> 2021), 24 pages. Rulebook text is verbatim except for formatting; `<!-- TGMW rulebook p.N -->` gives the page. Card
> text is quoted from MarvelCDB (https://marvelcdb.com/api/public/cards/gmw.json), **not** from the rulebook; the
> printed card and [faq-errata.md](faq-errata.md) beat it. This file holds the rules the whole box relies on and the
> modular sets the engine foundation implements (Ship Command, Power Stone, Badoon Headhunter); each scenario's own
> rules are appended by their own sections.

Related RR text already in this folder: the glossary entries **Victory X**, **Victory Display**, **Patrol**,
**Stalwart**, **Peril**, **Permanent**, **Setup (Keyword)**, **Amplify Icon**, **Hinder X**, **Villainous** and
**First Player**; the "Galaxy's Most Wanted Expansion" FAQ and errata in [faq-errata.md](faq-errata.md). The RR
glossary wording of the keywords supersedes the rulebook's short versions below.

## Summary (non-normative)

```
Victory X: defeated (a character or side scheme), the card goes to the shared, out-of-play victory display instead
  of its discard pile; an attachment with victory X goes there when what it is attached to is defeated.
Patrol: while a patrol minion is engaged with you, you cannot thwart the main scheme (basic thwarts and
  abilities labeled thwart; removing threat otherwise is not a thwart).
Stalwart: cannot be stunned or confused; gaining it removes those status cards.
Peril: while you resolve the card, the other players cannot play cards or trigger abilities.
Amplify icon: each boost card turned faceup during an enemy activation gains a boost icon per amplify icon in play.
Ship Command: the Milano support (Permanent, Setup) is controlled by the first player and follows the token; any
  player may "exhaust the Milano" when a card offers it, whoever controls it.
Power Stone: Setup, attached to the villain; Permanent; moves to the hero or villain that deals 3+ damage to its
  holder with a single attack.
```

## Important keywords
<!-- TGMW rulebook p.3 -->

\* Keywords new to this expansion.

**Hinder X** \*
When a card with the hinder X keyword is revealed, place X threat on that card.

**Incite X**
When a card with the incite X keyword is revealed, place X threat on the main scheme.

**Patrol** \*
While any minions with the patrol keyword are engaged with a player, that player cannot thwart the main scheme.

**Permanent**
A card with the permanent keyword cannot leave play.

**Piercing**
An attack with the piercing keyword discards any tough status cards from the target of the attack before dealing
damage.

**Ranged**
An attack with the ranged keyword ignores the retaliate keyword.

**Setup**
A card with the setup keyword begins the game in play.

**Stalwart** \*
A character that has the stalwart keyword cannot be stunned or confused.

**Team-Up**
The team-up keyword names two characters. To include a card with the team-up keyword in your deck, your chosen
identity must match one of the named characters. Additionally, a card with the team-up keyword cannot be played
unless both of the named characters (hero or ally) are in play.

**Villainous** \*
When a minion with the villainous keyword activates, give it a facedown boost card from the top of the encounter
deck. When you resolve that minion's activation, turn the boost card faceup and apply its boost icons to the minion's
stats for this activation. If the boost card has a boost ability, resolve its effects. Discard the boost card after
the activation.

**Victory X** \*
When a card with the victory X keyword is defeated, place it in the victory display instead of its owner's discard
pile.

## Victory display (new)
<!-- TGMW rulebook p.3 -->

The victory display is an out-of-play game area shared by all players. Cards in the victory display follow the
standard rules for out-of-play cards.

## Amplify icon (new)
<!-- TGMW rulebook p.3 -->

The amplify icon increases the number of boost icons on boost cards. When a boost card is turned faceup during an
enemy activation, add one additional boost icon to that card for each amplify icon in play.

## Frequently asked questions (the rules above)
<!-- TGMW rulebook p.21 -->

**Q.** In the Ship Command modular set, the treachery cards Blind Side (145), Hull Breach (146), and Power Drain (147)
give the player the option to exhaust the Milano. Can the player choose this option if they do not control the
Milano?
**A.** Yes. Because this effect is an option explicitly given to the player, that player may choose to exhaust the
Milano even if they do not control it.

**Q.** What happens when a stunned or confused character gains the stalwart keyword?
**A.** As the stalwart keyword prevents a character from being in a stunned or confused state, a character that gains
stalwart while they are stunned or confused will immediately have all stun and confuse status cards removed from
them.

**Q.** If Rocket Raccoon reveals Shadow of the Past, will the amplify icon from the Vendetta side scheme apply to
Blackjack O'Hare's quickstrike attack?
**A.** No. Each sentence of Shadow of the Past is resolved one at a time, meaning Blackjack O'Hare's quickstrike
attack will occur before Vendetta enters play.

## Ship Command (modular)

Card text (MarvelCDB):

- **16142 Milano** (support, unique; Aerial, Vehicle). Permanent. Setup. The first player controls the Milano.
  *Piloting* — **Resource**: Exhaust the Milano → generate a [wild] resource for any player.
- **16143 Rogue Vessel** (environment; Aerial, Vehicle; boost 2). Surge. **Forced Interrupt**: When the villain phase
  ends, deal 1 damage to each player. **First Player Action**: Exhaust the Milano and spend 2 resources of any type →
  discard this card.
- **16144 Cannonade** (side scheme; base threat 1; boost 2; 1 amplify icon). Hinder 3[per_hero]. **First Player
  Action**: Exhaust the Milano → remove 3 threat from this scheme.
- **16145 Blind Side** (treachery; boost 1). Peril. **When Revealed**: Choose one: exhaust the Milano; spend
  [physical][physical] resources; stun the first player.
- **16146 Hull Breach** (treachery; boost 2). Peril. **When Revealed**: Choose one: exhaust the Milano; spend
  [mental][mental] resources; deal 3 damage to the first player.
- **16147 Power Siphon** (treachery; boost 1). Peril. **When Revealed**: Choose one: exhaust the Milano; spend
  [energy][energy] resources; discard 1 card at random from the first player's hand.
- **16148 Special Delivery** (treachery). **When Revealed (Alter-Ego)**: You may exhaust the Milano. If you do not, the
  villain schemes with +1 SCH. **When Revealed (Hero)**: You may exhaust the Milano. If you do not, the villain attacks
  you with +1 ATK.

## Power Stone (modular)

- **16149 Power Stone** (attachment, unique; Infinity Stone). Setup. Attach to the villain. Permanent. **Forced
  Response**: After a hero or villain deals 3 or more damage to attached character with a single attack, attach Power
  Stone to the attacking hero or villain.

## Badoon Headhunter (modular, used by the campaign in every scenario)

- **16183 Badoon Headhunter** (minion; Badoon; SCH 1, ATK 1, 7 hit points). Victory 2. Villainous. [star] **Boost**:
  Put Badoon Headhunter into play engaged with you.
- **16184 On the Hunt** (treachery; boost 1 + [star]). Surge. **When Revealed**: Choose to either take 2 damage or
  discard 1 card at random from your hand. [star] **Boost**: Give the villain 1 additional boost card for this
  activation.
- **16185 Dead to Rights** (treachery; boost 1 + [star]). Surge. **When Revealed**: Exhaust your identity. If you
  cannot, place 2 threat on the main scheme. [star] **Boost**: Give the villain 1 additional boost card for this
  activation.
- **16186 Headhunter's Henchman** (minion; Badoon; SCH 1, ATK 2, 8 hit points; boost 3). Surge. Patrol.
- **16187 Fugitive Recovery** (side scheme; base threat 2; boost 3). Surge. Hinder 3[per_hero].

## How the engine reads these rules

- **Victory X** is the keyword `Victory n`. A minion or side scheme that is defeated leaves play by
  `DefeatedLeavesPlay`: with victory X, for the victory display (`AddToVictoryDisplay`, out of play, not discarded,
  so nothing that answers a card going to a discard pile sees it); without, for its discard pile as before. An
  attachment on a defeated minion goes the same way, so one with victory X goes to the display. The display is the
  game's, shown on the board beside the encounter discard pile, and kept in saved games.
- **Patrol**: the main scheme is not a scheme the engaged player can thwart (`SchemeYouCanThwart`): their hero's and
  allies' basic thwarts, a divided basic thwart, and any choice that would thwart it by an ability labeled thwart are
  not offered. Removing threat from the main scheme by an ability not labeled thwart is not kept from them (RR
  Thwart).
- **Stalwart**: a stalwart villain or minion is not given a stunned or confused status card; one that gains stalwart
  (from an attachment, say) has them removed as soon as it has it.
- **Peril**: from the treachery's reveal until it has resolved, the other players are offered no card to play and no
  ability but forced ones, and the resolving player may not use an ability on a card another player controls (the
  Milano's resource, when the first player is someone else).
- **Amplify**: each boost card turned faceup for an enemy's activation is counted with one more icon for each amplify
  icon in play in the area (`CountActivationBoostIcons`); boost icons counted otherwise (Lightning Bolt) are not.
- **Setup on encounter cards**: after the villain is set up and before the main scheme's own Setup, each card with
  the setup keyword in the encounter deck goes into play for the first player (RR Appendix II, step 11): the Milano
  under their control, the Power Stone attached as its "attach to" says. A scenario's Setup that puts one of them into
  play again finds it there already; one that sets the set aside takes it back out.
- **The Milano**: the first player controls it, and it moves to their play area whenever the first player token
  passes, the first player is chosen at setup, or the first player is eliminated. Its resource ability may be used by
  any player paying for something, the first player resolving it for them. "Exhaust the Milano" (a cost, `ExhaustMilanoCost`,
  or a card's option) may be done by any player, as the FAQ says, and only while it is in play and ready.
- **The Power Stone**: permanent, it is never discarded: when what it is attached to leaves play, it resolves "attach
  to the villain" again, and with no villain in play it is removed from the game. It moves after a hero's attack
  (damage from that hero's attack, not an ally's) or the villain's attack deals 3 or more damage at once to its
  holder; damage a tough status card prevents is not dealt.
- **First Player Action** on an encounter card (Rogue Vessel, Cannonade): an action only the first player may take, in
  their turn. Side and main schemes may now have abilities. A cost that exhausts the Milano and spends resources
  (Rogue Vessel) does not count the Milano's own resource among them.
- **Limits on encounter cards**: an ability on an encounter card is anyone's, so its limit counts every player's
  uses ("Limit once per round": once a round for the table). "(Limit once per round per player.)" (The Grand
  Collection, Library Labyrinth) is a limit of each player's own (`EachPlayer`), whether the ability is an action or
  a response.
- **Choose one** on Blind Side, Hull Breach and Power Siphon: an option with no valid target cannot be chosen (RR
  Choose): "exhaust the Milano" only with the Milano in play and ready, the resources only if the player can spend
  them, "stun the first player" only if they are not stunned already, "discard at random from the first player's
  hand" only if it has cards. With none open, nothing happens.

## Open rulings

- **The Milano for another player.** "Generate a [wild] resource for any player" is resolved by the Milano's
  controller. The engine lets the paying player use it, the first player deemed to agree; under peril the first
  player cannot help, so the player resolving a peril card may use it only if they control it.
- **"Give the villain 1 additional boost card for this activation"** (On the Hunt, Dead to Rights, Pirate Lackey,
  Pirate Commander, "You Stand Accused!") turned up for a villainous minion's activation (Badoon Headhunter) still
  gives the villain a facedown boost card (RR Villainous: a boost ability that refers to "the villain" still applies
  to the villain). He has no activation under way, so it stays on him and is turned up in his next activation.
- **Victory X on cards with uses** (RR: "placed in the victory display when its last all-purpose counter is removed")
  is not implemented: no card in the game so far has both.
- **Stalwart on identities and allies** (gained from player cards in later packs) is not implemented; only villains
  and minions have it in this box.
