# Timing, Ability Types and Resolution

> Source: *Marvel Champions: The Card Game* Rules Reference v1.8 (FFG, 2026), transcribed from the official PDF.
> Text is verbatim except for formatting; icons are written as `[energy]`, `[mental]`, `[physical]`, `[wild]`, `[boost]`, `[star]`,
> `[per player]`, `[acceleration]`, `[amplify]`, `[crisis]`, `[hazard]`, `[consequential damage]`, `[unique]`; `→` is the cost arrow.
> `<!-- RR v1.8 p.N -->` gives the page of the entry in the PDF.

This file collects the Rules Reference text an engine needs for ability timing and resolution. Everything under
the `###` entry headings is a verbatim copy of the matching glossary entry (the glossary files are canonical);
the summary below is ours and is **non-normative**: when in doubt, follow the quoted entries.

## Engineer's summary (non-normative)

- **Precedence:** card text / scenario rules > Rules Reference > Learn to Play (Golden Rules). "Cannot" beats
  "can" ("Cannot"). If nothing answers a question, pick the outcome worst for the players (Grim Rule).
- **Ability kinds** (Ability): *constant* (no bold trigger; always on while in play), *triggered* (bold trigger +
  colon), *keywords*. Keywords and status cards are now defined as equivalent constant/triggered abilities
  (see each keyword entry and Status Cards).
- **Mandatory:** constant, Setup, When Revealed, When Defeated, When Completed, Forced Action/Interrupt/Response,
  Boost, keywords ("may" inside them is optional). **Optional:** Action, Interrupt, Response, Resource.
  "Hero"/"Alter-Ego" in a trigger means the player must be in that form. Special abilities only resolve when
  another ability says so.
- **Windows:**
  - *Action*: during your turn (or another player's turn by request). Forced Action must resolve before the
    player phase ends.
  - *Resource*: whenever the controller is generating resources to pay a cost.
  - *Interrupt*: when the triggering condition initiates, before it resolves; "would" interrupts resolve earlier,
    when the condition becomes imminent. A replacement/cancel closes the window for the original condition.
  - *Response*: immediately after the triggering condition resolves. Responses to any step of revealing an
    encounter card wait until the whole reveal finishes. Responses to the text before a cost arrow resolve before
    the text after it.
  - One occurrence → one interrupt window and one response window, even if it creates several triggering
    conditions (Triggering Condition). Each interrupt/response triggers at most once per occurrence (per copy).
  - First player gets the first opportunity in each interrupt/response window, then player order (First Player).
- **Priority for the same triggering condition** (Ability → Simultaneous Timing Priority):
  1 constant abilities, delayed effects, lasting effects → 2 interrupts (a status-card Forced Interrupts,
  b Forced Interrupts, c Interrupts) → 3 Boost and When Revealed → 4 responses (a Forced Responses,
  b Responses) → 5 consequential damage. Within the same bold trigger, the first player orders simultaneous
  effects (Simultaneous Resolution, Forced).
- **Initiating** (Initiating Abilities): declare → place card on table (if playing) → check play restrictions
  (valid target, form) → determine cost(s) (choose X) → apply cost modifiers → pay (abort without paying if
  impossible) → effects attempt to initiate (cancel point) → resolve / enter play / event to discard.
- **Targets:** an ability that needs targets can only be initiated with ≥1 valid target; a target is valid if any
  part of the ability can affect it (Target).
- **Resolving text:** read the whole ability for alteration effects, then resolve one sentence at a time
  (Ability, Alteration Effect); "then" needs the earlier part fully resolved; "and" parts resolve simultaneously
  and independently.

## The Golden Rules
<!-- RR v1.8 p.4 -->

If the text of this Rules Reference directly contradicts the text of the Learn to Play book, the text of the Rules Reference takes precedence.

If the text of a card or the rules of a scenario directly contradicts the text of either the Rules Reference or the Learn to Play book, the text of the card or the scenario rules takes precedence.

## The Grim Rule
<!-- RR v1.8 p.4 -->

If players are unable to find the answer to a rules or timing conflict in this Rules Reference, resolve the conflict in the manner that the players perceive as the worst possible resolution at that moment with regards to winning the scenario, and continue with the game.

## Abilities and Ability Types

### Ability
<!-- RR v1.8 p.4 -->

An ability is game text on a card that explains what the card does *(or can do)*. Several examples can be seen in Appendix III of this Rules Reference.

- Card abilities only interact with cards that are in play, unless the ability specifically refers to an out-of-play area or element.
- Card abilities on hero, alter-ego, ally, upgrade, and support cards may only be used if the card is in play, unless the ability specifically refers to being used from an out-of-play state. Event cards implicitly interact with the game from an out-of-play area, as per the rules of the event card type.
- If an ability specifies one or more targets, that ability can only be initiated if it has at least one valid target. *For example, an ability that says “deal 5 damage to a minion” cannot be initiated if there are no minions in play.*
  - *Examples of targets include but are not limited to: “the villain,” “a minion,” “an enemy,” “a scheme,” “a hero,” “an ally,” “a character,” “a player,” “you,” “a card.”*
- When an ability has more than one sentence of text, read the entirety of the ability to check for alteration effects that may change the way the ability resolves. Then, resolve the ability one sentence at a time.
- An ability prefaced by a bold timing trigger followed by a colon is referred to as a triggered ability. An ability without a bold timing trigger is referred to as a constant ability. *(See below.)*
- Player card abilities cannot resolve during game setup, unless prefaced by a “**Setup**” timing trigger.
- The resolution of the following ability types is mandatory: constant abilities, “**Setup**” abilities, “**When Revealed**” abilities, “**When Defeated**” abilities, “**Forced Action**” abilities, “**Forced Interrupt**” abilities, “**Forced Response**” abilities, “**Boost**” abilities, and keywords.
  - If one of these ability types uses the word “may”, the part of the ability following “may” is optional.
- The resolution of the following ability types is optional: “**Action**”, “**Interrupt**”, “**Response**”, “**Resource**”. The player who controls the card that has an optional ability determines whether or not to use that ability at the appropriate time. Any player can use such an ability on an encounter card, with the following exceptions:
  - Only the player who controls a player card with an attachment that uses the word “you” or “your” can trigger abilities or pay costs on that attachment.
  - Only the player with an obligation in their play area can trigger abilities or pay costs on that obligation.

**Constant Abilities** — A constant ability is any non-keyword ability whose text contains no bold timing trigger defining its ability type. A constant ability becomes active as soon as its card enters play and remains active while the card is in play.

- Some constant abilities continuously seek a specific condition *(denoted by words such as “during”, “if”, or “while”)*. The effects of such abilities are active anytime the specific condition is met.
- If multiple instances of the same constant ability are in play, each instance affects the game independently.

**Triggered Abilities** — A triggered ability is indicated by a bold timing trigger followed by a colon and the rest of the ability text.

- Unless prefaced by the word “**Forced**”, all interrupt and response abilities are optional.
- “**Forced**” abilities, “**When Revealed**” abilities, “**When Defeated**” abilities, and “**When Completed**” abilities are triggered by the game at the ability’s appropriate timing point.
- If the bold timing trigger of an ability contains the word “Hero” or “Alter-Ego,” the ability can only be used if the player triggering the ability is in the specified form.
- If quotation marks are used around a timing trigger and colon, the quoted text is not itself a timing trigger, but is instead referring to other abilities with that trigger.

**Simultaneous Timing Priority** — Some abilities have timing priority over other abilities. In order, the timing priority of abilities with **the same triggering condition** is:

1. Constant abilities, delayed effects, and lasting effects.
2. Interrupts
   a. Status card “**Forced Interrupt**” abilities.
   b. “**Forced Interrupt**” abilities.
   c. “**Interrupt**” abilities.
3. “**Boost**” and “**When Revealed**” abilities.
4. Responses
   a. “**Forced Response**” abilities.
   b. “**Response**” abilities.
5. Consequential damage.

**See also**: Action, Alteration Effect, “And”, Cancel, Delayed Effect, Forced, In Play and Out of Play, Initiating Abilities, Interrupt, Labeled Ability, Lasting Effects, Qualifiers, Replacement Effect, Resource Ability, Response, Simultaneous Resolution, Special, Target, “Then”, When Defeated Abilities, When Revealed Abilities, “Would”

### Triggered Ability
<!-- RR v1.8 p.45 -->

A triggered ability is indicated by a bold timing trigger followed by a colon and the rest of the ability text.

**See also**: Ability, Action, Forced, Interrupt, Resource Ability, Response, Setup (Triggered Ability), Simultaneous Resolution, Special, Triggering Condition, When Defeated Abilities, When Revealed Abilities

### Triggering Condition
<!-- RR v1.8 p.45 -->

A triggering condition is a specific occurrence that takes place in the game. On card abilities, the triggering condition is the element of the ability that references such an occurrence, indicating the timing point at which the ability may be used. The description of an ability’s triggering condition usually follows the word “when” or “after.”

- Each “**Interrupt**“ and “**Response**” ability can only be triggered once per occurrence of its triggering condition.
  - Multiple copies of a card with an interrupt or response can each be triggered by the same triggering condition.
- If a single game occurrence creates multiple triggering conditions *(such as a single attack causing a character to both take damage and be defeated)*, those triggering conditions are handled with a single interrupt window and a single response window. During each of these windows, abilities that refer to any of the triggering conditions created by the occurrence may be used in any order.

**See also**: Interrupt, Response

### Action
<!-- RR v1.8 p.6 -->

“**Action**” is a type of triggered ability. Players are permitted to trigger action abilities during their turn, or by request during other players’ turns.

- Players can only trigger action abilities on cards they control or on encounter cards.
  - Players cannot trigger action abilities on obligations in other players’ play areas.
- Each “**Forced Action**” ability must be resolved before the player phase can end.
  - A forced action ability can be triggered at any time a non-forced action ability could be triggered.
  - If such an ability has a cost that cannot be paid or requires one or more valid targets and has none, the phase can end without that ability being resolved.

**See also**: Ability, Player Turn, Triggered Ability

### Interrupt
<!-- RR v1.8 p.25 -->

An interrupt ability is a type of triggered ability, indicated by the bold “**Interrupt**” timing trigger. Interrupt abilities may be resolved anytime the specified triggering condition occurs, as described in the interrupt’s ability text. The interrupt ability interrupts the resolution of the specified triggering condition, and resolves immediately before that triggering condition resolves.

- Players can only trigger interrupt abilities on cards they control or on encounter cards.
  - Players cannot trigger interrupt abilities on obligations in other players’ play areas.
- Multiple interrupts may be triggered by the same triggering condition, but each interrupt can only be triggered once per occurrence of the triggering condition.
  - Multiple copies of a card with an interrupt can each be triggered by the same triggering condition.
- An interrupt ability is resolved when its triggering condition initiates, but before that triggering condition resolves.
  - Interrupts that use the word “would” resolve before its triggering condition initiates, when that condition becomes imminent.
- If an interrupt changes *(via a replacement effect)* or cancels an imminent triggering condition, further interrupts to the original triggering condition cannot be triggered.
- Once all players decide they do not wish to resolve any *(further)* interrupts to a triggering condition, *(further)* interrupts to that instance of that triggering condition cannot be used.

**See also**: Cancel, Replacement Effect, Triggered Ability, “Would”

### Response
<!-- RR v1.8 p.38 -->

A response ability is a type of triggered ability, indicated by the bold “**Response**” timing trigger. Response abilities may be resolved after the specified triggering condition occurs, as described in the response’s ability text.

- Players can only trigger response abilities on cards they control or on encounter cards.
  - Players cannot trigger response abilities on obligations in other players’ play areas.
- Multiple responses may be triggered from the same triggering condition, but each response may only be triggered once per occurrence of the triggering condition.
  - Multiple copies of a card with a response can each be triggered by the same triggering condition.
- If single effect causes multiple triggering conditions to occur, responses to each of those triggering conditions can be resolved in any order. *(For example, if an attack triggers both “after you attack” and “after you defeat” responses, those responses can be resolved in any order.)*
- Once all players decide they do not wish to resolve any *(further)* responses to a triggering condition, *(further)* responses to that instance of that triggering condition cannot be used.

**See also**: Ability, Triggered Ability, Triggering Condition

### Forced
<!-- RR v1.8 p.20 -->

Forced is a bold trigger word. If the word “**Forced**” precedes a triggered ability, the ability’s initiation is mandatory.

- “**Forced Interrupt**” and “**Forced Response**” abilities must be resolved when their triggering conditions are met.
- “**Forced Action**” abilities can be triggered at any time during the player phase when a non-forced action ability could be triggered, but must be resolved before the player phase can end.
- If a forced ability requires one or more targets to resolve and it has no valid targets, it does not initiate.
  - Any costs that would have been paid to initiate the ability are not paid.
- For any given triggering condition, forced interrupts take priority and initiate before non-forced interrupts, and forced responses take priority and initiate before non-forced responses.
- If two or more forced abilities would initiate at the same moment, the first player determines the order in which the abilities initiate, regardless of who controls the cards bearing those abilities.
- Each forced ability must resolve as completely as possible before the next forced ability being triggered by the same triggering condition may initiate.

**See also**: Ability, Action, Target

### Resource Ability
<!-- RR v1.8 p.37 -->

A resource ability is a type of triggered ability, indicated by the bold “**Resource**” timing trigger.

- A resource ability can be triggered anytime the player who controls the ability is generating resources to pay a cost.
  - A resource ability that generates a resource only for a specific type of cost cannot be triggered when paying a cost not of that type. *(For example, a resource ability that generates a resource for an aspect card cannot be triggered when paying for a non-aspect card.)*

**See also**: Cost, Resource, Triggered Ability

### Special
<!-- RR v1.8 p.40 -->

A special ability is a type of triggered ability, indicated by the bold “**Special**” timing trigger. Special abilities may only be resolved through the explicit instruction of another card ability.

**See also**: Triggered Ability

### Setup (Triggered Ability)
<!-- RR v1.8 p.40 -->

“**Setup**” is a type of triggered ability that is resolved during setup.

- Setup abilities are mandatory.
- Setup abilities on encounter cards are resolved during the “Resolve Scenario Setup and When Revealed Abilities” step of setup.
- Setup abilities on player cards are resolved during the “Resolve Character Setup Abilities” step of setup.

**See also**: Triggered Ability, Appendix II: Setup

### When Revealed Abilities
<!-- RR v1.8 p.48 -->

A when revealed ability is a type of triggered ability, indicated by the bold “**When Revealed**” timing trigger.

When a player reveals a card from the encounter deck, a new scheme stage, or a new villain stage, all “**When Revealed**” abilities on the card resolve.

- If an encounter card with a “**When Revealed**” ability enters play during setup, resolve that ability during the “Resolve Scenario Setup and When Revealed Abilities” step.
- If an encounter card with a “**When Revealed**” ability is put into play without being revealed, the “**When Revealed**” ability does not trigger.
- “**When Revealed**” abilities on villain and main scheme cards cannot be canceled.

**See also**: Choose (Game Element), Encounter Card, Main Scheme, Triggered Ability, Villain, Appendix II: Setup

### When Defeated Abilities
<!-- RR v1.8 p.48 -->

A when defeated ability is a type of triggered ability, indicated by the bold “**When Defeated**” timing trigger.

- The “**When Defeated**” timing trigger is equivalent to the following trigger: “**Forced Interrupt**: When this card is defeated...”
- When a villain stage, side scheme, main scheme stage, ally, or minion is defeated, all “**When Defeated**” abilities on the card resolve.
  - A defeated card leaves play after its “**When Defeated**” ability is resolved, if any.

**See also**: Ally, Defeat, Main Scheme, Minion, Side Scheme, Triggered Ability, Villain

### When Completed Abilities
<!-- RR v1.8 p.48 -->

A when completed ability is a type of triggered ability, indicated by the bold “**When Completed**” timing trigger.

When a main scheme is complete, all “**When Completed**” abilities on the card resolve.

- The “**When Completed**” timing trigger is equivalent to the following trigger: “**Forced Interrupt**: When this scheme is completed...”

**See also**: Main Scheme

### Labeled Ability
<!-- RR v1.8 p.26 -->

A labeled ability is a triggered ability with a parenthetical following its bold trigger text that designates that ability as an attack, defense, and/or thwart.

- The identity of the player using the labeled ability is considered to be performing the labeled effect when the labeled ability begins resolving *(after costs have been paid)*.
- When a player resolves an ability labeled “*(attack),*” that ability is considered to be an attack made by that player’s identity.
- When a player resolves an ability labeled “*(defense),*” that ability is considered to be a defense made by that player’s identity.
  - When such an ability is initiated during an attack, the player’s identity becomes the defender of that attack.
- When a player resolves an ability labeled “*(thwart),*” that ability is considered to be a thwart made by that player’s identity.
- When a player resolves an ability with multiple labels, that ability is considered to be each of the labeled ability types, all made by that player’s identity.
- If a player triggers a labeled ability while their identity has one or more status cards that cancel any of the labeled ability types, the entire ability *(except for its costs)* is canceled.
  - That identity is not considered to have attacked, defended, or thwarted.
  - Each status card on the player’s identity that cancels any of the labeled ability’s types is removed when that ability is canceled. *For example, an ability labeled “*(attack/thwart)*” would remove both a confused status and a stunned status from the identity of the player who triggered the ability.*

**See also**: Attack (Player Ability Type), Defend, Thwart

### Keywords
<!-- RR v1.8 p.25 -->

A keyword is an attribute that conveys specific rules to its card.

- If a card gains multiple instances of a keyword, any additional instances have no effect unless that keyword is followed by a number *(e.g. Hinder X, Retaliate X)*, in which case the numbers for each instance are added together.

The following keywords are used in the game:

- **Alliance**: When a player declares their intention to play an alliance card, any player(s) may help pay the costs for that card. (**See**: Alliance)
- **Assault**: When a character makes a basic thwart against a scheme with the assault keyword, that character uses its ATK instead of its THW. (**See**: Assault)
- **Form**: A card with the form keyword grants an identity a unique form. (**See**: Form)
- **Guard**: While a minion with guard is engaged with a player, that player cannot attack the villain. (**See**: Guard)
- **Hinder X**: A card with the hinder X keyword enters play with X threat on it. (**See**: Hinder X)
- **Incite X**: When a player reveals a card with incite X, that player places X threat on the main scheme. (**See**: Incite X)
- **Linked (Card Title)**: Cards with the linked keyword are set aside until the card named in parentheses brings the linked card into the game. (**See**: Linked (Card Title))
- **Overkill**: Excess damage from attacks with overkill are dealt to the identity or villain. (**See**: Overkill)
- **Patrol**: While a minion with patrol is engaged with a player, that player cannot thwart the main scheme. (**See**: Patrol)
- **Peril**: While a player is resolving a card with peril, other players cannot help that player. (**See**: Peril)
- **Permanent**: Cards with permanent cannot leave play. (**See**: Permanent)
- **Piercing**: Attacks with piercing discard tough status cards from the attacked character before damage is dealt. (**See**: Piercing)
- **Quickstrike**: After this enemy engages a player, it immediately attacks that player if they are in hero form. (**See**: Quickstrike)
- **Ranged**: Attacks with ranged ignore retaliate. (**See**: Ranged)
- **Requirement (Resources)**: A card with the requirement keyword cannot be played unless each resource of the specified type is spent while paying for that card’s cost. (**See**: Requirement (Resources))
- **Restricted**: A player cannot control more than two restricted cards at a given time. (**See**: Restricted)
- **Retaliate X**: After a character with retaliate X is attacked, deal X damage to the attacker. (**See**: Retaliate X)
- **Setup**: Cards with setup start the game in play. (**See**: Setup)
- **Stalwart**: Characters with Stalwart cannot be stunned or confused. (**See**: Stalwart)
- **Steady**: A character with the steady keyword is not stunned or confused unless it has two stunned or confused status cards, respectively. (**See**: Steady)
- **Surge**: After a player reveals a card with surge, that player reveals an additional encounter card. (**See**: Surge)
- **Team-Up**: Cards with team-up cannot be played unless both characters listed by the keyword are in play. (**See**: Team-Up)
- **Teamwork (Trait)**: After a minion with teamwork enters play and engages a player, if there is at least one other minion that shares the specified trait in play, the minion that just entered play activates against the player it is engaged with. (**See**: Teamwork (Trait))
- **Temporary**: A card with temporary must be discarded from play at the end of the round. (**See**: Temporary)
- **Toughness**: When a character with toughness enters play, place a tough status card on it. (**See**: Tough/ Toughness)
- **Uses (X “type”)**: When a card with uses enters play, place X all-purpose counters from the token pool on that card. After the last all-purpose counter is removed from a card with uses *(and the effect resolves)*, discard that card. (**See**: Uses (X “type”))
- **Victory X**: When a card with victory X is defeated, add it to the victory display. (**See**: Victory X)
- **Villainous**: When a minion with villainous activates, give it a boost card. (**See**: Villainous)

### Status Cards
<!-- RR v1.8 p.41 -->

Status card represent different states a character may find themselves in during the game.

The following status cards are used in the game. When a character is given a status card, take a status card of the specified type from the pool and place it on that character.

- A character cannot have more than one status card of each type at a time.
  - Characters with the steady keyword can have one additional confused status card and one additional stunned status card.
- Status card abilities have timing priority over all conflicting triggered abilities.

The three status card types are:

**Confused** — A confused status card cancels a character’s next scheme or thwart. **See**: Confuse, Confused.

**Stunned** — A stunned status card cancels a character’s next attack. **See**: Stun, Stunned.

**Tough** — A tough status card prevents a character from taking damage. **See**: Tough.

**See also**: Ally, Attack (Enemy Activation), Attack (Player Ability Type), Basic Power, Damage, Form, Identity, Minion, Replacement Effect, Triggered Ability, Villain

## Initiating, Costs and Targets

### Initiating Abilities
<!-- RR v1.8 p.24 -->

When a player wishes to play a card or initiate a triggered ability, that player first declares their intent. Then, the player checks the following conditions in order:

1. If playing a card, the player places that card faceup on the table in front of them. *(This card is not in play.)*
2. Check play restrictions: can the card be played, or the ability initiated, at this time?
   - If the card or ability specifies one or more targets, check that it has at least one valid target. If the card or ability does not have at least one valid target, it cannot be played or initiated.
   - If the card or ability has a form requirement *(for example, “Hero form only” or “**Hero Action**”)*, the form of the player playing that card or initiating that ability is checked now.
3. Determine the cost *(or costs)* to play the card or initiate the ability and the player’s ability to pay them, taking modifiers into account.
   - If a card has a resource cost of X, the player playing that card chooses the value of X during this step.

If both conditions are met, follow these steps in order:

4. Apply any modifiers to the cost(s).
5. Pay the cost(s). If this step is reached and the cost(s) cannot be paid, abort this process without paying any costs.
6. The card commences being played, or the effects of the ability attempt to initiate.
7. The card is played or the ability *(if not canceled in the previous step)* resolves. The card enters play or, if it is an event card, its effects resolve and it is then placed in its owner’s discard pile.
   - If any of the above steps would make the triggering condition of an interrupt ability true, that ability may be initiated just before that triggering condition becomes true.
   - If any of the above steps would make the triggering condition of a response ability true, that ability may be initiated immediately after that triggering condition becomes true.
   - If the ability being initiated is on a card that is in play, the sequence does not stop from completing if that card leaves play during this sequence unless the card leaving play prevents a required cost from being paid.

**See also**: Ability, Cost, Play Restrictions and Permissions, Target

### Play Restrictions and Permissions
<!-- RR v1.8 p.33 -->

Many cards and abilities contain specific instructions pertaining to when or how they may or may not be used, or to specific conditions that must be true in order to use them.

- In order to use an ability or play a card, all of its play restrictions must be observed.
- A permission is an optional play restriction, which allows a player to play a card or use an ability outside the timing or specifications provided by the game rules. *For example, a permission might allow an ally card to be played from a player’s discard pile.*

**See also**: Ability, Play, Player

### Cost
<!-- RR v1.8 p.13 -->

A card’s resource cost is the numerical value that must be paid to play the card. Some abilities have a cost described in the ability text that must be paid to use the ability.

- A cost arrow icon *(→)* in ability text distinguishes a cost from an effect, in a “pay cost → resolve effect” format.
  - Text indicating the timing of an interrupt or response trigger that precedes a cost arrow is not considered part of the cost.
- A resource cost with the per-player icon ([per player]) is multiplied by the number of players who **started** the scenario.
  - If a cost with the per-player icon is reduced, the total cost of the card is reduced, not the value that is multiplied by the number of players.
- To pay a resource cost, a player spends resources that they generate by discarding cards from their hand or by using “**Resource**” card abilities.
  - Resources generated to pay for an ability on a card are considered to have been paid for that card.
- While paying a cost, a player is permitted to generate resources beyond the specified cost.
  - Resources generated beyond the specified cost are considered to have been overpaid for that cost and were not paid for that cost.
  - Any resources that are generated beyond the specified cost are lost after paying that cost.
- If multiple costs for a single card or ability require payment, those costs must be paid simultaneously.
  - A player generating resources for those costs chooses how to divide those resources between those costs. *(For example, a player paying costs for an event with a resource cost of 1 and an ability that reads “**Hero Action**: Spend X [energy] resouces →...” can spend a resource card that generates [energy] [energy] resources and use one of those icons to pay for the card’s resource cost and the other to pay for the cost before the arrow.)*
- An ability’s cost cannot be paid if that ability’s effect requires one or more targets and there is not at least one valid target.
- While a player is paying a cost, that player must pay costs with cards and/or game elements they control.
  - If a cost uses the word “choose,” the player can choose targets they do not control.
  - If a cost targets a “friendly” card, the player can target cards they do not control.
- If a cost requires a game element that is not in play, the player paying the cost may only use game elements that are in their own out-of-play areas.
- A cost requiring “any number” or “up to” some number of game elements requires a minimum of one such game element.
- Some card abilities may reference an “additional cost.” A player must pay all additional costs simultaneously with the cost that is being added to, even if multiple cards or abilities are adding separate additional costs. A player cannot pay the original cost or any of the additional costs individually; if they cannot pay for all of the costs at once, then they do not pay any of the costs and the effect associated with the costs does not occur.
- If dealing damage is a cost, that cost is considered paid even if some or all of that damage is prevented.
- If taking damage is a cost, that cost is not considered paid unless all of that damage was taken. *(If any of the damage is prevented, then the cost has not been paid.)*

**See also**: Ability, Cost Arrow Icon, Game Element, Initiating Abilities, Keywords

### Cost Arrow Icon (→)
<!-- RR v1.8 p.14 -->

A cost arrow icon *(→)* in ability text distinguishes a cost from an effect, in a “pay cost → resolve effect” format. Non-bolded text before the cost arrow icon must be paid and/or resolved in full before the text after the cost arrow icon can be resolved.

- Responses to the text preceding the cost arrow icon resolve before the text following the icon resolves.

**See also**: Cost, Icons

### Target
<!-- RR v1.8 p.42 -->

If a game function or card ability is directed toward a game element *(such as an attack that deals damage to an enemy)*, that game element becomes the target of that function or ability for the duration of that function’s or ability’s resolution.

- *Examples of targets include but are not limited to: “the villain,” “a minion,” “an enemy,” “a scheme,” “a hero,” “an ally,” “a character,” “a player,” “you,” “a card.”*
- If an ability or game function requires one or more targets, that ability or game function can only be initiated if it has at least one valid target. *For example, an ability that says “deal 5 damage to a minion” cannot be initiated if there are no minions in play.*
  - Basic powers are game functions that require a valid target.
  - The phrase “choose a [game element]” indicates that one or more targets must be selected in order for an ability to initiate.
  - Abilities that cause a player to draw one or more cards always have a valid target so long as that player has at least one card in their deck.
- A target is valid for an ability or game function if any part of that ability can affect that target.
  - *Examples of effects on a target include but are not limited to: dealing/healing damage, adding/ removing threat, giving/removing a status card, exhausting/readying the target, defeating/ discarding the target.*
  - **Exception**: A character with an ATK, SCH, or THW of 0 can perform an activation or basic power using that value against a target that is otherwise valid for that activation or basic power. *(For example, a hero with a THW of 0 can perform a basic thwart against a scheme with threat on it.)*
  - The cost of an ability or game function is not considered when determining if that ability or game function can affect a target.
  - If an ability or game function has multiple effects on its target, the target is valid if at least one of those effects can affect the target.
  - A target is not valid for an ability if that ability would cause the target to perform a game function that another ability says the target cannot perform. *For example, a character with an attachment that says “attached character cannot ready” is not a valid target for a card that readies a character*.
  - Damage that is dealt but not taken *(for example, if the damage is prevented)* is considered to affect a target.
  - A target that “cannot take damage” is not a valid target for an ability or game function whose only effect on that target is to deal it damage.
  - A target that cannot be attacked is not a valid target for an attack-labeled ability.
  - A target that cannot be thwarted is not a valid target for a thwart-labeled ability.
- An ability or game function that targets multiple game elements of a specific type *(for example, “each enemy”)* can be initiated as along as at least one of those game elements is a valid target.
  - That ability or game function does not resolve against any of those game elements that is not a valid target.
  - *For example, the crisis ([crisis]) icon prevents threat from being removed from the main scheme. An ability that says “remove 1 threat from each scheme” can be used while there is a crisis icon in play if there is at least 1 scheme from which threat can be removed. In this case, no threat would be removed from the main scheme.*
  - An ability that refers to a future target *(i.e. “the next card you play”)* does not require a target to initiate.
  - An ability with a search effect requires only a searchable game area in order to initiate.

**See also**: Ability, Choose (Game Element), Cost, Game Element, Labeled Ability

### Choose (Game Element)
<!-- RR v1.8 p.12 -->

The phrase “choose a [game element]” *(such as an ally, a minion, or a scheme)* indicates that a player must select a game element that meets the specific requirements of an ability.

- The player resolving the ability that uses the word “choose” is the player who makes the choice specified by the card.
- If a player card ability requires the choosing of one or more targets, and there are no valid targets for any part of the ability, the ability cannot be initiated.
- If multiple targets are required to be chosen by the same player, simultaneously choose as many as are available, to a maximum of the specified number.
  - The same target cannot be chosen multiple times this way.
- An effect that can choose “any number” of targets does not successfully resolve if zero of those targets are chosen.

**See also**: Ability, Game Element, Player, Target

### Choose (Option)
<!-- RR v1.8 p.12 -->

Some abilities instruct a player to choose between multiple options. *For example, “Choose to either take 1 damage or discard 1 card from your hand.”*

- When an encounter card requires a player to choose an option, they cannot choose an option that requires one or more targets if there are no valid targets for that option.
- When a player card requires a player to choose an option, they cannot choose an option that cannot be at least partially resolved. This includes options that:
  - Have a cost the player cannot pay.
  - Require one or more targets and there are no valid targets.
- When a card requires a player to choose multiple options from a list, that player cannot choose the same option multiple times.

**See also**: Ability, Player, Target

### Limit
<!-- RR v1.8 p.27 -->

“Limit X per [period]” is a limit that appears on some player cards. These limits are card-specific. Each copy of an ability with such a limit may be used X times per the specified period, per instance of that ability.

- If an effect with a limit is canceled, the card is still considered to have been played or the ability initiated, and it counts toward the limit.

**See also**: Player Card

### Max, Maximum
<!-- RR v1.8 p.28 -->

The words “max” and “maximum” impose a maximum across all copies of a card *(by title)* for all players.

- ”Max X per [period]” imposes a maximum number of times that copies of that card can be played during the designated time period.
  - If a card with a maximum is canceled, the card is still counted toward the maximum.
- “Max X per deck” restricts the number of copies of that card that may be included in each player deck.
- “Max 1 per player” is player specific, and restricts the number of copies of that card that each player may control in play at a given time.
  - A player cannot take control of another copy of a “Max 1 per player” card they already control.
- “Max 1 per [game element]” restricts the number of copies of that card that can be attached to each indicated game element. *(For example, if an upgrade is “Max 1 per ally,” an ally can have at most one copy of that upgrade attached.)*
- “Max 1 per [instance]” restricts the number of times an ability can be triggered by a single instance of a triggering effect across all copies of the card with the maximum. *(For example, if an ability has the text “(Max 1 per event.),” only one card with that ability can be triggered per event played.)*
- The parenthetical “(to a maximum of [value])” within an ability imposes a maximum on that ability for the duration of that ability.

**See also**: Cancel, Limit, Appendix I: Deck Customization

### Resolve
<!-- RR v1.8 p.37 -->

Effects, abilities, event cards, and treachery cards are each resolved under the following conditions:

- An effect is resolved when it is applied to the game state.
- An ability is resolved when it is triggered and one or more of its effects resolve.
- An event card is resolved when it is played and one or more of its abilities resolve.
- A treachery card is resolved when it is revealed and one or more of its abilities resolve.
- Constant abilities are never considered resolved. They are always active while the card they are on is in play.
- Card types other than events and treacheries are not considered resolved, though abilities on those other card types can be resolved.
- If all of the effects of an ability are canceled, that ability is not considered to have resolved.
- If all of the abilities of an event or treachery are canceled, that card is not considered to have resolved.

**See also**: Ability, Cancel, Event, Treachery

## Ordering and Simultaneous Effects

### Simultaneous Resolution
<!-- RR v1.8 p.40 -->

If two or more effects with the same bold timing trigger would resolve simultaneously, the first player determines the order in which the effects resolve.

**See also**: “Each Player”, Triggered Ability

### First Player
<!-- RR v1.8 p.19 -->

A first player is determined by the players at the beginning of the game.

The first player token is used to indicate which player is the first player. At the end of the round *(during step five of the villain phase)* the first player token passes to the next clockwise player, who becomes first player for the next round.

If the first player is eliminated, the first player token immediately passes clockwise to the next player.

The players as a group are encouraged to work together, but the first player decides the following:

- If an encounter card targets a specific player or card, and there are multiple eligible targets, the first player selects among the eligible options.
- If an encounter card requires a card ability to be resolved, a game function to be performed, or a choice to be made but does not specify which player should act, the first player does so.
- If two or more effects would resolve simultaneously, the first player decides the order in which to resolve them.

The first player has timing priority in the following situations:

- The first player has the first opportunity to use an interrupt at each appropriate game moment. Interrupt opportunities then proceed among the remaining players in player order.
- The first player has the first opportunity to use a response at each appropriate game moment. Response opportunities then proceed among the remaining players in player order.

**See also**: In Player Order, Player, Player Elimination, Player Phase

### In Player Order
<!-- RR v1.8 p.24 -->

If players are instructed to perform a sequence “in player order,” the first player performs their part of the sequence first, followed by the other players in clockwise order.

- If a sequence performed in player order does not conclude after each player has performed their part of the sequence once, the sequence of opportunities continues in a clockwise manner until it is complete.
- The phrase “next player” always refers to the next *(clockwise)* player in player order.

**See also**: Find, Player, Player Phase, Player Turn

### “Each Player”
<!-- RR v1.8 p.17 -->

When each player is instructed to resolve an effect, each player resolves that effect one at a time.

- If the effect does not specify what order the players resolve the effect in, the first player decides the order.
- If a player cannot fully resolve that effect, they resolve it as fully as possible. *For example, if each player is instructed to discard a certain number of cards from the encounter deck and the deck empties while one player is discarding from it, that player stops discarding as soon as the deck is emptied. After the deck is reset (and an acceleration token is placed next to the main scheme deck), the remaining players resume discarding from it.*

**See also**: Choose (Game Element), Player

### “Would”
<!-- RR v1.8 p.48 -->

The word “would” is used to define the triggering condition of some interrupt abilities, and establishes a higher timing priority for those abilities than interrupts to the same triggering condition without the word “would.”

- If an interrupt to a triggering condition that would occur changes the nature of that which is about to occur *(such as through a replacement effect)*, no further interrupts to the original trigger may be used since the resolution of that trigger is no longer valid.

*For example, an interrupt that states “when a character would be defeated” triggers before an interrupt that states “when a character is defeated.”*

**See also**: Interrupt, Replacement Effect, Triggered Ability

### Replacement Effect
<!-- RR v1.8 p.37 -->

A replacement effect replaces a specified effect with a different effect. Most replacement effects are interrupt abilities in the format of “when [triggering condition] would happen, do [replacement effect] instead.”

- When an effect is replaced, it is no longer considered imminent and no further interrupts or responses to that effect can be triggered.

**See also**: Ability, Alteration Effect, Cancel, Interrupt, “Otherwise”, Triggered Ability, “Would”

### Cancel
<!-- RR v1.8 p.11 -->

Some card abilities can cancel card or game effects.

- Cancel abilities interrupt the initiation of effects and prevent them from resolving.
- Anytime the effects of an ability are canceled, the ability *(apart from its effects)* is still regarded as initiated, and any costs are still paid. Only the effects are prevented from initiating, and do not resolve.
- If the effects of an event card are canceled, the card is still considered played, and it is discarded.
- If the effects of a treachery card are canceled, the card is still regarded as revealed, and it is still placed in the encounter discard pile.
- Cancel effects are considered a subtype of replacement effect, with the canceled effect being replaced with no effect.
  - Abilities dependent on the canceled effect cannot trigger as the canceled effect is not considered to have occurred.

**See also**: Ability, Replacement Effect

### Delayed Effect
<!-- RR v1.8 p.16 -->

Some abilities contain delayed effects. Such abilities specify a future timing point, or indicate a future condition that may arise, and dictate an effect that is to happen at that time.

- Delayed effects resolve automatically and immediately after their specified timing point or future condition occurs or becomes true, and before responses to that point or condition may be used.
  - Delayed effects have the same timing priority as constant effects.
- When a delayed effect resolves, it is not treated as a new triggered ability, even if the delayed effect was originally created by a triggered ability.

**See also**: Ability

### Lasting Effects
<!-- RR v1.8 p.26 -->

Some card abilities create effects or conditions that affect the game for a specified duration *(such as “until the end of the phase” or “until the end of this attack”)*. Such effects are known as lasting effects.

- A lasting effect persists beyond the resolution of the ability that created it, for the duration specified by the effect. The effect continues to affect the game for the specified duration whether or not the card that created the lasting effect is in play.
- For the specified duration of a lasting effect, it is treated as if it was a constant ability and has the same timing priority as a constant ability.
- Lasting effects update whenever the game state updates. *(For example, a lasting effect of “until the end of the phase, your hero gets +1 ATK for each minion in play” changes the affected hero’s ATK any time a minion enters or leaves play during that phase.)*
- If a card enters play *(or changes status to meet the criteria of a specified set of affected cards)* after the creation of a lasting effect, it is still affected by that lasting effect.
- A lasting effect expires as soon as the timing point specified by its duration is reached. This means that an “until the end of the round” lasting effect expires just before an “at the end of the round” ability or delayed effect may initiate.
- A lasting effect that expires at the end of a specified time period can only be initiated during that time period.

**See also**: Ability, Enters Play

### Modifiers
<!-- RR v1.8 p.29 -->

The game constantly checks and *(if necessary)* updates the count of any variable quantity that is being modified.

Any time a new modifier is applied or removed, the entire quantity is recalculated from the start, considering the unmodified base value and all active modifiers.

- The “per player” icon ([per player]) is not considered a modifier and is applied before any modifiers are applied.
- The calculation of a value treats all modifiers as being applied simultaneously. However, while performing the calculation, all additive and subtractive modifiers are calculated before doubling and/or halving modifiers are calculated.
- If a value is “set” to a specific number, the set modifier overrides all non-set modifiers. If multiple set modifiers are in conflict, the most recently resolved set modifier takes precedence.
- After all active modifiers have been taken into account, if a value is below zero, it is treated as zero: a card cannot have “negative” icons, attributes, traits, cost, or keywords.
- Fractional values are rounded up after all modifiers have been applied.
- If a card ability causes a character to “get” a statistic *(such as +1 ATK or 4 hit points)*, the ability modifies the character’s statistic while it is active.
  - If such an ability expires or otherwise becomes inactive, the modified statistic reverts to the value it would have without the modifier.
- A value of a dash *(–)* cannot be modified.

**See also**: Base Value, Dash (Value), Printed

## Ability Wording

### Alteration Effect
<!-- RR v1.8 p.7 -->

An alteration effect modifies the resolution of an ability that precedes it. Types of alteration effects include:

**Additional** — The word “additional” denotes a modifier to an ability or game state. The additional modifier is resolved simultaneously with any ability it is modifying and under the same conditions as that ability. *(For example, Repulsor Blast reads: “**Hero Action*** (attack)*: Deal 1 damage to an enemy and discard the top 5 cards of your deck. For each printed* [energy] *resource discarded this way, deal 2 additional damage to that enemy.”)*

**Already** — The word “already” denotes the resolution of an alternate ability if a specific condition is met. The “already” effect checks if this condition is met before the preceding ability attempts to resolve. If so, the “already” effect resolves instead. *(For example, I’m Tough reads: “**When Revealed**: Give Rhino a tough status card. If Rhino already has a tough status card, this card gains surge.”)*

**Each Time** — The phrase “each time” denotes a temporary interruption to a resolving ability. When a condition is met that is specified by the “each time” effect, the resolution of the preceding ability halts, the “each time” effect resolves in its entirety, then the preceding ability continues resolving. *(For example, “You Dare Oppose Me?“ reads: “**When Revealed**: Discard the top 5 cards of the encounter deck. Each time a card belonging to the Kree Fanatic set is discarded this way, deal that card to yourself as a facedown encounter card.”)*

**This/That Activation/Attack/Thwart** — Effects that refer to “this” or “that” activation, attack, or thwart denote a modifier to that activation, attack, or thwart. *(Examples: “That attack gains overkill.” “Give the villain an additional boost card for this activation.”)*

- If “this/that activation/attack/thwart” is part of a condition for an effect, it does not make that effect an alteration effect. *(For example, the following is not an alteration effect: “If this attack defeats that enemy, heal 2 damage from your hero.”)*

**See also**: Ability, Activation, Modifiers, Replacement Effect, Target

### “And”
<!-- RR v1.8 p.7 -->

The word “and” indicates that two or more effects within an ability resolve simultaneously.

- Individual effects connected by “and” are not dependent on each other. Resolve as much of each effect as possible.
- Each effect connected by “and” can be canceled or prevented independently.

**See also**: Ability, Cancel, Prevent

### “Then”
<!-- RR v1.8 p.44 -->

If the effect text of an ability includes the word “then”, the text preceding the word “then” must be fully true or resolved before the remainder of the effect described after the word “then” can be resolved.

- If the pre-”then” text of an effect fully resolves, the post-”then” text of the effect must also attempt to resolve.
- If the pre-”then” text of an effect does not fully resolve, the post-”then” text does not attempt to resolve.

### “Otherwise”
<!-- RR v1.8 p.31 -->

Effects beginning with “otherwise” resolve only if the preceding effect was not resolved.

- An “otherwise” effect will resolve if one or more of the following are true of the preceding effect:
  - It has a condition that is not true. *(For example, an ability reads: “If you are in hero form, take 2 damage. Otherwise, place 2 threat on the main scheme.” The “otherwise” portion resolves if the player is not in hero form.)*
  - It has an effect that cannot at least partially resolve. *(For example, an ability reads: “Discard 2 cards from your hand. Otherwise, exhaust your identity.” The “otherwise” portion resolves if the player cannot discard at least 1 card from their hand.)*
- If “otherwise” is preceded by a semicolon, the “preceding effect” refers to the effects before the semicolon in the same sentence. If the “otherwise” effect is its own sentence, the “preceding effect” refers to the sentence coming directly before the “otherwise” sentence.

**See also**: Replacement Effect, Target

### “For Each”
<!-- RR v1.8 p.20 -->

“For each” indicates an effect is repeated based on the number of a countable game element.

- If an effect with “for each” requires a target, that effect applies to a single target unless the “for each” clause includes a “choose” instruction.
  - *For example, an effect that reads “For each treachery looked at this way, remove 1 threat from a scheme” removes threat from a single scheme. Alternatively, an effect that reads “For each upgrade you control, choose an enemy and deal 2 damage to it” allows you to choose a different enemy for each upgrade you control.*
- If a “for each” effect without a “choose” instruction deals damage or removes threat, it is considered a single instance of damage dealt or threat removed, respectively.
- If a “for each” effect has a “choose” instruction, each iteration of that choice is considered a separate instance of that effect, even if the same target is chosen multiple times.
  - The game state updates after each instance *(for example, if a minion or side scheme is defeated)*.
  - Responses can be triggered after each instance.
  - *For example, if a player is engaged with a guard minion and uses an effect that says “for each resource you spent this way, choose an enemy and deal 2 damage to it,” that player can defeat the guard minion with one instance of 2 damage, making the villain a valid target for further instances. That player can trigger a response ability like “After you deal damage to an enemy” after each instance.*
- If another ability modifies a “for each” effect, that modifier is applied to each instance of the “for each” effect. *(For example, Flurry of Blades is an event that has the effect: “For each Psi-Katana, choose an enemy and deal 2 damage to it.” If modified by an effect that says “that event deals 1 additional damage,” Flurry of Blades deals 3 damage to each chosen enemy.)*

**See also**: Ability, Target

### “After”
<!-- RR v1.8 p.6 -->

The word “after” refers to a game occurrence that just concluded. Many response abilities use the term “after” to specify the time at which they can be used.

**See also**: Ability, Response

### May
<!-- RR v1.8 p.28 -->

The word “may” indicates that a specified player has the option to resolve the text that follows. If no player is specified, the option is granted to the controller of the card with the ability in question.

**See also**: Player

### “Cannot”
<!-- RR v1.8 p.11 -->

The word “cannot” is absolute and cannot be countermanded by other abilities or effects.

- If two abilities conflict, the ability with “cannot” takes precedence.
- If two rules conflict, the rule with “cannot” takes precedence.
- An ability can override a rule with “cannot” per the Golden Rules.

**See also**: The Golden Rules, Target

### Qualifiers
<!-- RR v1.8 p.35 -->

If ability text includes a qualifier followed by multiple terms, the qualifier applies to each item in the list, if applicable. *For example, in the phrase “each ready character and attachment” the word “ready” applies to both “character” and “attachment.”*

### Referential Ability
<!-- RR v1.8 p.36 -->

Some abilities refer to specific cards by name. These are called referential abilities. If an ability refers to a title shared by multiple cards in the game, that ability refers only to the card(s) that match the criteria highest in this list:

1. The card on which the referential ability is printed.
   - Such an ability is a self-referential ability.
   - *For example, the basic ally Spider-Man’s ability reads: “**Response**: After Spider-Man attacks or thwarts, choose another Web-Warrior character → ready that character.” This ability refers to the card it is printed on and does not trigger when another card called “Spider-Man” attacks or thwarts.*
2. Cards associated with the same identity, including:
   - The identity card
   - Identity-specific cards
   - The identity’s obligation cards
   - The identity’s nemesis set
   - Cards belonging to a side deck used by the identity.
3. Player cards (if the ability is on a player card) or encounter cards (if the ability is on an encounter card).

**See also**: Ability, Character, Classifications, Identity, Identity-Specific Card

### You, Your
<!-- RR v1.8 p.49 -->

While resolving card abilities, if the word “you” **can** be resolved as referring to the player’s identity, it must be resolved as such. This includes but is not limited to:

- If a sentence uses “you” and/or “your” multiple times, all instances in that sentence refer to the same identity or player.
- If a card deals damage to “you,” or “you” take damage, the player resolving that damage applies it to the hit point dial of their identity.
- If a card ability deals indirect damage to “you,” or “you” take indirect damage, the player resolving that damage can assign that damage to characters in play under their control.
- If a card ability exhausts “you,” the player resolving that card ability exhausts their identity.
- If a card ability places a status card on “you” *(such as “you are stunned”)*, the player resolving that card ability places that status card on their identity.
- If a card ability triggers from a game function that “you” perform *(such as “after you attack and defeat an enemy”)*, the player resolving that card ability must resolve that card ability as if the identity they control performed that game function, if able. *(For example, if an ability triggers “after you attack and defeat a minion,” it triggers after the controlling player’s identity attacks and defeats a minion but not when an ally under that player’s control attacks and defeats a minion.)*
- **Exception**: For abilities that trigger “after [enemy] attacks you,” “you” refers to the attacked player, even if that player defended with an ally.

While resolving card abilities, if the word “you” **cannot** be resolved as referring to the player’s identity, it must be resolved as referring to the player. This includes but is not limited to:

- If a game step or card ability discards cards from “your” hand, the player resolving that game step or card ability discards the cards from their hand.
- If a card ability searches “your” deck, the player resolving that ability searches for cards in their deck.
- If a game step or card ability performs a game function against a non-identity card “you” control *(such as exhausting, discarding, setting aside, etc.)*, the player resolving that game step or card ability performs that game function against the card in their play area.
- If a card ability triggers from a game function that “you” perform and cannot be interpreted in such a way that would refer to the resolving player’s identity *(such as “after you discard cards from your hand”)*, the player resolving that card ability must personally resolve and perform the game function(s) of that ability.

Some player cards are considered to be an extension of a player’s identity. When an action and/or ability is resolved on a card type that is an extension of an identity, that action and/or ability is considered to be performed by that identity.

Cards that are considered to be an extension of a player’s identity are:

- **Events** — Attacks, thwarts, defenses, action abilities, and triggered abilities that resolve from a player playing an event are also considered to be performed by that player’s identity.
- **Resources** — Attacks, thwarts, defenses, and triggered abilities that resolve from a player spending a resource card are also considered to be performed by that player’s identity.
- **Upgrades** — Unless attached to a different friendly character, upgrade cards are considered to be an extension of the controlling player’s identity. Attacks, thwarts, defenses, action abilities, and triggered abilities that resolve on such upgrades are also considered to be resolved by that player’s identity.

Cards that are **not** considered to be an extension of a player’s identity are:

- **Allies** — Attacks, thwarts, defenses, action abilities, and triggered abilities that resolve from allies in play under a player’s control are **not** considered to be performed by that player’s identity.
- **Encounter cards** — Triggered abilities that resolve from encounter cards are **not** considered to be performed by that player’s identity, whether those abilities are optional or forced.
- **Player Side Schemes** — Triggered abilities that resolve from player side schemes in play under a player’s control are **not** considered to be performed by that player’s identity.
- **Supports** — Attacks, thwarts, defenses, action abilities, and triggered abilities that resolve from supports in play under a player’s control are **not** considered to be performed by that player’s identity.

**See also**: Ability, Ally, Event, Identity, Player, Player Card, Player Deck, Player’s Play Area, Resource, Support, Upgrade
