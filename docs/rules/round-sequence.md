# Round Sequence, Setup, Attacks, Schemes and Boosts

> Source: *Marvel Champions: The Card Game* Rules Reference v1.8 (FFG, 2026), transcribed from the official PDF.
> Text is verbatim except for formatting; icons are written as `[energy]`, `[mental]`, `[physical]`, `[wild]`, `[boost]`, `[star]`,
> `[per player]`, `[acceleration]`, `[amplify]`, `[crisis]`, `[hazard]`, `[consequential damage]`, `[unique]`; `→` is the cost arrow.
> `<!-- RR v1.8 p.N -->` gives the page of the entry in the PDF.

This file collects, in game order, the Rules Reference text that defines the flow of a game: setup, the
player phase, the villain phase, enemy activations (attacks and schemes), boosts, defending, dealing and
revealing encounter cards, and damage/defeat. Everything under the `###` entry headings is a verbatim copy of
the matching glossary entry (the glossary files are the canonical copy); only the grouping is ours.

Note: RR v1.8 has no separate "timing and sequence" appendix. The step-by-step procedures live in the glossary
entries reproduced here: **Appendix II: Setup**, **Player Turn**, **End of Player Phase**, **Villain Phase**,
**Attack (Enemy Activation)**, **Scheme (Enemy Activation)**, **Reveal**, **Main Scheme** (advancing),
**Initiating Abilities**, **Damage** (damage resolution order) and **Player Elimination**.

## Quick map (non-normative summary)

```
Setup (Appendix II, steps 1-16)
Round:
  Player phase
    each player, in player order, takes a turn (Player Turn options, any order;
      change form max once per turn)
    End of Player Phase: 1 discard (optional, then down to hand size, in player order)
                         2 draw up to hand size (simultaneous)
                         3 ready all player cards + exhausted encounter cards
                         4 "until the end of the [player] phase" effects end
                         5 "when/after the [player] phase ends" effects
  Villain phase
    1 Place Threat        (acceleration field + acceleration icons/tokens)
    2 Enemies Activate    (per player in player order: villain, then that player's engaged minions)
                          hero form -> attack, alter-ego form -> scheme
    3 Deal Encounter Cards (1 each + 1 per hazard icon, extra cards in player order;
                          heroic mode adds X per player)
    4 Reveal Encounter Cards (player order; each player's cards in the order dealt)
    5 Pass First Player Token
    6 End of Villain Phase and Round ("until end of phase/round" effects end, then
                          "when/after the phase/round ends" effects)
Enemy attack: 1 give boost card (villain / villainous) -> 2 declare defender -> 3 flip boost cards
  (flip, resolve Boost abilities, +ATK per boost icon, discard, repeat) -> 4 calculate damage
  (ATK incl. boosts, minus hero DEF if a hero defended) -> 5 deal damage -> 6 forced then
  non-forced "after attack/attacked/defends/damage" abilities
Enemy scheme: 1 give boost card -> 2 flip boost cards (+SCH per boost icon) -> 3 place threat = SCH
```

## Round Overview
<!-- RR v1.8 p.4 -->

The following is an overview of a game round, and the glossary entries that cover each part of the game round.

1. Player phase begins. **See**: Player Phase
2. Each player takes a turn. **See**: Player Turn
3. Player phase ends. **See**: End of Player Phase
4. Villain phase begins. **See**: Villain Phase
5. Place threat on main scheme. **See**: Main Scheme
6. Villain and minions activate. **See**: Activation, Attack (Enemy Activation), Scheme (Enemy Activation)
7. Deal encounter cards. **See**: Deal
8. Reveal and resolve encounter cards. **See**: Reveal
9. Pass the first player token. **See**: First Player
10. End the round. Proceed to step one of the next game round.

## Setup (Appendix II)
<!-- RR v1.8 p.51 -->

To set up a game, perform the following steps in order:

1. **Select Identities.** Each player selects one identity, placing their alter-ego side face up.
2. **Set Hit Points.** Each player sets their hit point dial equal to the starting hit points of their character, found at the bottom of their identity card.
3. **Select First Player.** As a group, the players select a first player and place the first player token in front of that player.
4. **Set Aside Obligations.** For each identity being played, set aside their obligation card.
5. **Set Aside Nemesis Sets.** For each identity being played, set aside their nemesis and the encounter cards of that nemesis.
6. **Shuffle Player Decks.** Each player shuffles their player deck.
7. **Collect Tokens and Status Cards.** Collect a pool of damage tokens, threat tokens, acceleration tokens, and all-purpose counters within reach of each player. Place stacks of stunned, confused, and tough status cards near this pool.
8. **Select Scenario.** Select a scenario and put its villain deck and main scheme deck into play near the center of the play area.
9. **Set the Villain’s Hit Points.** Set the villain’s hit point dial to the value indicated by the villain card.
10. **Create the Encounter Deck.** Shuffle the encounter sets listed on side 1A of the main scheme card with the obligation cards set aside during setup step four to create the encounter deck.
11. **Put Setup Cards Into Play.** Search each deck and the set aside area for any cards with the setup keyword and put them into play.
12. **Resolve Scenario Setup and When Revealed Abilities.**
    a. Resolve any “**Setup**” abilities on main scheme card 1A.
    b. Flip the main scheme card to side 1B and resolve any “**When Revealed**” abilities on that side.
    c. Resolve any “**Setup**” and “**When Revealed**” abilities on the villain.
13. **Campaign Setup.** If playing in campaign mode, resolve the Setup campaign instructions listed for the scenario in its associated rulebook.
14. **Draw Cards.** Each player draws cards from their deck until they have cards equal in number to their hand size *(including modifiers)*, as listed near the bottom of their identity card.
15. **Resolve Mulligans.** Each player may discard any number of cards from hand, and then draw up to their starting hand size. *(Do not shuffle these discarded cards back into their decks at this time.)*
16. **Resolve Player Setup Abilities.** Resolve any “**Setup**” abilities on player cards in play.

The game is now ready to begin.

## Player Phase

### Player Phase
<!-- RR v1.8 p.34 -->

During the player phase, each player *(in player order)* takes one turn.

After each player has taken a turn, the players discard down or draw up to their hand size and ready each exhausted card.

- Effects that last “until the end of the [player] phase” end after players draw up to their hand size and all cards are readied. Then, effects that resolve “when/ after the player phase ends” are resolved.

**See also**: End of Player Phase, In Player Order, Player, Player Turn

### Player Turn
<!-- RR v1.8 p.34 -->

During their turn, a player may perform the following options, in any order. Each option, except “change form,” may be performed as many times as the player is able, so long as they are able to pay the required costs.

- **Change form** from hero to alter-ego, or from alter-ego to hero. **This option may only be performed once each turn.**
- **Play** an ally, upgrade, support, or player side scheme card from hand.
- **Use** their alter-ego’s basic recovery *(if in alter-ego form)* or their hero’s basic attack or thwart power *(if in hero form)*.
- **Use** an ally card they control in play to attack an enemy or thwart a scheme.
- **Trigger** an “**Action**” ability on:
  a. A card in play they control.
  b. An encounter card in play.
  c. Any card in play with text that allows that player to trigger its action ability.
  d. An event card in their hand *(by playing that event)*.
     - If the action ability is preceded by “Hero” or “Alter-Ego”, the player must be in the specified form in order to trigger the ability.
- **Ask** another player to trigger any “**Action**” ability that player could trigger on their own turn. The other player then decides whether or not to trigger the ability. *(Another player may offer to use an action during the active player’s turn, as well.)*

**See also**: Ally, Basic Power, Encounter Card, Event, Form, Ownership and Control, Play, Player, Player Phase, Support, Triggered Ability, Upgrade

### Form, Change Form
<!-- RR v1.8 p.21 -->

A player can be in either hero or alter-ego form at a given time. This is indicated by the player’s identity card.

- Once each round, during their turn, each player is permitted to change form by flipping their identity card.
- When a player changes form, only the form changes. The character retains their sustained damage, status cards, lasting effects, attached cards, tucked cards, tokens, and current state *(ready or exhausted)*.
- If a card ability causes a player to change forms, it does not count against the one voluntary form change the player is permitted during their turn that round.
- While a player is in hero form, card abilities that interact with their alter-ego do not interact with their identity.
- While a player is in alter-ego form, card abilities that interact with their hero do not interact with their identity.
- Cards with the “[type] form” keyword grant an identity unique forms.
  - These forms are in addition to the identity’s alter-ego and hero forms, and they come with their own conditions for changing into them.
  - When an identity changes their additional form, it does not count against the once-per-turn limit on flipping from hero to alter-ego (or vice versa), but it does count as changing form for the purpose of triggering card effects.
- Cards with the text “[type] form only” can only be played or put into play by a player whose identity is in the specified form.

**See also**: Flip, Identity, Keywords

### Play, Put into Play
<!-- RR v1.8 p.32 -->

Playing a card involves paying the card’s cost and placing the card in the play area. This causes the card to enter play *(or, in the case of an event card, to resolve its ability and be placed in the discard pile)*. Cards are played from a player’s hand.

Some abilities cause cards to be put into play. This bypasses the need to pay the card’s cost as well as any restrictions or prohibitions regarding playing that card. A card that is put into play enters play in its controller’s play area.

- Cards with the text “[type] form only” can only be played or put into play by a player whose identity is in the specified form.
- When an event card is played, place it on the table, resolve its ability, and place the card in its owner’s discard pile.
- A card that is put into play is not considered to have been played.
- When a card is put into play, its resource cost is ignored.
- Unless otherwise stated by the “put into play” effect, cards that are put into play must do so in a play area or state that matches the rules of playing the card.

**See also**: Enters Play, In Play and Out of Play, Initiating Abilities, Leaves Play, Play Restrictions and Permissions

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

### Basic Power
<!-- RR v1.8 p.10 -->

A basic power is a statistic that allows a character to perform a certain game function.

- There are five different basic powers:
  - Attack power *(ATK)* can be used by a character to perform a basic attack and deal damage to another character. Generally, heroes, allies, villains, and minions have attack power.
  - Thwart power *(THW)* can be used by a character to perform a basic thwart and remove threat from a scheme. Generally, heroes and allies have thwart power.
  - Defense power *(DEF)* can be used by a character to perform a basic defense and prevent damage from an attack. Generally, only heroes have defense power.
  - Recovery power *(REC)* can be used by a character to perform a basic recovery and heal damage from themself. Generally, only alter-egos have recovery power.
  - Scheme power *(SCH)* can be used by a character to perform a basic scheme and place threat on the main scheme. Generally, villains and minions have scheme power.

**See also**: Ally, Attack (Enemy Activation), Attack (Player Ability Type), Defend, Identity, Minion, Modifiers, Recover, Recovery, Scheme (Enemy Activation), Thwart

### Attack (Player Ability Type)
<!-- RR v1.8 p.10 -->

Some game effects and card abilities reference an attack. There are a few different ways an attack can occur:

- A hero or ally can use their basic attack power to attack an enemy. A character must exhaust to use this power. This deals damage equal to the character’s ATK value to the enemy.
  - A character can only initiate a basic attack if there is an enemy that can be attacked by that character or if that character is stunned.
  - An ability that allows a hero or ally to “make a basic attack without exhausting” can allow an exhausted character to make a basic attack.
- If a triggered ability is labeled as an attack—such as “**Hero Action** *(attack)*”—resolving that ability is considered to attack the specified target. Unless specified by the ability’s text, a hero does not exhaust when using such an ability.
  - An ability labeled as an attack is considered a single attack, even if that attack deals multiple instances of damage.
  - When an attack ability has its damage increased by another ability, each instance of damage in that attack ability that does not use the word “additional” is increased by the specified amount.
- If an ability says “Make the following X attacks in order,” followed by two or more instances of damage, each of those instances is considered a separate attack.
  - An ability that increases the damage of an attack only increases the damage of one of that ability’s attacks, though such an ability can be triggered separately for each attack.
- Hero and ally attacks can target any enemy, unless a card ability *(such as guard)* is preventing that enemy from being attacked.
- When an attack targets multiple enemies, the attacking character is considered to have attacked each of those enemies.
  - Each attacked enemy with the retaliate X keyword that is still in play after the attack resolves deals its retaliate damage to the attacking character.
- The order of resolution for abilities triggered by the resolution of an attack is as follows:
  <!-- sic: the source numbers this sub-list 7-9 -->
  7. Forced abilities *(such as the retaliate keyword)* with the following triggers (in any order):
     - “after [character] attacks [and damages/defeats] [an enemy/a minion]...”
     - “after [character] is attacked...”
  8. Non-forced abilities with the triggers listed above.
  9. Consequential damage (for allies).

**See also**: Ally, Basic Power, Damage, Defend, Enemy, Identity, Labeled Ability, Minion, Modifiers, Retaliate X, Target, Villain

### Thwart
<!-- RR v1.8 p.44 -->

Some game effects and card abilities reference a thwart attempt. There are a few different ways this can occur:

- A hero or ally can use their basic thwart power to thwart a scheme. A character must exhaust to use this power. This removes threat equal to the character’s THW value from the scheme.
  - A character can only initiate a basic thwart if there is a scheme with at least one threat for the character to remove or if that character is confused.
  - An ability that allows a hero or ally to “make a basic thwart without exhausting” can allow an exhausted character to make a thwart attempt.
- If a triggered ability is labeled as a thwart—such as “**Hero Action** *(thwart)*”—resolving that ability is considered to thwart the specified scheme. Unless specified by the ability’s text, a hero does not exhaust when using such an ability.
  - An ability labeled as a thwart is considered a single thwart, even if that thwart removes multiple instances of threat.
  - If an ability increases the amount of threat an ability labeled as a thwart removes and that ability removes multiple instances of threat, each of those instances that does not use the word “additional” is increased by the specified amount.

**See also**: Ally, Basic Power, Consequential Damage, Exhausted, Labeled Ability, Main Scheme, Modifiers, Side Scheme, Threat

### Recover, Recovery
<!-- RR v1.8 p.36 -->

Recovery is a basic power a player can use in alter-ego form. To recover, the player exhausts their alter-ego and heals a number of hit points equal to their REC value.

- An identity that has no damage to heal cannot perform a basic recovery.

**See also**: Basic Power, Form, Heal, Hit Points, Identity

### Consequential Damage ([consequential damage])
<!-- RR v1.8 p.13 -->

After an ally attacks, it takes consequential damage equal to the number of consequential damage icons *([consequential damage])* beneath its ATK field.

After an ally thwarts, it takes consequential damage equal to the number of consequential damage icons *([consequential damage])* beneath its THW field.

- Consequential damage is dealt to an ally after resolving abilities that are triggered by the ally attacking or thwarting.
- If the target of an ally’s basic power leaves play before that ally deals damage equal to its ATK or removes threat equal to its THW, the ally does not take consequential damage *(but still exhausts)*.
  - That ally is not considered to have attacked or thwarted for the purpose of other abilities.
  - *For example, Iron Fist’s ability reads: “**Interrupt**: When Iron Fist attacks an enemy, remove 1 mystic counter from him → stun that enemy and deal 1 damage to it.” If this effect defeats the enemy Iron Fist was attacking, the basic attack aborts as soon as the target leaves play, so Iron Fist does not take consequential damage from it.*

**See also**: Ally, Attack (Player Ability Type), Basic Power, Damage, Icons, Thwart

### Ally Limit
<!-- RR v1.8 p.7 -->

Each player is permitted to control a maximum of three allies in play at any given time. This is referred to as the “ally limit.”

A player may play or put into play allies beyond their ally limit. However, if a player ever controls a number of allies greater than their ally limit in play, they must immediately choose and discard from play ally cards they control until they have a number of allies in play equal to their ally limit. This occurs before abilities that resolve upon entering play.

**See also**: Ally, Discard, Enters Play

### End of Player Phase
<!-- RR v1.8 p.18 -->

To end the player phase, perform the following steps:

1. In player order, each player may discard any number of cards from their hand, and must discard down to their hand size if they have more cards than their hand size.
2. Each player simultaneously draws up to their hand size.
3. Each player simultaneously readies all of their cards. Ready each exhausted encounter card.
4. Any effects that last “until the end of the [player] phase” end.
5. Resolve any “when/after the [player] phase ends” effects.

**See also**: Discard, Draw, Hand Size, Lasting Effects, Player, Player Phase, Player Turn, Ready

### Hand Size
<!-- RR v1.8 p.21 -->

Each player checks their hand size at the end of the player phase, either discarding down to or drawing up to the number of cards indicated by their hand size value.

- When drawing up to their hand size, a player draws cards one at a time, checking after each card is drawn whether they are at their hand size.

**See also**: End of Player Phase, Modifiers

## Villain Phase

### Villain Phase
<!-- RR v1.8 p.47 -->

The steps of the villain phase are:

1. **Place Threat.** Place the amount of threat indicated in the main scheme’s acceleration field onto that scheme. If any acceleration icons or tokens are active, additional threat equal to the number of such icons and tokens is also placed at this time.
2. **Enemies Activate.** In player order, each player resolves the following:
   a. The villain activates against the player.
   b. Each minion engaged with the player activates against them, in the order of that player’s choice.
3. **Deal Encounter Cards.** Deal one encounter card to each player. Deal one additional card for each hazard icon on a card in play. These additional cards are dealt in player order.
4. **Reveal Encounter Cards.** The first player reveals each of their encounter cards, one card at a time in the order in which they were dealt, resolving each card based on its card type. Each player repeats this process in player order, until no dealt encounter cards remain.
5. **Pass First Player Token.** Pass the first player token to the next clockwise player.
6. **End of Villain Phase and Round.**
   a. Any effects that last “until the end of the [villain] phase” or “until the end of the round” end.
   b. Resolve any “when/after the [villain] phase ends” or “when/after the round ends” effects.

**See also**: Acceleration Icon, Activation, Attack (Enemy Activation), Deal, Engage, Find, Hazard Icon, In Player Order, Main Scheme, Minion, Player, Reveal, Scheme (Enemy Activation), Threat, Villain

### Main Scheme, Main Scheme Deck
<!-- RR v1.8 p.27 -->

Main scheme is an encounter card type. The main scheme represents the villain’s primary objective.

- During step one of the villain phase, place the amount of threat indicated in the main scheme’s acceleration field *(bottom-right corner)* on that scheme. This value is modified by all active acceleration tokens and icons.
- If the amount of threat on a main scheme is equal to or greater than its target threat value, that main scheme is completed and the main scheme deck advances.
  - **If the villain completes the final stage of the main scheme deck, the villain wins the game.**
  - If the main scheme advances other than through having threat on it equal to or greater than its target threat value, that main scheme is **not** considered completed.
- When the main scheme deck advances, follow these steps:
  1. Remove the top main scheme card from the game. Return all tokens (except acceleration tokens) that were on that card to the token pool and discard each card attached to it.
  2. Resolve any “**When Revealed**” ability on the “A” side of the new top card of the main scheme deck.
  3. Flip the top card of the main scheme deck to its “B” side, place threat on that card equal to its starting threat value, and resolve any “**When Revealed**” ability on that side of the card.
- When the main scheme deck advances, excess threat from the previous stage does not carry over to the new stage.
- When the main scheme deck advances, acceleration tokens on it carry over to the new stage.
- Main scheme cards cannot be discarded from play.

**See also**: Acceleration Icon, Acceleration Token, Card Types, Encounter Card, Removed from the Game, Target Threat, Threat, Villain, Villain Phase

### Acceleration Icon ([acceleration])
<!-- RR v1.8 p.5 -->

An acceleration icon represents additional forces that are advancing the villain’s nefarious plans.

During step one of the villain phase, place X additional threat on the main scheme, where X is the number of acceleration icons in play.

- Each acceleration icon is equivalent to the following constant ability: “During the Place Threat step of the villain phase, place 1 additional threat on the main scheme.”
- An acceleration icon can be removed from play by defeating the encounter card it is printed on.
- Accleration icons are not considered acceleration tokens, and vice versa.

**See also**: Icons, Main Scheme, Threat, Villain Phase

### Acceleration Token
<!-- RR v1.8 p.5 -->

Acceleration tokens are functionally equivalent to acceleration icons *(but are not considered acceleration icons)*. They are placed next to the main scheme as a reminder to add X additional threat to the main scheme during step one of the villain phase, where X is the number of acceleration tokens in play.

Acceleration tokens enter play through one of two effects:

- If the encounter deck is empty, place one acceleration token next to the main scheme.
- Card effects may instruct the players to add an acceleration token to play.
  - Acceleration tokens placed on cards other than the main scheme still add threat to the main scheme during step one of the villain phase.

Acceleration tokens on the main scheme cannot be removed from play. Unlike other tokens, when a main scheme card leaves play, the acceleration token does not get discarded.

- Acceleration tokens placed on cards other than the main scheme are removed from play when the card they are placed on leaves play.
- Accleration tokens are not considered acceleration icons, and vice versa.

**See also**: Component Limitations, Encounter Deck, Main Scheme, Villain

### Activation
<!-- RR v1.8 p.6 -->

There are two types of enemy activations: an attack activation and a scheme activation. Whenever an enemy attacks or schemes, it is considered to have activated.

- During step two of the villain phase, the villain activates once per player, in player order. If the identity of the player resolving the activation is in hero form, the villain initiates an attack against that player’s identity. If the identity of the player resolving the activation is in alter-ego form, the villain initiates a scheme.
- During step two of the villain phase, each minion engaged with a player activates against that player. If the identity of the engaged player is in hero form, the minion initiates an attack against that player’s identity. If the identity of the engaged player is in alter-ego form, the minion initiates a scheme.
- Each time the villain activates, give the villain one boost card from the encounter deck for that activation.
- Some card abilities can also cause enemies to attack or scheme. These are also considered activations.
- If multiple enemies activate against you simultaneously, resolve the villain’s activation first *(if any)* in the order of your choice, followed by minion activations in the order of your choice.
- If an activating minion leaves play, that minion’s activation ends immediately and no further steps of that activation resolve.
- An effect that initiates an enemy activation is considered resolved after that activation has fully resolved.
- If an effect initiates an activation during the resolution of another activation, the newly initiated activation resolves after the current activation has finished resolving.
  - If multiple activations are initiated this way, the first player decides the order in which they are resolved.
  - All abilities triggered by the initial activation resolve before subsequent activations initiate.

**See also**: Boost, Attack (Enemy Activation), Scheme (Enemy Activation), Minion, Villain, Villain Phase

### Attack (Enemy Activation)
<!-- RR v1.8 p.8 -->

An attack is a type of enemy activation. When an enemy initiates an attack, it targets a specific player, then resolves that attack against that player.

- Enemy attacks are always initiated against both a player and a character.
  - Normally the attacked character is the player’s hero, but abilities can instead cause an enemy to attack a player’s alter-ego or an ally that player controls. In all of these cases, the player is still considered attacked.
  - If a character other than the attacked character defends the attack, that character becomes the new target of that attack.
  - If a player other than the attacked player defends the attack with a character they control, that player becomes the new target of that attack.
  - Abilities that trigger “When/After [enemy] attacks you” are resolved when/after a player is attacked, regardless of which character they control was attacked. *(For example, Ultron I reads: “**Forced Response**: After Ultron attacks you, choose to either place 1 threat on the main scheme or put the top card of your deck into play facedown, engaged with you as a drone minion.” This effect resolves against the attacked player regardless of if that player used an ally to defend the attack.)*

To resolve an enemy attack, follow these steps:

1. **Give boost card:** If a villain, or a minion with the villainous keyword, is attacking, give it one facedown boost card from the encounter deck. *(If a minion without the villainous keyword is attacking, skip this step.)*
2. **Declare defender:** If a player wishes to defend, that player exhausts a hero or ally as the defender.
   - The defending character becomes the target character for the attack.
   - If a player other than the target player defends, the defending player becomes the target player for the attack.
3. **Flip boost cards:** Flip and resolve each of the attacking enemy’s boost cards, one at a time and in the order in which they were dealt, by doing the following:
   a. Flip the boost card faceup.
   b. Resolve any “**Boost**” abilities, indicated by the star icon in the boost area. *(All other abilities on the boost card are ignored.)*
   c. Increase the attacking enemy’s ATK value by one for each boost icon on the card.
   d. Discard the boost card.
   e. If the enemy has any boost cards remaining, repeat these steps with the next boost card.
4. **Calculate damage:** Determine how much damage will be dealt by the attack.
   - The base damage is equal to the attacking enemy’s ATK, including modifiers from abilities in play and boost icons resolved for the attack.
   - If a hero has been declared the defender of the attack, reduce the amount of damage dealt by that hero’s DEF value.
5. **Deal damage:** Deal the amount of damage calculated in the previous step, based on the following:
   - If a **hero was declared the defender** of the attack, the damage from the attack is dealt to that hero.
     - The defending hero is considered to have been attacked.
     - If a hero with a tough status makes a basic defense, the damage is first reduced by that hero’s DEF value. If the damage is reduced to 0, the hero keeps their tough status.
   - If an **ally was declared the defender** of the attack, all damage from the attack is dealt to the ally. *(If the ally is defeated by the attack, additional damage does not carry over to the identity.)*
     - The defending ally is considered to have been attacked.
     - If the defending ally leaves play prior to damage from the attack being dealt, the attack is considered to have no character defending and the identity of that ally’s controller becomes the target of the attack.
   - If **no character was declared the defender** of the attack, the attack is considered undefended. All damage from the attack is dealt to the character targeted by the attack*.*
     - The targeted character is considered to have been attacked.
6. The attack finishes resolving and the following types of abilities trigger in order:
   a. Forced abilities *(such as the retaliate keyword)* with the following triggers (in any order):
      - “after [character] attacks [and damages/defeats] [you/an ally]...”
      - “after [character] is attacked...”
      - “after [character] defends [and takes no damage]...”
      - “after [character] [takes/deals] damage...”
   b. Non-forced abilities with the triggers listed above.

These rules also apply to enemy attacks:

- Interrupts that trigger “when [enemy name] attacks” have the same timing as interrupts that trigger “when [the villain/an enemy] initiates an attack.”
- If an enemy attack ends before damage is dealt, abilities that trigger after an attack or after a character defends an attack resolve as normal.
- Abilities that trigger when an enemy “attacks and damages” or “attacks and defeats” a character trigger only when that character is damaged/defeated by damage from the attack itself, and not by an ability that resolves during the attack.
  - *For example, Sonic Converter reads: “**Forced Response**: After Klaw attacks and damages a character, stun that character.” If Klaw is given a boost ability that says “**Boost**: Deal 1 damage to each hero,” that boost ability does not trigger the ability on the Sonic Converter.*

**See also**: Activation, Ally, Attacks Against Allies, Boost, Damage, Defend, Enemy, Identity, Minion, Modifiers, Retaliate X, Target, Villain, Villainous

### Scheme (Enemy Activation)
<!-- RR v1.8 p.39 -->

A scheme is a type of enemy activation. If an enemy is instructed to scheme, follow these steps:

1. If a villain, or a minion with the villainous keyword, is scheming, give it one facedown boost card from the encounter deck. *(If a minion without the villainous keyword is scheming, skip this step.)*
2. Resolve each of the scheming enemy’s boost cards, one at a time and in the order in which they were dealt, by doing the following:
   a. Flip the boost card faceup.
   b. Resolve any “**Boost**” abilities, indicated by the star icon in the boost area. *(All other abilities on the boost card are ignored.)*
   c. Increase the scheming enemy’s SCH value by one for each boost icon on the card.
   d. Discard the boost card.
   e. If the enemy has any boost cards remaining, repeat these steps with the next boost card.
3. Place threat on the main scheme equal to the scheming enemy’s modified SCH value.

**See also**: Activation, Boost, Minion, Modifiers, Villain, Villain Phase, Villainous

### Boost, Boost Icon ([boost])
<!-- RR v1.8 p.11 -->

Each time the villain attacks or schemes, the villain is given one facedown card from the encounter deck, as a boost card. Minions with the villainous keyword are also given a boost card when they activate.

During the activation *(and after any defenders are declared if the villain is attacking)*, each boost card on the enemy is turned face up one at a time. Add the number of boost icons on each card to the enemy’s ATK value *(if it is attacking)* or SCH value *(if it is scheming)* for that activation. Boost icons are located at the bottom-right of the card.

If the boost field has a star icon, it indicates that the card has a “**Boost**” ability. Refer to the card’s text box and resolve the “**Boost**” ability when the card is turned face up. The “**Boost**” ability is located beneath the divider line in the text box.

- A star icon is not itself considered a boost icon, and does not contribute to the villain’s ATK or SCH value.
- Only the ability text beneath the divider line is active on a card that is resolving as a boost card.
- Damage dealt by a boost ability is not considered to be damage dealt by the activation during which the boost ability resolved.
- If additional boost cards are resolved for an activation, the boost icons are cumulative, and all “**Boost**” abilities on those cards resolve.
- After applying a boost card to an activation, discard it.
- If an enemy is dealt a boost card outside of its own activation, that boost card remains facedown on that enemy until that enemy activates.
  - If that enemy is a villain or a minion with the villainous keyword, it still gets dealt another boost card at the start of its activation as normal.

**See also**: Attack (Enemy Activation), Scheme (Enemy Activation), Star Icon, Villainous

### Amplify Icon ([amplify])
<!-- RR v1.8 p.7 -->

The amplify icon represents various forces that are empowering or bolstering the villain.

When a boost card is turned faceup **during an enemy activation**, add one additional boost icon to that card for each amplify icon in play.

- Each amplify icon is equivalent to the following constant ability: “Each boost card gains [boost].”

**See also**: Activation, Boost, Enemy, Icons

### Villainous
<!-- RR v1.8 p.47 -->

When a character with the villainous keyword uses a basic power, give it a facedown boost card from the top of the encounter deck. When resolving that basic power, turn the boost card faceup, resolve any boost ability on that card, and apply its boost icons to the character’s stats for this use. Discard the boost card after it is resolved.

- The villainous keyword is equivalent to the following triggered ability: “**Forced Interrupt**: When this character uses a basic power, give it a boost card.”
- If a boost ability on a boost card dealt to a character refers to “the villain,” that ability still applies to the villain *(even if another type of character is resolving it)*.

**See also**: Activation, Attack (Enemy Activation), Boost, Discard, Keywords, Minion, Reminder Text, Scheme (Enemy Activation)

### Defend, Defense
<!-- RR v1.8 p.15 -->

During an enemy attack, a player may defend against that attack using cards they control.

- Only one player at a time can defend against an enemy attack. While a player is defending, other players cannot defend against that same attack.
- A hero can use their basic defense power to defend against an enemy attack. A hero must exhaust to use this power. The amount of damage dealt by the attack is reduced by the hero’s DEF value, and any remaining damage is dealt to that hero. While a hero is defending against an attack, other friendly characters cannot defend against that attack.
  - When a card ability says to “declare [a hero] the defender” of an attack, that hero is considered to be making a basic defense.
  - A card ability that allows a hero to be declared as a defender without exhausting can be used on an exhausted hero.
- An ally can exhaust to defend against an enemy attack. Damage from the attack is dealt to that ally. While an ally is defending against an attack, other friendly characters cannot defend against that attack.
  - When an ally defends an attack, that ally becomes the target character for that attack, and its controller becomes the target player for that attack.
  - When a card ability says to “declare [an ally] the defender” of an attack, that ally becomes the defender of the attack.
  - A card ability that allows an ally to be declared as a defender without exhausting can be used on an exhausted ally.
- When a player initiates a triggered ability labeled as a defense—such as “**Hero Interrupt** *(defense)*”— during an enemy attack, that player’s identity becomes the defender and is considered to have defended the attack if there is not already a defender.
  - The player’s identity is considered to be the defender as soon as the defense-labeled ability begins resolving.
  - Abilities that trigger “when your hero defends against an attack” can be triggered when resolving a defense-labeled ability.
  - Resolving a defense-labeled ability is not a basic defense and does not cause a hero to reduce the amount of damage dealt by that hero’s DEF. That hero can still be declared the defender of the attack during the “Declare Defender” step or by another card ability.
  - Unless specified by the ability’s text, a hero does not exhaust when using a defense-labeled ability.
  - The defending player may resolve any number of defense abilities during an enemy attack *(as long as the triggering conditions of those abilities are met)*.
  - Once a player resolves a defense-labeled ability during an enemy attack, other players cannot resolve defense-labeled abilities for that same attack.
  - Defense-labeled abilities can be played during an attack by a player whose ally is defending that attack. In that case, the player’s identity does **not** become the defender.
  - A player can trigger abilities labeled as a defense outside of an attack if the ability’s triggering condition is met. When triggered this way, the player’s identity is not considered to have defended an attack.
- If a player defends against an enemy attack that targets a different player *(either by defending with a character they control or by resolving a defense ability)*, the defending player becomes the new target of that attack.
  - Any triggered ability that refers to “you” refers to the player who was the target of the attack when that ability resolved. *(For example, the “you” in an ability that triggers “when [enemy] attacks you” refers to the player against whom the attack initiated, while the “you” in an ability that triggers “after [enemy] attacks you” refers to the player whose character defended the attack.)*
  - Any constant or boost abilities that refer to “you” refer to the defending player.
- If no character is used to defend against an enemy attack, that attack is considered undefended. Additionally, if a defending ally is defeated before damage from the attack is dealt *(such as through a “Boost” ability)*, the attack is considered undefended.
- Abilities that trigger after a character defends an attack resolve after that attack ends.
  - If an effect causes a defended attack to end before fully resolving, the attack is still considered to have been defended.
  - If an ability triggers after a character uses a basic power, that ability triggers after an attack in which a character made a basic defense resolves.

**See also**: Ability, Ally, Attack (Enemy Activation), Damage, “Friendly”, Identity, Labeled Ability, Player

### Attacks Against Allies
<!-- RR v1.8 p.10 -->

Some effects cause a villain or minion to attack an ally directly. When this occurs, any undefended damage from that attack is placed on the ally that was attacked.

- The player who controls the ally is considered the attacked player.
  - Abilities that resolve while/when/after the attacking enemy “attacks you” resolve against the attacked player.
  - Any boost abilities that refer to “you” refer to the player who controls the attacked ally.
- Players may defend these attacks as normal by declaring a hero or an ally as the defender.
- If the attack has overkill and defeats an ally (whether that ally was the attacked ally or a defending ally), any excess damage from that attack is dealt to the identity of the player who controlled the defeated ally.

**See also**: Ally, Attack (Enemy Activation), Defend, Target

### Deal, Deal an Encounter Card
<!-- RR v1.8 p.15 -->

During step three of the villain phase, each player is dealt one facedown encounter card.

If a card ability instructs a player to be dealt an encounter card, the player takes the top card of the encounter deck and places it facedown in front of them. This card is not revealed at this time. This card is added to the queue of cards that player resolves during the villain phase.

- If a player is dealt an encounter card during step three or four of the villain phase, the extra encounter card is added to the queue of cards that are being dealt and revealed in those same steps.

**See also**: Ability, Encounter Card, Encounter Deck, Player, Villain Phase

### Hazard Icon ([hazard])
<!-- RR v1.8 p.21 -->

During the Deal Encounter Cards step of the villain phase, for each hazard icon on cards in play, deal one player one additional card *(not one card per player)*. Additional cards are dealt in player order *(first additional card to the first player, the second to the second player, etc.)*.

- Each hazard icon is equivalent to the following constant ability: “During the Deal Encounter Cards step of the villain phase, deal 1 additional encounter card. Additional cards from hazard icons are dealt in player order.”

**See also**: Deal, In Player Order, Villain Phase

### Reveal
<!-- RR v1.8 p.38 -->

During step four of the villain phase, each player *(in player order)* reveals and resolves all facedown encounter cards that have been dealt to them, one card at a time in the order in which they were dealt.

To reveal an encounter card, follow these steps:

1. Turn the encounter card faceup.
2. If the encounter card’s type is:
   - **Attachment**: It enters play attached to the game element specified by its “attach to” text. If it does not have “attach to” text, place it on the table in front of the player revealing it. *(It is not in play.)*
   - **Environment**: It enters play in the villain’s play area.
   - **Minion**: It enters play in the play area of the player revealing it. It is considered to engage that player.
   - **Obligation**: It enters play in the play area of the player revealing it.
     - If the card specifies a player to give it to, that player is considered to be revealing it.
   - **Side scheme**: It enters play in the villain’s play area.
   - **Treachery**: Place it on the table in front of the player revealing it. *(It is not in play.)*
   - **Other**: Place it on the table in front of the player revealing it. *(It is not in play.)*
3. Resolve each “**When Revealed**” ability on that card *(including those provided by keywords)*.
4. If the card is a treachery, discard it.

If a player is instructed by card text to reveal an encounter card from the encounter deck or any other game area, this same resolution procedure applies.

- Responses (both forced and non-forced) to any step of the revealing of an encounter card are not resolved until after all steps of the reveal process have been completed.

**See also**: Attachment, Choose (Game Element), Choose (Option), Deal, Encounter Card, Enters Play, Environment, In Player Order, Minion, Obligation, Player, Side Scheme, Treachery, Villain Phase

### Surge
<!-- RR v1.8 p.42 -->

When an encounter card with this keyword is revealed, the player resolving the card deals themself a facedown encounter card from the top of the encounter deck.

- The surge keyword is equivalent to the following triggered ability: “**When Revealed**: Deal yourself 1 facedown encounter card.”
- Complete the process of resolving the original card, as well as any response abilities that are triggered by that card being revealed, before revealing the additional card.

**See also**: Encounter Card, Encounter Deck, Keywords, Reminder Text, Reveal

### Engage
<!-- RR v1.8 p.18 -->

When a minion enters play in a player’s play area, it engages that player.

Unless otherwise specified by the minion or by the effect that put the minion into play, the minion engages the player who is resolving the current encounter card.

- An engaged minion remains engaged with the same player until it is defeated, removed from play, or a card ability causes it to engage another player.
- If a card ability instructs a player to engage a minion, that minion is also considered to have engaged that player.
- While a minion is engaged with a player, card abilities cannot cause the minion to engage with the same player again *(as the two are already engaged)*.

**See also**: Ability, Defeat, Minion, Player, Player’s Play Area

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

## Damage, Defeat, Decks and Winning

### Damage
<!-- RR v1.8 p.14 -->

Damage reduces a character’s hit points. If a character has zero or fewer remaining hit points, it is defeated.

- Damage on an identity or villain is tracked by a hit point dial. If such a character takes damaged, reduce its dial by the amount of damage that it took.
- Damage on an ally or minion is tracked by damage tokens. If such a character takes damage, place the specified value of damage tokens on the character.
- When damage is dealt to a character, that character takes damage.
  - When the amount of damage an effect deals is modified, the amount of damage the character takes is similarly modified.
  - When the amount of damage a character takes is modified *(such as by damage being prevented)*, the amount of damage dealt is **not** modified.
  - The order of resolution for effects surrounding the dealing and taking of damage are as follows:
    1. Abilities that trigger “when [character] would deal/be dealt any amount of damage...”
    2. Tough status cards.
    3. Abilities that trigger “when [character] would take any amount of damage...”
    4. Abilities that trigger “when [character] takes any amount of damage...”
    5. Placing of damage on the character.
    6. Abilities that trigger “when [character] would be defeated...”
    7. Abilities that trigger “when [character] is defeated...” *(including “**When Defeated**” abilities)*.
    8. Discarding of a defeated character.
    9. Abilities that trigger “after [character] deals/is dealt/takes any amount of damage...” or “after [character] defeats/is defeated...”

**See also**: Component Limitations, Defeat, Hit Points, Indirect Damage, Move, Prevent, Tough

### Tough
<!-- RR v1.8 p.44 -->

Tough is a status that prevents a character from taking damage.

- A tough status card resolves like the triggered ability: “**Forced Interrupt**: When this character would take damage, remove a tough status card from it instead.”
- If a character with a tough status card would take any amount of damage, prevent all of that damage and discard a tough status card from that character instead.
  - A character with multiple tough status cards discards only one tough status card each time it would take damage.
  - When a hero with a tough status card defends an attack, they reduce the damage from the attack by their DEF first. If the damage is reduced to 0, the hero does not lose their tough status card.
- As a tough status card prevents damage fully, the character who had the tough status card is not considered to have taken damage.

**See also**: Damage, Overkill, Status Cards, Toughness

### Overkill
<!-- RR v1.8 p.31 -->

If an ally is defeated by an attack with the overkill keyword, deal any damage on that ally beyond its hit points to the identity of the player who controls the ally.

If a minion is defeated by an attack with the overkill keyword, deal any damage on that minion beyond its hit points to the villain.

- The overkill keyword is equivalent to the following ability: “If this attack defeats an ally, deal the damage beyond its hit points to the identity of that ally’s controller. If this attack defeats a minion, deal the damage beyond its hit points to the villain.”
- Damage dealt by overkill to an identity or villain is considered damage from an attack, but does not constitute an attack against that character.
- If a card ability counts excess damage dealt, that ability counts the same value of excess damage that is calculated when resolving the overkill keyword.
- If excess damage from an attack with overkill is prevented, that damage is **not** dealt to the identity or villain.

**See also**: Ally, Attack (Enemy Activation), Attack (Player Ability Type), Damage, Hit Points, Identity, Keywords, Minion, Reminder Text, Villain

### Indirect Damage
<!-- RR v1.8 p.24 -->

Some card abilities may deal “indirect damage.”

- Indirect damage dealt to a player can be divided as that player chooses among characters under their control.
- Indirect damage dealt to a group of players *(or among players)* can be divided as the group chooses among friendly characters in play.
- All indirect damage from a single source is first assigned and then resolved simultaneously.
  - While assigning indirect damage, a character cannot be assigned more indirect damage than would cause it to be defeated. This is assessed without accounting for interactions with other abilities.
  - A character with a tough status card can be assigned indirect damage up to its remaining hit points, and all damage assigned to it is prevented by its tough status card.
- Characters that cannot take damage cannot be assigned indirect damage.
  - If indirect damage dealt to a player cannot be assigned to any character that player controls, that damage is ignored.
- If an enemy’s attack deals indirect damage, the indirect damage is dealt during step four of the enemy activation *(after player’s have the opportunity to defend against the attack)*.
  - Only the defending character, or the attacked player’s identity if the attack was undefended, is considered to have been attacked, even if other characters were assigned some or all of the indirect damage.
- *For example, if you take 5 indirect damage, but you control an ally with 4 hit points remaining, you may assign 4 of that indirect damage to the ally, then assign the remaining 1 indirect damage to your identity.*

**See also**: Ally, Attack (Enemy Activation), Damage, Defeat, Player

### Defeat
<!-- RR v1.8 p.15 -->

If a character has zero or fewer remaining hit points, or if a side scheme has no threat on it, it is defeated.

- If an ally, minion, or side scheme is defeated, it is discarded.
- If an identity or stage of the villain is defeated, it is removed from the game.

**See also**: Ally, Hit Points, Minion, Player, Player Elimination, Removed from the Game, Side Scheme, Villain, Villain Defeat

### Villain Defeat
<!-- RR v1.8 p.47 -->

If the villain’s hit point dial is reduced to zero, the players have defeated that stage of the villain.

Remove the current stage of the villain deck from the game. The next sequential stage of the villain deck is revealed. Set the villain’s hit point dial as indicated by that stage.

**If the final stage of the villain deck is defeated, the players win the game.**

- The revealing of a villain cannot be canceled.
- Excess damage that is dealt to defeat a villain stage does not carry over to the new stage.
- If the new stage of the villain has the same title as the previous stage:
  - The new stage of the villain is considered the same character as the defeated stage for purposes of card abilities *(such as the retaliate X keyword)*.
  - Attachments, upgrades, status cards, counters, and non-damage tokens on a villain carry over to the new stage.
  - If the villain was defeated while activating, the villain resumes that activation using the new villain stage.
- If the new stage of the villain has a different title from the previous stage:
  - The new stage of the villain **not** is considered the same character as the defeated stage for purposes of card abilities.
  - Attachments, upgrades, status cards, counters, and non-damage tokens do **not** carry over to the new stage.
  - If the villain was defeated while activating, the activation ends without resolving.

**See also**: All-Purpose Counter, Attachment, Damage, Defeat, Excess Damage, Hit Points, Status Cards, Villain

### Player Elimination
<!-- RR v1.8 p.34 -->

A player is eliminated from the game if their identity is defeated. This usually occurs when the character’s remaining hit points are reduced to zero.

When a player is eliminated, perform the following in order:

1. If the eliminated player has the first player token, they pass it to the next clockwise player.
2. If there are minions engaged with the eliminated player, each of those minions engages the next clockwise player, retaining any tokens, attached cards, boost cards, tucked cards, and status cards on them.
3. For each card in the eliminated player’s play area that are not owned by that player, do the following:
   - If that card is an attachment with the permanent keyword, resolve its “attach to” text. If it has none or that text does not have a valid target, remove the attachment from the game.
   - Remove each non-attachment card with the permanent keyword from the game.
   - Place each other card in its owner’s discard pile.
4. Place each card owned by the eliminated player in the eliminated player’s discard pile.
5. Remove the eliminated player’s play area and each other game element within it *(hand, deck, discard pile, cards in play, hit point dial, etc.)* from the game.

When a player is eliminated, the remaining players continue to play the game. Eliminated players no longer participate in the game but are considered to win or lose along with the rest of the group, depending on how they finish.

- If all players are eliminated, the game ends and the players lose.
- If a player is eliminated partway through the resolution of an ability, resolve the entire ability.
  - If a player is eliminated in the middle of an attack, that attack ends.
- Effects that refer to the players in the game ignore eliminated players, except for the per player icon ([per player]).

**See also**: Ability, Deal, Engage, Game Element, Hit Points, Identity, Minion, Per Player Icon, Player, Player Card, Player Deck, Player’s Play Area, Winning the Game

### Encounter Deck
<!-- RR v1.8 p.17 -->

The encounter deck contains the encounter cards *(attachments, environments, minions, side schemes, and treacheries)* that players can face during a scenario.

- The order of cards within the encounter deck cannot be changed unless a player is instructed to do so by a game step, game function, or card ability.

If the encounter deck is empty, the encounter discard pile is immediately shuffled to create a new encounter deck. When this occurs, place an acceleration token next to the main scheme deck.

- If a card ability discards a specified number of cards from the encounter deck or until a card with specific criteria is discarded, discard cards from the encounter deck until the discard condition is met or the encounter deck is empty. If the encounter deck is emptied this way, that card ability is considered to be fulfilled. Do not continue the discard effect with the newly shuffled encounter deck.
- If the encounter deck empties during the resolution of any other type of game effect *(for example, the dealing of encounter cards)*, that effect finishes resolving after the encounter deck has been reset.
- If there are no cards in both the encounter deck and the encounter discard pile simultaneously *(such as all cards from the encounter deck being in play)*, an infinite loop occurs with an infinite number of acceleration tokens being placed next to the main scheme deck. If this happens, the players lose.

**See also**: Acceleration Token, Attachment, Choose (Game Element), Discard, Discard Pile, Environment, Minion, Side Scheme, Treachery

### Player Deck
<!-- RR v1.8 p.33 -->

A player’s deck contains the player cards *(allies, events, player side schemes, resources, supports, and upgrades)* owned by the player who started the game with that deck.

- The order of cards within a player’s deck cannot be changed unless a player is instructed to do so by a game step, game function, or card ability.

If a player deck empties, the player shuffles their discard pile to make a new deck. That player immediately deals themself one facedown encounter card from the top of the encounter deck.

- If the player’s deck empties and reshuffles while the player was drawing cards, the player continues to draw cards up to the specified number.
- If the player’s deck empties while the player was discarding cards from their deck, no further cards are discarded from the newly shuffled deck.
- If a player deck empties and the player has no cards in their discard pile, the deck does not reset until there is at least one card in the player’s discard pile, then the player deals themself one facedown encounter card.

**See also**: Discard, Discard Pile, Draw, Encounter Deck, Shuffle

### Winning the Game
<!-- RR v1.8 p.48 -->

If the final villain stage is defeated, the players win the game. If the final stage of the main scheme deck is completed, the villain wins the game.

- Some scenarios have alternate win or loss conditions. If these conditions are met, the players win or lose accordingly.
  - Alternate win or loss conditions cannot be blanked or canceled.
