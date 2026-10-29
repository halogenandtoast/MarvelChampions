# Valkyrie Hero Pack

Source: official [MC25 Valkyrie rulesheet](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc25_valkyrie_rulesheet.pdf), 1 rules page (and blank page). Rules Reference v1.8 takes precedence over obsolete keyword wording. Card data: local MarvelCDB dump.

## Rulesheet

### Alliance
<!-- MC25 rulesheet p.1 -->

Alliance is a new keyword that appears on some player cards in this pack. When a player declares their intention to play an alliance card, any player may help pay the costs for that card.

### Hinder X
<!-- MC25 rulesheet p.1 -->

When a card with the hinder X keyword is revealed, place X threat on that card.

Current wording and timing: [RR Alliance](glossary-a-c.md#alliance), [RR Hinder X](glossary-d-h.md#hinder-x). Only the player playing the alliance card resolves it; any player may contribute to its costs.

## Current errata

See [RR Valkyrie errata](faq-errata.md#valkyrie-hero-pack).

- Aragorn gives **you** +4 hit points and the Aerial trait, so this applies in alter-ego form as well.
- Shieldmaiden has the Defense trait and its Hero Interrupt is a defense ability. Its declaration of Valkyrie as defender does not exhaust her.
- Beguiled has the Condition trait. Its target is the highest-cost ally without Beguiled attached, and that ally engages its controller. Otherwise, Beguiled gains surge.

## Implementation reference (non-normative)

Death-Glow is set aside out of play by Brunnhilde's setup ability. Valkyrie plays it paying its normal cost through Death Perception. When its attached enemy is defeated, its Forced Interrupt sets it aside again; it readies Valkyrie only if Valkyrie defeated that enemy. Valhalla and Flight of the Valkyrior refer to the enemy that had Death-Glow attached when defeated, even though its Forced Interrupt has already set Death-Glow aside.

Dragonfang and Valkyrie's Spear grant their ordinary bonuses outside the specified marked-enemy attack/defense, and grant +2 **instead** during that context. Trouble in Otherworld prevents Valkyrie's attacks against the marked enemy; allies are distinct characters. Powerful Enchantments protects attachments on friendly characters; a Beguiled ally is treated as a minion and is not a friendly character.

The Enchantress, Beguiled and Seduced nemesis cards duplicate the Enchantress modular cards from The Mad Titan's Shadow; see [the existing rulings](scenario-loki.md).

## Starter deck

The rulesheet refers to the divider card's reverse for its complete deck list. Local pack quantities before the divider yield 41 physical player cards: 16 signature cards (including Death-Glow), 18 Aggression cards, and 7 basic cards. Death-Glow is set aside during normal identity Setup, after the opening hand and mulligan. It is found in its current zone without a replacement draw.

## Rulings (from the project owner, 2026-10-29)

- Thor initiates one attack, resolves it against the chosen player's minions in the controller's chosen order, and takes consequential damage once.
- Death-Glow follows normal identity Setup timing, after the opening hand and mulligan. The physical starter includes 41 cards; Setup finds Death-Glow in its current zone and sets it aside. The earlier proposal to set it aside before drawing was retracted.

- Cosmic Alliance: both chosen characters must be exhausted (project owner, 2026-10-29).

## Effect feasibility

Quick Strike requires positive ATK against at least one legal enemy unless stun replaces its attack. Smash the Problem requires positive ATK unless confusion replaces its thwart. Problem Solvers requires its chosen pair to provide positive combined THW unless confusion replaces its thwart. Paying costs alone does not count as changing the game state (RR “Cost”, “Target”).
