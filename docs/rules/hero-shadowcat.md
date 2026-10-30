# Shadowcat — Mutant Genesis

Source: [official MC32 rulebook](https://images-cdn.fantasyflightgames.com/filer_public/2c/52/2c52bcb0-4e21-4bf8-8052-b2ef41266737/mc32_mutant_genesis_rulebook_v5-compressed.pdf), pp.3 and 22. Additional-form and permanent rules use Rules Reference v1.8 where it supersedes the rulebook.

## Additional forms

<!-- MC32 rulebook p.3; implementation summary -->

Mass forms supplement hero and alter-ego forms. Changing mass form counts as changing form for effects such as Ready to Rumble, but does not spend the once-per-turn hero/alter-ego flip. Shadowcat begins with her permanent mass form upgrade in play, Solid side faceup. Exhaustion persists when it flips.

Solid has an optional response after attacking or defending. Phased has a forced response after attacking or defending. While defending in Phased form, Shadowcat cannot take damage; this includes damage beyond the ordinary attack damage while she is defending.

The Powerful Punch FAQ in `faq-errata.md` resolves nested attack/defense timing: finish Powerful Punch's attack-labeled effect, resolve the mass form reaction, resume the enemy attack with Shadowcat defending, then resolve the mass form reaction after the defense. A thwart does not itself trigger either mass form response.

The Mutant Protectors FAQ makes its player the enemy attack's target and the ally put into play its defender.

Permanently Phased changes the mass form to Phased and prohibits attacks, defenses and mass form changes. It does not prohibit changing to alter-ego, which is necessary to remove the obligation.

## Rulings (from the project owner, 2026-10-30)

An after-ignore window occurs when Shadowcat actually bypasses Guard to attack the villain, or Patrol/Crisis to thwart the main scheme. All cards imposing the bypassed restriction are eligible for the corresponding upgrade response. Merely checking possible targets does not trigger these windows.

## Starter deck

<!-- MC32 rulebook p.22; implementation summary -->

Signature cards: Solid / Phased (set aside for setup); Lockheed; Kitty's Room; Acute Control; Intangible Interference; Phased and Confused (2); Shadowcat Surprise (3); Phased Strike (2); Airwalk (2); Quick Shift (2).

Aggression: Wolverine; Magik; Attack Training (3); Gatekeeper (3); Team Strike (3); Toe to Toe (3); Aggressive Energy (2).

Basic: Colossus; X-Mansion; Shadow and Steel; Ready to Rumble (3); Energy; Genius; Strength.

Obligation: Permanently Phased. Nemesis: White Queen; The Hellfire Club; Hellfire Pawn (2); Telepathic Restraint.

## Continuous status effects

White Queen and Telepathic Restraint continuously apply actual confused/stunned status cards while their conditions hold. Removing their source does not discard a status card that has already been placed. The engine reapplies these statuses when necessary; it must not model them solely as temporary Boolean conditions.
