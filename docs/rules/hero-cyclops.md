# Cyclops Hero Pack (MC33)

Sources: [official MC33 rules insert](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc33_cyclops_rules_insert.pdf#page=1)
(p.2 is blank); Rules Reference v1.8; local MarvelCDB data, codes 33001–33035;
the [printed starter-deck card](https://hallofheroeslcg.com/wp-content/uploads/2022/09/c1-1.jpg).
The following is an implementation summary of the insert's rules and FAQ.
Current Rules Reference definitions and published errata take precedence over
older keyword summaries. Card text is in `frontend/public/data/cards.json`.

## Rules introduced in the insert

<!-- MC33 rules insert p.1; implementation summary -->

**Amplify icon.** Each amplify icon in play adds one boost icon when a boost
card is turned faceup during an enemy activation. This modifies the boost
card, rather than permanently modifying the activating enemy's printed stats.

**Piercing.** Resolve the removal of the attacked character's tough status
cards before applying piercing attack damage.

**Ranged.** Retaliate does not apply against a ranged attack.

**Stalwart.** Stun and confuse cannot affect a stalwart character.

**Team-Up.** The deck's identity must match one of the two named characters.
Playing the card also requires both named characters to be in play; an ally
can satisfy either named character.

**Temporary.** Discard a temporary card when the round ends.

**Villainous.** A villainous minion receives a facedown encounter boost card
when it activates. Turn that card faceup, resolve its boost ability, apply its
boost icons for that activation, and discard it afterward.

## Current keyword definitions

<!-- RR v1.8 pp.7,32,36,40,43,44,47; implementation summary -->

Use the canonical glossary entries for [amplify](glossary-a-c.md),
[piercing and ranged](glossary-p-r.md), and
[stalwart, team-up, temporary and villainous](glossary-s-z.md). In particular:

- Piercing removes no tough cards if the attack would deal no damage. It removes
  every existing tough card before positive damage, including multiple tough
  status cards held by Colossus.
- Gaining stalwart immediately removes existing stunned and confused status
  cards as well as preventing new ones.
- Team-Up requires friendly characters; an ally can match by its title or
  subtitle. Deck inclusion checks the identity's hero or alter-ego title.
- Temporary represents a forced interrupt to the round ending. Check the
  card's current keywords, including keyword-removal modifiers.
- Current villainous applies when a character uses a basic power, including a
  minion activation. A boost effect referring to the villain still refers to
  the villain when the boost card belongs to a different character's activation.

## Field Commander FAQ

<!-- MC33 rules insert p.1; implementation summary -->

Field Commander changes the order of player turns without changing who is
first player. Cyclops takes one turn before the first player's turn. The
remaining players then take their turns in normal order from the first player,
skipping Cyclops because he has already taken his turn. If Cyclops is already
first player, he still receives only one turn.

Its keyword-removal ability applies to Cyclops-set upgrades attached to minions.
It does not remove temporary from an upgrade attached to a villain. Resolve
that continuous modifier from the current game state at the round-end window.

## Ricochet Beam FAQ

<!-- RR v1.8 Appendix IV, Cyclops Hero Pack, p.64; implementation summary -->

The [official FAQ](faq-errata.md) allows both damage instances to target the
same enemy. If Exploit Weakness is attached to that enemy, it increases each
instance separately: Ricochet Beam deals 4 damage and then 4 damage, for 8 total.
The second target must still satisfy its upgrade requirement when chosen after
the first damage instance resolves.

## Printed-card checks

The printed identity has THW 2, ATK 1, DEF 2, 10 hit points and hero hand size 5.
Scott Summers has REC 3 and alter-ego hand size 6. These match MarvelCDB.

The [printed Blindfold card](https://hallofheroeslcg.com/wp-content/uploads/2022/10/cc4-1.jpg)
has ATK 0 with **zero attack consequential damage**; MarvelCDB's local record
incorrectly supplies `attackCost: 1`. Use zero, as printed. Her THW is 2 with
one consequential damage and she has 2 hit points.

Printed ally checks also confirm Phoenix and Beast each have THW 2 and ATK 2,
with one consequential damage for either power; Dust has THW 1 and ATK 1,
with one printed consequential damage for either power; Rockslide has THW 1
with one consequential damage and ATK 3 with two; Angel has THW 1 and ATK 2,
with one consequential damage for either power. Dust's optional interrupt adds
one further consequential damage after its multi-target attack.

## Starter deck

<!-- MC33 printed starter-deck card; verified against MarvelCDB quantities -->

The printed list contains forty shuffled cards, codes 33002–33026 at the
quantities below. Scott Summers's deck-building permission admits its three
off-aspect X-Men allies while the deck's chosen aspect remains Leadership.

Signature cards (15): Phoenix; Ruby Quartz Visor; Field Commander; Exploit
Weakness ×2; Practiced Defense ×2; Priority Target ×2; Full Blast; Ricochet
Beam ×2; Tactical Brilliance ×3.

Leadership and off-aspect allies (15): Beast; Dust (Aggression); Rockslide
(Protection); Blindfold (Justice); Danger Room Training ×3; Coordinated Attack
×3; Teamwork ×3; Effective Leadership ×2.

Basic cards (10): Angel; Utopia; Danger Room; Game Time ×3; Psychic Rapport;
Energy; Genius; Strength.

Lost Visor is the obligation. The nemesis set contains Mister Sinister,
Genetic Manipulation, Gene Therapy ×2 and Concussive Force. Identity cards,
the obligation and nemesis cards are outside the forty-card player deck.

Marked, Befuddle, Pinned Down and Honorary X-Men are additional deck-building
cards and are not included in the printed starter deck.

## Rules and timing references

Optic Blast is an attack ability, not a basic attack, and does not have an
exhaustion cost of its own. Full Blast separately requires exhausting Cyclops.
Lost Visor's attack prohibition covers both basic attacks and attack abilities.

For searches and returned cards, use [Search and Set Icon](glossary-s-z.md):
searching a deck requires shuffling it afterward, and a Cyclops card means a
card of the Cyclops hero set rather than any card supplied in this hero pack.
The various searches retain their printed zones: Constant Training searches
only the deck; Beast and Danger Room also search the discard pile; Phoenix
and Tactical Brilliance choose from the discard pile.

For Danger Room's direct attachment, apply the current rules for putting a
card into play and resolving its attachment requirements in
[Play, Put into Play](glossary-p-r.md) and [Upgrade](glossary-s-z.md).
The selected ally must satisfy the Training upgrade's attachment restrictions.

No unpublished project-owner rulings are recorded here. Published card text,
the insert's Field Commander FAQ and the current Rules Reference determine
the rules above.

## Rulings (from the project owner, 2026-10-30)

- Befuddle substitutes the character's THW for ATK only. An ally still takes its attack consequential damage, using its attack consequential icons.
- The concurrent Phoenix implementation owns Psychic Rapport (33023, reprinted as 34023). Cyclops uses and tests that shared implementation. Builds are coordinated through a single owner.
