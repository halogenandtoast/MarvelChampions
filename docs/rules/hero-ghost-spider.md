# Ghost-Spider (Sinister Motives)

Sources: [MC27 rulebook](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc27_sinister_motives_rules_v5-compressed.pdf),
pp.3, 18–19; local MarvelCDB card data, codes 27001–27029; current Rules Reference v1.8.
This is an implementation summary. Card text is available in `frontend/public/data/cards.json`.

Ghost-Spider has 10 hit points, THW 1, ATK 2, DEF 3 and hand size 5.
Gwen Stacy has REC 3 and hand size 6. Her starter deck uses Protection.

## Rules and timing

- Dizzying Reflexes and Web-Bracelet respond after an interrupt or response event's
  ability has finished resolving. Actions and basic powers do not satisfy this trigger.
  The hero's limit is once per phase; Web-Bracelet's maximum is shared by copies for
  each event occurrence.
- Ghost Kick and Phantom Flip respond to a completed basic ATK, THW or DEF use.
  Each card title has its own maximum of one per triggering basic power use.
- George Stacy's attached events are outside the hand. They may be played as if in
  hand, but cannot be spent as resource cards from hand. Blanking the support removes
  this play permission. Its attached cards are discarded when it leaves play.
- George Stacy holds at most three events, including attachments made by Parental
  Guidance. See the official answer reproduced in
  [FFG rulings](https://hallofheroeslcg.com/official-ffg-rulings/) under Parental Guidance.
- Requirement resources must actually be spent, even if a cost reduction reduces the
  ordinary resource cost below their number. A Requirement card cannot be played
  ignoring its resource cost. Wild resources count only as their declared type.
- Web Binding cancels an activation rather than defending. A stunned or confused
  activation replaced by a status card does not open its activation interrupt.
- Ticket to the Multiverse is removed as a cost. Its explicit shuffle of the discard
  pile into the deck is not a player deck running out of cards.
- Worried Father uses the current erratum: George Stacy is attached facedown to the
  obligation. It searches the deck, hand, discard pile and play area.

## Current Rules Reference changes

The v1.8 entries for Incite and Surge define them as equivalent to When Revealed
abilities. Cancellation follows those current definitions; older explanations that
exclude keywords from When Revealed cancellation predate these changes. See
`glossary-i-o.md` (Incite), `glossary-s-z.md` (Surge), and `faq-errata.md` (Worried Father).

Across the Spider-Verse repeats the entire ability for the chosen player, including
exhausting a Web-Warrior card. Its repeat can be used again if its costs can be paid;
there is no additional repetition cap (RR FAQ).

## Starter deck

The fifteen signature cards are Ghost Kick ×3, Parental Guidance, Phantom Flip ×3,
Pirouette and Punch ×2, Web Binding ×2, George Stacy, Ticket to the Multiverse and
Web-Bracelet ×2.

The other twenty-five are Silk, Spider-Man (Miles Morales), Spider-UK, Bait and Switch ×3,
Jump Flip ×3, Return the Favor ×3, What Doesn't Kill Me ×3, Spider-Man (Hobie Brown),
Across the Spider-Verse, Young Love, Energy, Genius, Strength, Web of Life and Destiny,
and Plan B ×3. The obligation and nemesis set are not part of the forty-card deck.
