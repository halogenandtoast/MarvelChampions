# Spider-Man (Miles Morales, Sinister Motives)

Sources: [MC27 rulebook](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc27_sinister_motives_rules_v5-compressed.pdf),
pp.3, 20–21; local MarvelCDB card data, codes 27030–27060; current Rules Reference v1.8.
This is an implementation summary. Card text is available in `frontend/public/data/cards.json`.

Spider-Man has 9 hit points, THW 2, ATK 2, DEF 2 and hand size 5.
Miles Morales has REC 4 and hand size 6. His starter deck uses Justice.

## Rules and timing

- Venom Blast and Spider Camouflage are Specials. They are resolved only when another
  effect instructs a player to resolve them; they are not free player actions.
- Venom Blast's damage is damage from the hero's ability, including when Web-Shot
  instructs its resolution. It is not attack damage. Its stun applies to the same
  enemy selected for the damage. Spider Camouflage grants tough and confuses an enemy.
- Arachnobatics checks whether the enemy has each status card. A Steady enemy with
  one stun or confuse card still grants the corresponding damage bonus, even though
  it is not yet stunned or confused under Steady's rule.
- The resource conditions on Double Life, Swing In and Web-Shot use the resources
  actually spent. An unspent generated resource does not satisfy the condition;
  a wild resource counts only as the declared type.
- The resource reduction on a Requirement card cannot remove its mandatory spending
  requirement. Putting that card into play is different from playing it.
- Power Within and Defense Mechanism respond to the hero's completed basic powers.
  Their discard is a cost; the Special resolves afterward with the hero as its source.
- Jefferson Davis compares all schemes' threat before selecting a scheme tied for
  least threat. Another scheme with threat does not allow bypassing a zero-threat
  scheme that has the least threat.
- A cost that exhausts “up to” some number of cards requires at least one card under
  the current RR Cost entry. Homeland Intervention therefore exhausts one to three
  S.H.I.E.L.D. cards. Dum Dum Dugan's optional effect is not such a cost.
- Field Agent prevents consequential damage to a S.H.I.E.L.D. ally, including an ally
  another player controls. Agent 13 may likewise ready another player's support.
- Dum Dum Dugan has THW 3 with **three** consequential damage and ATK 3 with **two**.
  The Justice Spider-Woman has ATK 2 with **two** consequential damage.

## Starter deck

The fifteen signature cards are Arachnobatics ×2, Double Life ×2, Swing In ×2,
Web-Shot ×3, Ganke Lee, Jefferson Davis, Power Within, Defense Mechanism and Web-Shooter ×2.

The other twenty-five are Monica Chang, Spider-Woman (Jessica Drew), Homeland
Intervention ×3, Global Logistics ×3, Field Agent ×3, Surveillance Team ×2,
Agent 13, Dum Dum Dugan, Ghost-Spider, Spider-Man (Peter Parker), Young Love, Energy,
Genius, Strength, Government Liaison ×3 and Sky-Destroyer.
The obligation and nemesis set are not part of the forty-card deck.
