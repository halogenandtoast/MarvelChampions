# Mutant Genesis campaign

Non-normative implementation reference. The [official English rulebook](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc32_mutant_genesis_rulebook_v5-compressed.pdf) and [campaign log](https://cdn.svc.asmodee.net/production-fantasyflightgames/uploads/2026/09/mc32_mutant_genesis_campaign_log.pdf) are authoritative. Card summaries use the local MarvelCDB dataset; printed text and current [errata](faq-errata.md) take precedence.

## Campaign instructions (rulebook pp4-5,7,10,12,16,19)
All campaign setup follows ordinary scenario setup. Win five scenarios: 32060 Sabretooth, 32084 Project Wideawake, 32109 Master Mold, Mansion Attack (32121a), 32138 Magneto. Only victories apply log updates. Ordinary losses retry with no penalty. Expert last-scenario loss loses campaign (existing generic logic).

### Roles and role-building
Scenario1 each player chooses a distinct role; randomly deal one of their five role upgrades into play. Later scenarios grant random remaining role upgrade only if immediately preceding campaign side scheme defeated. Any role upgrade that began a won game in play is removed permanently whether used or not. Cards also explicitly remove themselves from game/pool on activation.
Role-building: during setup may add up to one event and/or one upgrade from either associated aspect, only if chosen title not already in own deck, only for that game, exempt deck size. Follow the conditional scenario instructions: in scenarios 2–5 this option requires defeating the preceding campaign side scheme (project-owner ruling below). Choices are restricted by card type, associated aspect, and absence of that title from the player’s deck.

### Future Past
Five cards 32166-32170. Scenario1 shuffle all set aside. Each later scenario shuffle recorded active cards into encounter deck, shuffle remaining unused/unremoved Future Past cards set aside. Campaign side-scheme defeat shuffles top set-aside Future Past card into encounter deck. Victory1-4 removes victory-display Future Past cards permanently and replaces recorded active cards with those in encounter deck/discard/in play. Do not reintroduce permanently defeated cards into later Future Past deck.

### Scenario1
Choose roles/deal upgrades/role-build; prepare Future Past deck; reveal Frightened Police 32171a. Victory: record scheme defeated; process Future Past; remove initial role upgrades; expert record surviving HP capped printed base.
### Scenario2
Future Past setup; grant role upgrade conditional on Frightened Police defeated; reveal Enemy of My Enemy 32172a; expert restore recorded HP and offer acceleration token for full heal. Victory: record scheme defeated; process Future Past/remove upgrades; record Jubilee 32088b if in play; record every CAPTIVE ally that ENTERED play (event history, even if later defeated); expert HP.
### Scenario3
The Sentinel Factory’s ordinary printed Setup puts the Magneto ally into play in campaign mode as well as standalone play. The previous campaign side-scheme result does not disable this instruction.
Future Past; Jubilee if logged put into play; may distribute logged CAPTIVE allies into any player decks; upgrade reward conditional Enemy of My Enemy; reveal Find the Prisoners 32173a; expert HP/heal. Victory: record scheme defeated; Future Past/upgrades; update Jubilee presence (clear if absent); every ally still under Find the Prisoners/Rescue Captives removed for rest of campaign; expert HP.
### Scenario4
Future Past; Jubilee; distribute logged CAPTIVE allies; upgrade reward conditional Find the Prisoners; reveal Surprise Attack 32174a; expert HP/heal. Victory: record scheme defeated; Future Past/upgrades; update Jubilee; expert HP.
### Scenario5
Future Past; Jubilee; distribute CAPTIVE allies; reward conditional Surprise Attack; reveal Magnetos Fortress 32175a; expert HP/heal; expert loss loses campaign. Victory campaign won.
Expert eliminated players skip victory personal steps and must take acceleration token heal to rejoin. Existing Sinister runner hp==0 path is useful; verify where zero recorded for eliminated identities.

## Campaign cards 171-175 (all dual faces)
- 32171a Frightened Police: defeat adds Future Past then flips to 32171b Metro PD support (Permanent; first player controls; exhaust choose 1 damage enemy or remove 1 threat scheme).
- 32172a Enemy of My Enemy: defeat Future Past then flips to 32172b Magneto ally (THW2/ATK3/HP5; Victory1; first player control; ignores ally limit; after attacks defeats Sentinel heal1).
- 32173a Find Prisoners: reveal each searches deck ally puts under; defeat Future Past then flip/reveal 32173b Rescue Captives preserving facedown cards. Rescue response after YOU defeat Sentinel: spend resource choose stored ally into play under your control, any player.
- 32174a Surprise Attack: defeat Future Past then defeating player puts 32174b Reactive Defense obligation in own area. AE action spend E/M/P deal5 each enemy remove from game. The front refers to its reverse by a slightly different title in MarvelCDB; use the linked reverse card, Reactive Defense (32174b).
- 32175a Magnetos Fortress: defeat Future Past then flips to 32175b Magnetos Power attachment, attach villain Magneto, Permanent +1SCH/+1ATK.

## Role upgrades 176-195
Brawler: Coup de Grace 176 (attack +3 damage and overkill); Swagger177 (basic defend +3DEF ready hero); Brazen Defense178 (enemy attacks pay1 prevent3/deal3); Ferocious Attack179 (pay3 attack6 ready hero); War Cry180 (hero resource WW for Attack/Defense event; tough).
Commander: Coup de Grace181; Compassion182 (after AE recover heal3 divided own characters draw1); Group Assault183 (phase all ally attacking consequential damage prevented); Shock and Awe184 (pay3 attack6 ready own allies); Improvisation185 (WW Attack/Tactic event ready/heal2 ally).
Defender: Swagger186; Surprise187 (after thwart divide3 threat removal/confuse enemy); Heroic Intervention188 (pay3 thwart divide5/tough); Determined Defense189 (defend pay2 attack removes main threat INSTEAD damage); Bodyguard190 (WW Defense/Thwart event draw1).
Peacekeeper: Surprise191; Compassion192; Rescue Operation193 (phase prevent all ally thwart consequential damage); Mentorship194 (pay3 thwart divide5 ready all own allies); Fortitude195 (WW Tactic/Thwart event stun enemy).
Every role card removes itself game/pool; test even when remaining effects partially fail. Determined Defense may need new enemy attack damage replacement primitive; resource side effects must occur once per resource ability resolution and at correct payment timing.

## Rulings (from the project owner, 2026-10-30)

- Follow the conditional setup indentation. During scenarios 2–5, role-building is available only if the preceding scenario's campaign side scheme was defeated. Scenario 1 always allows role-building.

## Timing and implementation requirements

The Future Past deck and role draws use the game's stored random generator. Role choices resolve sequentially so two players cannot choose the same role. Role-building cards are temporary additions for that game, not permanent campaign rewards. Record allies that entered play during Project Wideawake even if they subsequently left play. Find the Prisoners and Rescue Captives retain the same facedown allies across their card-type change. Victory instructions must inspect those held cards before the board is discarded. Exclude permanently lost allies from all future campaign deck construction and rescued-ally distribution.
