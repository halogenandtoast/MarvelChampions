# Synthezoid Smackdown acceptance audit

Scope: all 80 non-player card faces in the `synthezoid` pack, plus its eight leader reward dependencies. Ordinary player-card expansions are outside this work. Fear No Evil and Shadowland are deferred to a later session at the user's request.

All Synthezoid requirements below are verified. Final acceptance: 8,959 unit examples, 37 randomized game cases with two seeds each, seven browser cases, the production frontend build/typecheck and live API checks all pass. This audit accompanies the native scenario completion commit. The original multi-expansion goal remains incomplete because Fear No Evil and Shadowland are deferred.

| Requirement | Implementation and verification |
|---|---|
| Complete card registration | Whole-pack `CoverageSpec` checks all 88 viewer rows against the appropriate factories, including leader stages, both main-scheme faces, obligations and required rewards. It passes in the final complete unit suite: 8,959 examples, zero failures. |
| She-Hulk and Vision | Existing leader, attachment, treachery, side-scheme, defense and main-scheme behavior tests cover both leaders' signature mechanics and their shared Civil War rules. Native scenario tests exercise both difficulties and one- and two-hero cooperative setup. |
| Eight modular sets | All eight have individual behavior tests and verified five-physical-card gathering. Each was also checked in randomized conventional and cooperative Civil War games. See [modular evidence](encounter-synthezoid-modulars.md). |
| Eight leader rewards | All eight have behavior tests, including actual payment, optional timing, mass forms, cross-board targets and stun replacement. See [reward evidence](player-synthezoid.md). |
| Official errata | Moon Knight has a once-per-phase replay limit, including recursion and reload checks. Phased Out uses Hero Action or Hero Response. Both corrections are included in viewer data and its generator. |
| Cooperative and competitive setup | Both leaders are registered with their recommended modules and schemes. The scenario suite includes every three-by-three faction pairing in Normal and Expert, independent deck homes, proper physical Standard PvP printings, eight unowned rewards and two- and four-seat setup. |
| Dedicated multiplayer creation | Existing Civil War team creation exposes the new presets, all twelve modules per faction and all five choices for each main-scheme stage. The new browser case selects both leaders and checks their submitted defaults. Seven browser cases pass; the production frontend build and typecheck pass. |
| One account controlling both sides | A live isolated API check creates, seats and starts both two- and four-seat She-Hulk versus Vision games using one account. Reload preserves two independent boards, balanced team rosters and chosen presets. Live validation also rejects own-team She-Hulk and Vision hero choices for both seat counts. |
| Randomized play | All 37 game cases pass with two seeds each: twelve cooperative, five modular combinations and twenty competitive cases covering every new/mixed leader pairing in Normal/Expert with two/four seats. The log contains 58 total examples including 21 related behavior/setup checks, zero failures, in 2,052.4 seconds. The five-second question watchdog is unchanged. |
| Final regression | The final build succeeds. All 8,959 non-smoke unit examples pass, zero failures, in 209.8 seconds. This includes 78 native scenario/inventory examples, Synthezoid reward claiming and setup-time Game JSON restoration. |

Sources: the Synthezoid Smackdown rulebook, the shared Civil War rules, and Rules Reference v1.8, including its FAQ errata. Source URLs are catalogued in [README.md](README.md). No third-party card images or links are added.
