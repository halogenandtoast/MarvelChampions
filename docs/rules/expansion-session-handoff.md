# Expansion implementation handoff

The user asked to stop after completing Synthezoid Smackdown and resume Fear No Evil and Shadowland in another session. Do not treat this as completion of the original multi-expansion goal.

## Completed milestones

- Civil War: committed before Synthezoid work, completion commit `5dcdf0e02d`. See [Civil War audit](civil-war-completion-audit.md).
- Ronan modular: commit `76276fe6da`, five distinct cards/seven physical cards, with behavior and randomized checks.
- Synthezoid: all 80 non-player faces, eight modulars, eight required leader rewards and native cooperative/competitive scenario/UI integration are complete and verified; see [acceptance audit](synthezoid-completion-audit.md).

## Remaining expansion scope

Fear No Evil and Shadowland gameplay implementation remains. Viewer text is already available in `frontend/public/data/cards.json`: 239 Fear No Evil faces and 93 distinct Shadowland faces (108 physical Shadowland cards). The viewer pack keys are `fne` and `shadowland`; `stld` is Star-Lord. Existing player heroes/cards and their nemesis work should be preserved. The user excluded ordinary player cards from the expansion task; required scenario/leader dependencies belong in scope.

Use the rulebook URLs catalogued in [README.md](README.md), local Rules Reference v1.8 and its FAQ, and recent official rulings when a rules question arises. Never link to the third-party simulator, download its images or use its images; the repository already has card images.

The engine is under `src/Marvel`, API under `app`, client under `frontend`. Follow the repository's lifted message-layer conventions and behavior-test style. Refer to `~/Code/LordOfTheRings` before introducing new server/client feature patterns.

## Verification workflow

Only one Stack command at a time. Use `--fast --ghc-options -DDEVELOPMENT`. Keep randomized smoke isolated from compilation and unit-suite execution; its five-second question watchdog remains unchanged. The development build watcher was restored after verification, and the normal API on port 3020 serves both Synthezoid scenarios. The isolated verification API and Vite server were stopped. Never stop the parent `dev.up` process: its cleanup kills its process group.

The isolated API verification database is `marvelchampions_cw_verify_20261103`, separate from the normal `marvelchampions` development database. Verification scripts/logs live under `/tmp/mc-synthezoid-*`; durable findings belong in the rules documents and committed tests.
