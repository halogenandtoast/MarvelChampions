module Marvel.CivilWar.MainSchemeSpec (spec) where

import Marvel.CivilWar.LeaderSpec (withLeader)
import Marvel.Keyword qualified as K
import Marvel.MainScheme.Types qualified as Main
import Marvel.Minion.Cards qualified as M
import Marvel.Treachery.Cards qualified as T
import Marvel.Modifier
import TestImport

replaceScheme :: CardCode -> GameAppT MainSchemeId
replaceScheme code = do
  old <- mainSchemeId
  resolve [ReplaceMainScheme old code]
  mainSchemeId

spec :: Spec
spec = describe "Civil War main schemes" do
  for_ ["56063b","56172b","56121b","56199b","56122b","57044b","57005b","56200b"] $ \code ->
    for_ [("solo",["spider-man"],0,7),("two heroes",["spider-man","captain-marvel"],4,14)] $ \(description,heroes,initial,target) ->
      it (show code <> " resolves its initial threat and target for " <> description) do
        tg <- testGame heroes
        inGame tg do
          sid <- replaceScheme code
          mainSchemeThreat `shouldReturn` initial
          scheme <- getsGame $ fromMaybe (error "missing scheme") . lookup sid . gameMainSchemes
          Main.getThreshold (toAttrs scheme) `shouldReturn` target

  for_ ["56063b","56172b"] $ \code ->
    it (show code <> " limits its treachery threat to once per player per phase") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        _ <- replaceScheme code
        vid <- withLeader "57040"
        resolve [VillainMessage vid $ VillainDamaged GameRef $ toDamage 10 FromAbility]
        first <- encounterCard T.hardToKeepDown
        second <- encounterCard T.hardToKeepDown
        resolve [RevealEncounterCard you first,RevealEncounterCard you second]
        mainSchemeThreat `shouldReturn` 1
        _ <- stackEncounterDeck [M.hydraMercenary,M.hydraMercenary]
        endTurn
        pass
        before <- mainSchemeThreat
        third <- encounterCard T.hardToKeepDown
        resolve [RevealEncounterCard you third]
        mainSchemeThreat `shouldReturn` (before + 1)

  for_ ["57005b","56200b"] $ \code ->
    it (show code <> " requires two stuns to cancel the leader's attack and removes both") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        _ <- replaceScheme code
        vid <- withLeader "56059"
        changeForm you
        _ <- stackEncounterDeck [M.hydraMercenary,M.hydraMercenary]
        resolve [VillainMessage vid (VillainStunned GameRef)]
        start [VillainMessage vid $ VillainAttacks you]
        pass
        firstDamage <- damageOf you
        liftIO $ firstDamage `shouldSatisfy` (> 0)
        getCharacterStatusCount (VillainCharacter vid) StunnedStatus `shouldReturn` 1
        resolve [VillainMessage vid (VillainStunned GameRef),VillainMessage vid $ VillainAttacks you]
        damageOf you `shouldReturn` firstDamage
        getCharacterStatusCount (VillainCharacter vid) StunnedStatus `shouldReturn` 0
        resolve [VillainMessage vid (VillainStunned GameRef)]
        start [VillainMessage vid $ VillainAttacks you]
        pass
        damageOf you >>= (\damage -> liftIO $ damage `shouldSatisfy` (> firstDamage))

  for_ ["56121b","56199b"] $ \code ->
    it (show code <> " grants Guard to ordinary minions while it is in play") do
      tg <- testGame ["spider-man"]
      inGame tg do
        sid <- replaceScheme code
        minion <- addMinion (player1 tg) M.weaponsRunner
        getEnemyKeywords (EnemyMinionId minion) >>= (`shouldSatisfy` elem K.Guard)
        resolve [ReplaceMainScheme sid "01097"]
        getEnemyKeywords (EnemyMinionId minion) >>= (`shouldSatisfy` notElem K.Guard)

  for_ ["56124b","57045b"] $ \code ->
    it (show code <> " exhausts allies as they enter play") do
      tg <- testGame ["spider-man"]
      inGame tg do
        _ <- replaceScheme code
        cat <- putIntoPlay (player1 tg) "01067"
        getsGame (fmap (allyExhausted . toAttrs) . lookup (allyIdOf cat) . gameAllies) `shouldReturn` Just True

  for_ ["56122b","57044b"] $ \code ->
    it (show code <> " discards three cards after the villain phase threat step") do
      tg <- testGame ["spider-man"]
      inGame tg do
        sid <- replaceScheme code
        _ <- withLeader "57040"
        _ <- stackEncounterDeck [M.hydraMercenary,M.hydraMercenary]
        endTurn
        before <- unDeck . playerIdentityDeck <$> playerAttrs (player1 tg)
        mainSchemeThreat `shouldReturn` 1
        choose "resolve the main scheme after threat" (isAbilityOf sid)
        after <- unDeck . playerIdentityDeck <$> playerAttrs (player1 tg)
        liftIO $ after `shouldBe` drop 3 before
        mainSchemeThreat `shouldReturn` 3

  for_ ["56142b","57006b"] $ \code ->
    it (show code <> " increases an undefended enemy leader attack by two") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        _ <- replaceScheme code
        vid <- withLeader "57040"
        changeForm you
        _ <- stackEncounterDeck [M.hydraMercenary]
        start [VillainMessage vid $ VillainAttacks you]
        pass
        damageOf you `shouldReturn` 4
        getModifiers (VillainRef vid) >>= (`shouldSatisfy` notElem (AttackModifier 2))

  for_ ["56142b","57006b"] $ \code ->
    it (show code <> " gives only one extra ATK to a defended attack") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        _ <- replaceScheme code
        vid <- withLeader "57040"
        changeForm you
        _ <- stackEncounterDeck [M.hydraMercenary]
        start [VillainMessage vid $ VillainAttacks you]
        let decline = whenM (offers isNoResponse) $ choose "decline optional responses" isNoResponse >> decline
        decline
        choose "defend with Spider-Man" isDefend
        pass
        damageOf you `shouldReturn` 0

  for_ ["56124b","57045b","56142b","57006b"] $ \code ->
    it (show code <> " loses a cooperative game when its nine-per-player target is reached") do
      tg <- testGame ["spider-man","captain-marvel"]
      inGame tg do
        sid <- replaceScheme code
        resolve [MainSchemeMessage sid $ MainSchemePlaceThreat 17]
        getsGame gameState `shouldReturn` InProgress
        resolve [MainSchemeMessage sid $ MainSchemePlaceThreat 1]
        getsGame gameState `shouldReturn` Finished Lost
