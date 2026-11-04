module Marvel.Encounter.ShadowlandProtegesSpec (spec) where

import Marvel.Attack (attackOverkill,attackPiercing)
import Marvel.FearNoEvil.Cards qualified as FNE
import Marvel.Matchers
import Marvel.Query
import TestImport

protegeGame :: CardCode -> IO TestGame
protegeGame code = do
  tg <- testGame ["captain-marvel"]
  tg <$ inGame tg do
    let you = player1 tg
    changeForm you
    clearHand you
    vid <- villainIdNow
    resolve [ReplaceVillain vid code Advanced]
    void $ setEncounterDeck (replicate 8 FNE.street60188) []

spec :: Spec
spec = describe "Shadowland protege villain abilities" do
  for_ ["65012a","65012b"] $ \code -> do
    it (show code <> " gives an eligible minion tough after attacking") do
      tg <- protegeGame code
      inGame tg do
        let you = player1 tg
        mid <- addMinion you FNE.street60092
        vid <- villainIdNow
        resolve [VillainMessage vid $ VillainAttacks you]
        getCharacterStatusCount (MinionCharacter mid) ToughStatus `shouldReturn` 1

    it (show code <> " discards three cards and deals a discarded minion when no minion can gain tough") do
      tg <- protegeGame code
      inGame tg do
        let you = player1 tg
        vid <- villainIdNow
        ([_,_,minion,_,_],_) <- setEncounterDeck [FNE.street60188,FNE.street60085,FNE.street60092,FNE.street60094,FNE.street60188] []
        resolve [VillainMessage vid $ VillainAttacks you]
        map ecCardId . playerIdentityEncounterCards . toAttrs <$> getPlayer you `shouldReturn` [ecCardId minion]
        selectAny (MinionWithId $ MinionId $ unCardId $ ecCardId minion) `shouldReturn` False
        getEncounterDiscard >>= \cards -> liftIO $ map ecCardId cards `shouldNotContain` [ecCardId minion]
        length <$> getEncounterDeck `shouldReturn` 1

  for_ ["65013a","65013b"] $ \code ->
    it (show code <> " retaliates against an attacking player once per round when that player cannot discard") do
      tg <- protegeGame code
      inGame tg do
        let you = player1 tg
        vid <- villainIdNow
        choose "make the first basic attack" (isHeroAttack you)
        choose "attack Daredevil" (isTargetChoice vid)
        choose "resolve Daredevil's response" (isAbilityOf vid)
        choose "Daredevil attacks" (isLabel "$label.slProtegeAttacks")
        pass
        damageOf you `shouldReturn` 2
        resolve [IdentityMessage you ReadiedIdentity]
        choose "make a second basic attack" (isHeroAttack you)
        choose "attack Daredevil again" (isTargetChoice vid)
        pass
        damageOf you `shouldReturn` 2

  for_ ["65014a","65014b"] $ \code ->
    for_ [("01005",True),("01009",False)] $ \(discardedCode,oddCost) ->
      it (show code <> " resolves the printed-cost branch for " <> show discardedCode) do
        tg <- protegeGame code
        inGame tg do
          let you = player1 tg
          vid <- villainIdNow
          [discarded] <- stackDeck you [discardedCode]
          unless oddCost $ resolve [GiveStatus (IdentityRef you) ToughStatus]
          runNow [VillainMessage vid $ VillainAttacks you]
          choose "resolve Elektra's discard" (isAbilityOf vid)
          attack <- getEnemyAttack $ EnemyVillainId vid
          (attackOverkill <$> attack) `shouldBe` Just (not oddCost)
          (attackPiercing <$> attack) `shouldBe` Just (not oddCost)
          pass
          damageOf you `shouldReturn` (if oddCost then 3 else 2)
          getCharacterStatusCount (IdentityCharacter you) ToughStatus `shouldReturn` 0
          discardOf you >>= \cards -> liftIO $ map pcCardId cards `shouldContain` [pcCardId discarded]

  for_ ["65015a","65015b"] $ \code ->
    for_ [False,True] $ \alreadyStunned ->
      it (show code <> " offers only a possible stun and otherwise takes its indirect-damage option, stunned=" <> show alreadyStunned) do
        tg <- protegeGame code
        inGame tg do
          let you = player1 tg
          vid <- villainIdNow
          when alreadyStunned $ resolve [GiveStatus (IdentityRef you) StunnedStatus]
          runNow [VillainMessage vid $ VillainAttacks you]
          choose "resolve Gorgon's interrupt" (isAbilityOf vid)
          offers (isLabel "$label.stunYourIdentity") `shouldReturn` not alreadyStunned
          choose "resolve the selected penalty" (isLabel $ if alreadyStunned then "$label.slTwoIndirect" else "$label.stunYourIdentity")
          pass
          damageOf you `shouldReturn` (if alreadyStunned then 4 else 2)
          getCharacterStatusCount (IdentityCharacter you) StunnedStatus `shouldReturn` 1

  it "Gorgon's physical-resource option pays a resource instead of applying either penalty" do
    tg <- protegeGame "65015a"
    inGame tg do
      let you = player1 tg
      backflip <- putInHand you "01003"
      vid <- villainIdNow
      runNow [VillainMessage vid $ VillainAttacks you]
      choose "resolve Gorgon's interrupt" (isAbilityOf vid)
      choose "spend a physical resource" (isLabel "$label.slSpendPhysical")
      payWith [backflip]
      pass
      damageOf you `shouldReturn` 2
      getCharacterStatusCount (IdentityCharacter you) StunnedStatus `shouldReturn` 0

  for_ [("65016a",2),("65016b",3)] $ \(code,damage) -> do
    it (show code <> " deals one indirect damage after an undefended attack") do
      tg <- protegeGame code
      inGame tg do
        vid <- villainIdNow
        resolve [VillainMessage vid $ VillainAttacks $ player1 tg]
        damageOf (player1 tg) `shouldReturn` damage
        mainSchemeThreatNow `shouldReturn` 0

    it (show code <> " places threat after a defended attack instead of dealing indirect damage") do
      tg <- protegeGame code
      inGame tg do
        let you = player1 tg
        ally <- putIntoPlay you "01050"
        vid <- villainIdNow
        runNow [VillainMessage vid $ VillainAttacks you]
        choose "defend with Hulk" (\case AllyDefend who _ -> who == allyIdOf ally; _ -> False)
        choose "resolve Lady Bullseye's response" (isAbilityOf vid)
        pass
        damageOf you `shouldReturn` 0
        mainSchemeThreatNow `shouldReturn` 1

  for_ [("65017a",1,1),("65017b",2,3)] $ \(code,firstDamage,totalDamage) ->
    it (show code <> " repeats only its first attack that round with its printed bonus") do
      tg <- protegeGame code
      inGame tg do
        let you = player1 tg
        vid <- villainIdNow
        resolve [VillainMessage vid $ VillainAttacks you]
        damageOf you `shouldReturn` firstDamage
        length <$> getEncounterDeck `shouldReturn` 6
        resolve [VillainMessage vid $ VillainAttacks you]
        damageOf you `shouldReturn` totalDamage
        length <$> getEncounterDeck `shouldReturn` 5
