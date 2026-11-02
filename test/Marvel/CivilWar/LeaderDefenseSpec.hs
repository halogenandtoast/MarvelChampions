module Marvel.CivilWar.LeaderDefenseSpec (spec) where

import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (withLeader,attachmentIdOf,leaderHp)
import Marvel.Matchers
import Marvel.Minion.Cards qualified as M
import TestImport

spec :: Spec
spec = describe "Leader defensive attachments" do
  for_ [("caps an attack above three",Nothing,6,3,False)
       ,("keeps Cosmic Flight against an attack of three",Nothing,3,3,True)
       ,("does not cap non-attack damage",Just FromAbility,6,6,True)
       ,("caps another leader's attack",Just FromAttack,6,3,False)] $ \(title,kind,amount,taken,remains) ->
    it title do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
            source = IdentityRef you
            actual = fromMaybe (FromPlayerAttack you) kind
        vid <- withLeader "56092"
        flight <- encounterCard C.cw56100
        resolve [RevealEncounterCard you flight]
        before <- leaderHp vid
        resolve [VillainMessage vid $ VillainDamaged source (toDamage amount actual)]
        leaderHp vid `shouldReturn` (before - taken)
        getAttachedCharacter (attachmentIdOf flight) `shouldReturn` (if remains then Just (VillainCharacter vid) else Nothing)

  it "keeps the second Cosmic Flight after the first caps the incoming damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "56092"
      flight1 <- encounterCard C.cw56100
      flight2 <- encounterCard C.cw56100
      resolve [RevealEncounterCard you flight1,RevealEncounterCard you flight2]
      before <- leaderHp vid
      resolve [VillainMessage vid $ VillainDamaged (IdentityRef you) (toDamage 6 $ FromPlayerAttack you)]
      leaderHp vid `shouldReturn` (before - 3)
      attachments <- traverse (getAttachedCharacter . attachmentIdOf) [flight1,flight2]
      liftIO $ length (catMaybes attachments) `shouldBe` 1

  it "keeps Cosmic Flight when tough prevents the attack first" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "56092"
      flight <- encounterCard C.cw56100
      resolve [RevealEncounterCard you flight,GiveStatus (VillainRef vid) ToughStatus]
      before <- leaderHp vid
      resolve [VillainMessage vid $ VillainDamaged (IdentityRef you) (toDamage 6 $ FromPlayerAttack you)]
      leaderHp vid `shouldReturn` before
      getAttachedCharacter (attachmentIdOf flight) `shouldReturn` Just (VillainCharacter vid)

  for_ [StunnedStatus,ConfusedStatus] $ \status ->
    it ("replaces " <> show status <> " with tough and discards Contaminant Immunity") do
      tg <- testGame ["spider-man"]
      inGame tg do
        vid <- withLeader "56168"
        immunity <- encounterCard C.cw56175
        resolve [RevealEncounterCard (player1 tg) immunity,GiveStatus (VillainRef vid) status]
        getAttachedCharacter (attachmentIdOf immunity) `shouldReturn` Nothing
        villainMatches VillainWithToughStatus vid `shouldReturn` True
        enemyMatches (if status == StunnedStatus then StunnedEnemy else ConfusedEnemy) (EnemyVillainId vid) `shouldReturn` False

  it "lets a subsequent stun reach Spider-Woman after Immunity leaves" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "56168"
      immunity <- encounterCard C.cw56175
      resolve [RevealEncounterCard you immunity,VillainMessage vid $ VillainConfused (IdentityRef you)]
      resolve [VillainMessage vid $ VillainStunned (IdentityRef you)]
      enemyMatches StunnedEnemy (EnemyVillainId vid) `shouldReturn` True
      enemyMatches ConfusedEnemy (EnemyVillainId vid) `shouldReturn` False

  it "prevents a hero's attack, stuns the hero, and discards Mass Increase" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      vid <- withLeader "57040"
      form <- encounterCard C.cw57046a
      mass <- encounterCard C.cw57049
      resolve [RevealEncounterCard you form,RevealEncounterCard you mass]
      before <- leaderHp vid
      choose "make a basic attack" $ \case UseAbility ab -> abilityIndex ab == 301; _ -> False
      choose "attack Vision" (isTargetChoice vid)
      choose "resolve Mass Increase" (isAbilityOf $ attachmentIdOf mass)
      pass
      leaderHp vid `shouldReturn` before
      isStunned you `shouldReturn` True
      getAttachedCharacter (attachmentIdOf mass) `shouldReturn` Nothing
      -- The prevention ends with this attack, even when the hero attacks again.
      resolve [RemoveStatus (IdentityRef you) StunnedStatus]
      resolve [VillainMessage vid $ VillainDamaged (IdentityRef you) (toDamage 2 $ FromPlayerAttack you)]
      leaderHp vid `shouldReturn` (before - 2)

  it "prevents every remaining target of a single attack event" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      vid <- withLeader "57040"
      minion <- addMinion you M.thomasEdison
      pass
      form <- encounterCard C.cw57046a
      mass <- encounterCard C.cw57049
      resolve [RevealEncounterCard you form,RevealEncounterCard you mass]
      before <- leaderHp vid
      melee <- putInHand you "05030"
      resources <- traverse (putInHand you) ["01089","01090"]
      playWith melee resources
      choose "attack Vision first" (isTargetChoice vid)
      choose "resolve Mass Increase" (isAbilityOf $ attachmentIdOf mass)
      saved <- getsGame toJSON
      case fromJSON @Game saved of
        Success restored -> withGame_ $ const restored
        Error reason -> liftIO $ expectationFailure reason
      choose "attack the minion with the same event" (isTargetChoice minion)
      pass
      leaderHp vid `shouldReturn` before
      minionDamageOf minion `shouldReturn` Just 0
      isStunned you `shouldReturn` True

  it "does not trigger Mass Increase while Vision is Intangible" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      vid <- withLeader "57040"
      form <- encounterCard C.cw57046b
      mass <- encounterCard C.cw57049
      resolve [RevealEncounterCard you form,RevealEncounterCard you mass]
      before <- leaderHp vid
      choose "make a basic attack" $ \case UseAbility ab -> abilityIndex ab == 301; _ -> False
      choose "attack Vision" (isTargetChoice vid)
      choose "reduce damage with Intangible" (isAbilityOf $ attachmentIdOf form)
      pass
      leaderHp vid `shouldReturn` (before - 1)
      isStunned you `shouldReturn` False
      getAttachedCharacter (attachmentIdOf mass) `shouldReturn` Just (VillainCharacter vid)

  it "prevents an ally's damage and stuns that ally" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      form <- encounterCard C.cw57046a
      mass <- encounterCard C.cw57049
      resolve [RevealEncounterCard you form,RevealEncounterCard you mass]
      ally <- putIntoPlay you "01067"
      let aid = allyIdOf ally
      before <- leaderHp vid
      choose "attack with Black Cat" $ \case UseAbility ab -> abilitySource ab == AllyRef aid && abilityIndex ab == 301; _ -> False
      choose "attack Vision" (isTargetChoice vid)
      choose "resolve Mass Increase" (isAbilityOf $ attachmentIdOf mass)
      pass
      leaderHp vid `shouldReturn` before
      characterMatches (CharacterWithStatus StunnedStatus) (AllyCharacter aid) `shouldReturn` True
      getAttachedCharacter (attachmentIdOf mass) `shouldReturn` Nothing

  for_ [(C.cw56100,"56092"),(C.cw56175,"56168")] $ \(def,leader) ->
    it ("reveals the defensive boost " <> show (toCardCode def)) do
      tg <- testGame ["spider-man"]
      inGame tg do
        vid <- withLeader leader
        card <- encounterCard def
        _ <- stackEncounterDeck [M.hydraMercenary]
        resolve [RevealBoostCard card (EnemyVillainId vid),ClearBoosts]
        getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (VillainCharacter vid)
