module Marvel.CivilWar.LeaderSpec (spec,withLeader,leaderHp,attachmentIdOf) where

import Control.Monad (replicateM_)
import Marvel.CivilWar.Cards qualified as C
import Marvel.Hp (HP (..),unHp)
import Marvel.Keyword qualified as K
import Marvel.Matchers
import Marvel.Modifier
import Marvel.Message.Lifted qualified as L
import Marvel.Phase (VillainPhaseStep (..))
import Marvel.Minion.Cards qualified as M
import Marvel.Treachery.Cards qualified as T
import Marvel.SideScheme.Cards qualified as S
import Marvel.Villain.Types qualified as V
import TestImport

withLeader :: CardCode -> GameAppT VillainId
withLeader code = do
  vid <- villainIdNow
  resolve [ReplaceVillain vid code Swapped,VillainMessage vid SetVillainHp]
  let (_,_,_,stage) = C.printedLeaderStats code
  withGame_ $ entitiesL . villainsL . ix vid %~ \(V.Villain leader) -> V.Villain $ V.overVillainAttrs (\a -> a {V.villainStage = stage}) leader
  pure vid

attachmentIdOf :: EncounterCard -> AttachmentId
attachmentIdOf = AttachmentId . unCardId . ecCardId

leaderHp :: VillainId -> GameAppT Int
leaderHp vid = getsGame $ maybe 0 (unHp . V.villainHp . toAttrs) . lookup vid . gameVillains

spec :: Spec
spec = describe "Civil War leaders" do
  for_ ["56059","56061","56092","56094","56137","56139","56168","56170","57001","57003","57040","57042"] $ \code ->
    it ("advances " <> show code <> " and prevents further damage until the phase ends") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        changeForm you
        vid <- withLeader code
        withGame_ $ entitiesL . villainsL . ix vid %~ \(V.Villain leader) -> V.Villain $ V.overVillainAttrs (\a -> a {V.villainHp = HP 1}) leader
        choose "make a basic attack" $ \case UseAbility ability -> abilityIndex ability == 301; _ -> False
        choose "attack the leader" (isTargetChoice vid)
        choose "resolve the leader's next stage" (isAbilityOf vid)
        hp <- leaderHp vid
        resolve [VillainMessage vid $ VillainDamaged GameRef (toDamage 1 FromAbility)]
        leaderHp vid `shouldReturn` hp
        modifiers <- getModifiers (VillainRef vid)
        liftIO $ modifiers `shouldSatisfy` elem CannotTakeDamage
        encounters <- playerIdentityEncounterCards . toAttrs <$> getPlayer you
        liftIO $ length encounters `shouldBe` 1
        let (_,_,_,stage) = C.printedLeaderStats code
        getsGame (fmap (V.villainStage . toAttrs) . lookup vid . gameVillains) `shouldReturn` Just (stage + 1)
        endTurn
        getModifiers (VillainRef vid) >>= (`shouldSatisfy` notElem CannotTakeDamage)

  it "attaches Cap's Shield and gives Captain America tough" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "56137"
      card <- encounterCard C.cw56143
      resolve [RevealEncounterCard you card]
      getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (VillainCharacter vid)
      villainMatches VillainWithToughStatus vid `shouldReturn` True
      getModifiers (VillainRef vid) >>= (`shouldSatisfy` elem (KeywordModifier $ K.Retaliate 1))
      getModifiers (VillainRef vid) >>= (`shouldSatisfy` elem (AttacksAgainstLoseKeyword K.Piercing))

  for_ [("the shield moves first",False,0),("retaliate resolves first",True,1)] $ \(description,retaliateFirst,expectedDamage) ->
    it ("lets the player choose whether " <> description) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        changeForm you
        vid <- withLeader "56137"
        card <- encounterCard C.cw56143
        resolve [RevealEncounterCard you card,VillainMessage vid $ VillainDamaged GameRef (toDamage 1 FromAbility)]
        choose "make a basic attack" $ \case UseAbility ability -> abilityIndex ability == 301; _ -> False
        choose "attack Captain America" (isTargetChoice vid)
        offers (isLabeledAbility "$label.resolveRetaliate") `shouldReturn` True
        offers (isAbilityOf $ attachmentIdOf card) `shouldReturn` True
        when retaliateFirst $ choose "resolve retaliate" (isLabeledAbility "$label.resolveRetaliate")
        choose "take Cap's Shield" (isAbilityOf $ attachmentIdOf card)
        pass
        getAttachedCharacter (attachmentIdOf card) `shouldReturn` Just (IdentityCharacter you)
        damageOf you `shouldReturn` expectedDamage
        getModifiers (IdentityRef you) >>= (`shouldSatisfy` elem (KeywordModifier $ K.Retaliate 1))

  for_ [("return the shield before retaliating",False,0),("retaliate before returning the shield",True,1)] $ \(description,retaliateFirst,expectedDamage) ->
    it ("lets the attacked hero " <> description) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        changeForm you
        vid <- withLeader "56137"
        card <- encounterCard C.cw56143
        let aid = attachmentIdOf card
        resolve [RevealEncounterCard you card,VillainMessage vid $ VillainDamaged GameRef (toDamage 1 FromAbility),AttachmentMessage aid $ AttachedToIdentity you]
        before <- leaderHp vid
        _ <- stackEncounterDeck [M.hydraMercenary]
        start [VillainMessage vid $ VillainAttacks you]
        pass
        offers (isLabeledAbility "$label.resolveRetaliate") `shouldReturn` True
        offers (isAbilityOf aid) `shouldReturn` True
        when retaliateFirst $ choose "resolve retaliate" (isLabeledAbility "$label.resolveRetaliate")
        choose "return Cap's Shield" (isAbilityOf aid)
        choose "resolve Captain America's tough" (isTargetChoice vid)
        pass
        getAttachedCharacter aid `shouldReturn` Just (VillainCharacter vid)
        leaderHp vid `shouldReturn` (before - expectedDamage)
        villainMatches VillainWithToughStatus vid `shouldReturn` True

  it "saves and reloads a leader and its shield during a forced-response choice" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      vid <- withLeader "56137"
      shield <- encounterCard C.cw56143
      resolve [RevealEncounterCard you shield,VillainMessage vid $ VillainDamaged GameRef (toDamage 1 FromAbility)]
      choose "make a basic attack" $ \case UseAbility ability -> abilityIndex ability == 301; _ -> False
      choose "attack Captain America" (isTargetChoice vid)
      saved <- getsGame toJSON
      case fromJSON @Game saved of
        Success restored -> liftIO $ toJSON restored `shouldBe` saved
        Error reason -> liftIO $ expectationFailure reason

  it "allows an ATK-based attack event to take Cap's Shield" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      vid <- withLeader "56137"
      shield <- encounterCard C.cw56143
      resolve [RevealEncounterCard you shield,VillainMessage vid $ VillainDamaged GameRef (toDamage 1 FromAbility)]
      counter <- putInHand you "01077"
      _ <- stackEncounterDeck [M.hydraMercenary]
      start [VillainMessage vid $ VillainAttacks you]
      chooseIf (isLabel "$label.noResponse")
      choose "defend Captain America's attack" isDefend
      playWith counter []
      choose "take the shield after Counter-Punch" (isAbilityOf $ attachmentIdOf shield)
      pass
      getAttachedCharacter (attachmentIdOf shield) `shouldReturn` Just (IdentityCharacter you)
      damageOf you `shouldReturn` 0

  it "allows a basic attack's overkill damage to take Cap's Shield" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      vid <- withLeader "56137"
      shield <- encounterCard C.cw56143
      resolve [RevealEncounterCard you shield,VillainMessage vid $ VillainDamaged GameRef (toDamage 1 FromAbility)]
      mid <- addMinion you M.hydraSoldier
      resolve [MinionMessage mid $ MinionDamaged GameRef (toDamage 3 FromAbility)]
      marked <- putInHand you "33032"
      playOn marked mid []
      choose "make a basic attack" $ \case UseAbility ability -> abilityIndex ability == 301; _ -> False
      choose "attack the wounded minion" (isTargetChoice mid)
      choose "take the shield after overkill" (isAbilityOf $ attachmentIdOf shield)
      pass
      getAttachedCharacter (attachmentIdOf shield) `shouldReturn` Just (IdentityCharacter you)

  it "keeps the Shield on the hero when Riposte prevents all attack damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      vid <- withLeader "56137"
      shield <- encounterCard C.cw56143
      let aid = attachmentIdOf shield
      resolve [RevealEncounterCard you shield,VillainMessage vid $ VillainDamaged GameRef (toDamage 1 FromAbility),AttachmentMessage aid $ AttachedToIdentity you]
      before <- leaderHp vid
      riposte <- putInHand you "48018"
      resource <- putInHand you "01086"
      _ <- stackEncounterDeck [M.hydraMercenary]
      start [VillainMessage vid $ VillainAttacks you]
      chooseIf (isLabel "$label.noResponse")
      choose "defend" isDefend
      playWith riposte [resource]
      pass
      offers (isAbilityOf aid) `shouldReturn` False
      offers (isAbilityOf $ EventRef $ EventId $ unCardId $ pcCardId riposte) `shouldReturn` True
      choose "retaliate first" (isLabeledAbility "$label.resolveRetaliate")
      pass
      leaderHp vid `shouldReturn` (before - 4)
      getAttachedCharacter aid `shouldReturn` Just (IdentityCharacter you)
      damageOf you `shouldReturn` 0

  it "does not transfer the Shield after a flat-damage attack event" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      vid <- withLeader "56137"
      shield <- encounterCard C.cw56143
      resolve [RevealEncounterCard you shield,VillainMessage vid $ VillainDamaged GameRef (toDamage 1 FromAbility)]
      haymaker <- putInHand you "01087"
      energy <- putInHand you "01088"
      playWith haymaker [energy]
      choose "attack Captain America" (isTargetChoice vid)
      pass
      getAttachedCharacter (attachmentIdOf shield) `shouldReturn` Just (VillainCharacter vid)
      damageOf you `shouldReturn` 1

  for_ [(0,1),(3,0)] $ \(initialCounters,remaining) ->
    it ("resolves Energy Channel after retaliate defeats its attacker, from " <> show initialCounters <> " counters") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        changeForm you
        vid <- withLeader "56092"
        channel <- encounterCard C.cw56098
        let aid = attachmentIdOf channel
        resolve [RevealEncounterCard you channel,PlaceCounters (AttachmentRef aid) initialCounters]
        effects <- L.runQueueT $ L.capture $ L.lastingModifiers vid (SourceEntity $ VillainRef vid) [KeywordModifier $ K.Retaliate 2] False
        resolve effects
        cat <- putIntoPlay you "01002"
        choose "Black Cat attacks" $ \case UseAbility ability -> abilitySource ability == AllyRef (allyIdOf cat) && abilityIndex ability == 301; _ -> False
        choose "attack Captain Marvel" (isTargetChoice vid)
        choose "resolve retaliate" (isLabeledAbility "$label.resolveRetaliate")
        choose "resolve Energy Channel" (isAbilityOf aid)
        pass
        getCountersOn aid `shouldReturn` remaining
        damageOf you `shouldReturn` 0
        getsGame (isJust . lookup (allyIdOf cat) . gameAllies) `shouldReturn` False

  it "caps Finesse at three skill counters and spends them on Spider-Woman's next attack" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "56168"
      card <- encounterCard C.cw56174
      resolve [RevealEncounterCard you card]
      replicateM_ 4 do
        _ <- stackEncounterDeck [M.hydraMercenary]
        treachery <- encounterCard T.hardToKeepDown
        resolve [RevealEncounterCard you treachery]
      getCountersOn (attachmentIdOf card) `shouldReturn` 3
      _ <- stackEncounterDeck [M.hydraMercenary]
      start [VillainMessage vid $ VillainAttacks you]
      choose "spend Finesse" (isAbilityOf vid)
      getCountersOn (attachmentIdOf card) `shouldReturn` 0
      pass
      damageOf you `shouldReturn` 6

  it "places an energy counter when Captain Marvel defeats an ally with her attack" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "56092"
      channel <- encounterCard C.cw56098
      resolve [RevealEncounterCard you channel]
      ally <- putIntoPlay you "01084"
      _ <- stackEncounterDeck [S.breakinAndTakin]
      resolve [VillainMessage vid $ VillainAttacksAlly you (allyIdOf ally)]
      getCountersOn (attachmentIdOf channel) `shouldReturn` 1

  it "deals one damage after an alter-ego changes to hero form against She-Hulk" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "57001"
      changeForm (player1 tg)
      choose "resolve She-Hulk" (isAbilityOf vid)
      damageOf (player1 tg) `shouldReturn` 1

  it "gives She-Hulk two ATK and overkill, then discards Superhuman Strength after the attack" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "57001"
      strength <- encounterCard C.cw57007
      resolve [RevealEncounterCard (player1 tg) strength]
      getModifiers (VillainRef vid) >>= (`shouldSatisfy` elem (AttackModifier 2))
      _ <- stackEncounterDeck [M.hydraMercenary]
      resolve [VillainMessage vid $ VillainAttacks (player1 tg)]
      damageOf (player1 tg) `shouldReturn` 5
      getAttachedCharacter (attachmentIdOf strength) `shouldReturn` Nothing

  it "overkills an ally defender with Superhuman Strength" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57001"
      strength <- encounterCard C.cw57007
      resolve [RevealEncounterCard you strength]
      ally <- putIntoPlay you "01084"
      _ <- stackEncounterDeck [M.hydraMercenary]
      start [VillainMessage vid $ VillainAttacks you]
      choose "grant overkill" (isAbilityOf $ attachmentIdOf strength)
      choose "defend with Nick Fury" $ \case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False
      pass
      damageOf you `shouldReturn` 2
      getAttachedCharacter (attachmentIdOf strength) `shouldReturn` Nothing

  it "flips Vision's mass form after the threat step and preserves its attachment" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "57040"
      card <- encounterCard C.cw57046a
      resolve [RevealEncounterCard (player1 tg) card]
      getModifiers (VillainRef vid) >>= (`shouldSatisfy` elem (AttackModifier 1))
      resolve [VillainPhaseStep StepEnemiesActivate]
      let aid = attachmentIdOf card
      code <- getsGame $ fmap (toCardCode . getCardDef) . lookup aid . gameAttachments
      liftIO $ code `shouldBe` Just "57046b"
      getAttachedCharacter aid `shouldReturn` Just (VillainCharacter vid)
      getModifiers (VillainRef vid) >>= (`shouldSatisfy` elem (SchemeModifier 1))

  it "Intangible reduces each damage instance by one" do
    tg <- testGame ["spider-man"]
    inGame tg do
      vid <- withLeader "57040"
      card <- encounterCard C.cw57046b
      resolve [RevealEncounterCard (player1 tg) card]
      before <- leaderHp vid
      resolve [VillainMessage vid $ VillainDamaged GameRef (toDamage 3 FromAbility)]
      leaderHp vid `shouldReturn` (before - 2)
