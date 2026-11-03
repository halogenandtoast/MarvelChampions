module Marvel.CivilWar.FreedomFightersSpec (spec) where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.LeaderSpec (withLeader)
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.Damage
import Marvel.Keyword (Keyword(Stalwart,Steady))
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as M
import Marvel.Modifier
import Marvel.Query (selectList)
import TestImport hiding (ExhaustedIdentity)

putMinion :: IdentityId -> CardDef -> GameAppT (MinionId,EncounterCard)
putMinion you def = do
  card <- encounterCard def
  L.runQueueT (L.capture $ L.putEncounterCardIntoPlay you card) >>= resolve
  pure (MinionId $ unCardId $ ecCardId card,card)

spec :: Spec
spec = describe "Civil War Freedom Fighters" do
  for_ [C.cw56153,C.cw56157] $ \def -> do
    it (show (toCardCode def) <> " resolves a teammate's minion boost against the revealing player without moving the minion") do
      tg <- testGame ["spider-man","captain-marvel"]
      inGame tg do
        let you = player1 tg
            other = player2 tg
        (mid,original) <- putMinion other C.cw56150
        card <- encounterCard def
        resolve [RevealEncounterCard you card]
        getCharacterStatusCount (IdentityCharacter you) ConfusedStatus `shouldReturn` 1
        getCharacterStatusCount (IdentityCharacter other) ConfusedStatus `shouldReturn` 0
        getMinionEngagedPlayer mid `shouldReturn` Just other
        getEncounterDiscard >>= (`shouldSatisfy` notElem original)
        getsGame gameInPlayBoosts `shouldReturn` []
        getsGame gameBoostPlayerOverride `shouldReturn` Nothing

    it (show (toCardCode def) <> " surges when only minions without boost abilities are present") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        _ <- putMinion you M.hydraSoldier
        [next,_] <- stackEncounterDeck [M.hydraMercenary,M.shocker]
        card <- encounterCard def
        resolve [RevealEncounterCard you card]
        playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` [next]

    it (show (toCardCode def) <> " ignores a blanked minion's boost text and surges") do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        (mid,_) <- putMinion you C.cw56150
        L.runQueueT (L.capture $ L.lastingModifiers GameRef (SourceEntity $ MinionRef mid) [Blank] False) >>= resolve
        [next,_] <- stackEncounterDeck [M.hydraMercenary,M.shocker]
        card <- encounterCard def
        resolve [RevealEncounterCard you card]
        getCharacterStatusCount (IdentityCharacter you) ConfusedStatus `shouldReturn` 0
        playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` [next]

    it (show (toCardCode def) <> " as a boost resolves only minions engaged with its resolving player and does not borrow their icons") do
      tg <- testGame ["spider-man","captain-marvel"]
      inGame tg do
        let you = player1 tg
        vid <- withLeader "57040"
        _ <- putMinion you C.cw56150
        _ <- putMinion (player2 tg) M.hydraSoldier
        resolve [GiveStatus (IdentityRef you) ConfusedStatus]
        _ <- stackEncounterDeck [def,M.hydraMercenary]
        resolve [VillainMessage vid VillainSchemes]
        getCharacterStatusCount (IdentityCharacter you) ConfusedStatus `shouldReturn` 1
        mainSchemeThreat `shouldReturn` 1
        getsGame gameInPlayBoosts `shouldReturn` []

  it "resolves boost text instead of repeating a minion's When Revealed" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- putMinion you C.cw56079
      resolve [GiveStatus (IdentityRef you) StunnedStatus]
      card <- encounterCard C.cw56153
      resolve [RevealEncounterCard you card]
      damageOf you `shouldReturn` 0
      getCharacterStatusCount (IdentityCharacter you) StunnedStatus `shouldReturn` 1
      playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` []

  it "putting Weapons Runner into play again neither re-enters it nor changes its engagement" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
          other = player2 tg
      (mid,original) <- putMinion other M.weaponsRunner
      L.runQueueT (L.capture $ L.dealDamage mid you $ toDamage 1 FromAbility) >>= resolve
      card <- encounterCard C.cw56153
      resolve [RevealEncounterCard you card]
      minionDamageOf mid `shouldReturn` Just 1
      getMinionEngagedPlayer mid `shouldReturn` Just other
      getEncounterDiscard >>= (`shouldSatisfy` notElem original)
      playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` []

  it "shuffling Zola's Mutate as its boost removes the live host and discards its attachment" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      (mid,original) <- putMinion you M.zolasMutate
      attachment <- encounterCard C.cw56191
      resolve [RevealEncounterCard you attachment]
      _ <- stackEncounterDeck [M.hydraMercenary,M.shocker]
      card <- encounterCard C.cw56153
      resolve [RevealEncounterCard you card]
      minionDamageOf mid `shouldReturn` Nothing
      getEncounterDeck >>= (`shouldSatisfy` elem original)
      getEncounterDiscard >>= (`shouldSatisfy` elem attachment)
      getEncounterDiscard >>= (`shouldSatisfy` notElem original)
      getRemovedFromGameEncounterCards >>= (`shouldSatisfy` notElem original)

  it "Cloak and Dagger can reveal their existing physical cards without creating fresh copies" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (cloak,_) <- putMinion you C.cw56194
      (dagger,_) <- putMinion (player2 tg) C.cw56195
      card <- encounterCard C.cw56153
      resolve [RevealEncounterCard you card]
      minionDamageOf cloak `shouldReturn` Just 0
      minionDamageOf dagger `shouldReturn` Just 0
      length <$> selectList AnyMinion `shouldReturn` 2
      mainSchemeThreat `shouldReturn` 2
      damageOf you `shouldReturn` 2

  it "Frost Giant's reactive boost text lasts through the current attack's damage" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      _ <- putMinion you M.frostGiant
      changeForm you
      _ <- stackEncounterDeck [C.cw56153,M.hydraMercenary]
      resolve [VillainMessage vid $ VillainAttacks you]
      damageOf you `shouldReturn` 1
      getCharacterStatusCount (IdentityCharacter you) StunnedStatus `shouldReturn` 1
      getsGame gameInPlayBoosts `shouldReturn` []

  it "a boost resolved outside an activation does not leave a watcher for a future attack" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      _ <- putMinion you M.frostGiant
      card <- encounterCard C.cw56153
      resolve [RevealEncounterCard you card]
      getsGame gameInPlayBoosts `shouldReturn` []
      changeForm you
      _ <- stackEncounterDeck [M.hydraMercenary,M.shocker]
      resolve [VillainMessage vid $ VillainAttacks you]
      getCharacterStatusCount (IdentityCharacter you) StunnedStatus `shouldReturn` 0

  it "boost 'you' follows the revealing seat even when it is not the active player" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let active = player1 tg
          revealing = player2 tg
      _ <- putMinion active C.cw56150
      card <- encounterCard C.cw56153
      resolve [RevealEncounterCard revealing card]
      getCharacterStatusCount (IdentityCharacter revealing) ConfusedStatus `shouldReturn` 1
      getCharacterStatusCount (IdentityCharacter active) ConfusedStatus `shouldReturn` 0
      getActivePlayerId `shouldReturn` active

  it "does not resolve the other team's minions' boosts" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- addSecondBoard tg
      _ <- putMinion (player2 tg) C.cw56150
      [next,_] <- stackEncounterDeck [M.hydraMercenary,M.shocker]
      card <- encounterCard C.cw56153
      resolve [RevealEncounterCard you card]
      getCharacterStatusCount (IdentityCharacter you) ConfusedStatus `shouldReturn` 0
      playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` [next]

  it "an outstanding boost choice survives a Game JSON round trip and cleans its scope afterward" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- putMinion you C.cw56156
      ally <- putInHand you "01050"
      L.runQueueT (L.capture $ L.putIntoPlay you ally) >>= resolve
      card <- encounterCard C.cw56153
      start [RevealEncounterCard you card]
      saved <- getsGame toJSON
      restored <- case fromJSON saved of
        Success value -> pure value
        Error err -> error err
      toJSON (restored :: Game) `shouldBe` saved
      withGame_ $ const restored
      choose "exhaust Hulk" (isTargetChoice $ AllyCharacter $ allyIdOf ally)
      pass
      getsGame gameInPlayBoosts `shouldReturn` []

  it "older saved games default to no in-play boost scopes" do
    tg <- testGame ["spider-man"]
    inGame tg do
      saved <- getsGame toJSON
      let withoutScopes = case saved of
            Object fields -> Object $ KeyMap.delete "inPlayBoosts" $ KeyMap.delete "boostPlayerOverride" fields
            value -> value
      restored <- case Aeson.fromJSON withoutScopes of
        Success value -> pure value
        Error err -> error err
      gameInPlayBoosts (restored :: Game) `shouldBe` []
      gameBoostPlayerOverride restored `shouldBe` Nothing

  it "a live Defender's boost callback resolves once even though its ordinary text handles the same callback" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      friendly <- addSecondBoard tg
      _ <- putMinion you C.cw56184
      L.runQueueT (L.capture $ do
        L.lastingModifiers GameRef (SourceEntity $ VillainRef friendly) [KeywordModifier Stalwart] False
        L.lastingModifiers GameRef (SourceEntity $ IdentityRef you) [KeywordModifier Steady] False) >>= resolve
      card <- encounterCard C.cw56153
      resolve [RevealEncounterCard you card]
      getCharacterStatusCount (VillainCharacter friendly) StunnedStatus `shouldReturn` 0
      getCharacterStatusCount (IdentityCharacter you) StunnedStatus `shouldReturn` 1

  it "Goblin Knight's delayed shuffle leaves play once after the activation" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      (mid,original) <- putMinion you M.goblinKnight
      _ <- stackEncounterDeck [C.cw56153,M.hydraMercenary]
      resolve [VillainMessage vid VillainSchemes]
      minionDamageOf mid `shouldReturn` Nothing
      fmap (length . filter (== original)) getEncounterDeck `shouldReturn` 1
      getRemovedFromGameEncounterCards >>= (`shouldSatisfy` notElem original)
      getsGame gameInPlayBoosts `shouldReturn` []

  it "a minion's boost-only callback can schedule an attack" do
    tg <- testGameAgainst "07001" ["spider-man"]
    inGame tg do
      let you = player1 tg
      changeForm you
      _ <- putMinion you M.escapedConvict
      card <- encounterCard C.cw56153
      resolve [RevealEncounterCard you card]
      damageOf you >>= (`shouldSatisfy` (> 0))
      getsGame gameInPlayBoosts `shouldReturn` []

  it "two resolutions of Frost Giant's boost retain two independent reactive effects" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      _ <- putMinion you M.frostGiant
      changeForm you
      L.runQueueT (L.capture $ L.lastingModifiers GameRef (SourceEntity $ IdentityRef you) [KeywordModifier Steady] False) >>= resolve
      stored <- encounterCard C.cw56153
      L.runQueueT (L.capture $ L.giveCardAsBoost stored vid) >>= resolve
      _ <- stackEncounterDeck [C.cw56157,M.hydraMercenary]
      resolve [VillainMessage vid $ VillainAttacks you]
      getCharacterStatusCount (IdentityCharacter you) StunnedStatus `shouldReturn` 2
      getsGame gameInPlayBoosts `shouldReturn` []

  it "a boost referring to the activating enemy has no such target outside an activation" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- withLeader "57040"
      _ <- putMinion you M.agentOfApocalypse
      [next,_] <- stackEncounterDeck [M.hydraMercenary,M.shocker]
      card <- encounterCard C.cw56153
      resolve [RevealEncounterCard you card]
      headMay <$> getEncounterDeck `shouldReturn` Just next
      playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` []
      getsGame gameInPlayBoosts `shouldReturn` []

  it "an in-play boost can give the actual activating enemy another boost card" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      vid <- withLeader "57040"
      _ <- putMinion you M.agentOfApocalypse
      [_,_,next] <- stackEncounterDeck [C.cw56153,M.hydraMercenary,M.shocker]
      resolve [VillainMessage vid VillainSchemes]
      mainSchemeThreat `shouldReturn` 2
      headMay <$> getEncounterDeck `shouldReturn` Just next
      getsGame gameInPlayBoosts `shouldReturn` []
