module Marvel.CivilWar.CapeKillerSpec (spec) where

import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as M
import Marvel.Modifier
import Marvel.Trait qualified as T
import TestImport

spec :: Spec
spec = describe "Civil War Cape-Killer" do
  for_ [False,True] $ \unregistered ->
    it ("Cape-Killer's Quickstrike gains its attack bonus only against an Unregistered identity: " <> show unregistered) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        when unregistered $ do
          obligation <- encounterCard C.cw56106
          resolve [RevealEncounterCard you obligation]
        changeForm you
        _ <- addMinion you C.cw56105
        damageOf you `shouldReturn` if unregistered then 4 else 2

  it "Cape-Killer's bonus expires after the attack" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      obligation <- encounterCard C.cw56106
      resolve [RevealEncounterCard you obligation]
      mid <- addMinion you C.cw56105
      changeForm you
      resolve [MinionMessage mid $ MinionAttacks you]
      damageOf you `shouldReturn` 4
      resolve [RemoveFromPlay $ ObligationRef $ ObligationId $ unCardId $ ecCardId obligation]
      resolve [MinionMessage mid $ MinionAttacks you]
      damageOf you `shouldReturn` 6

  it "Bring Them In exhausts every ready Unregistered identity on the board" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      for_ [(p,C.cw56106),(q,C.cw56114)] $ \(you,def) -> do
        obligation <- encounterCard def
        resolve [RevealEncounterCard you obligation]
      [next] <- stackEncounterDeck [C.cw56106]
      card <- encounterCard C.cw56107
      resolve [RevealEncounterCard p card]
      for_ [p,q] $ \you -> playerIdentityExhausted <$> playerAttrs you `shouldReturn` True
      playerIdentityEncounterCards <$> playerAttrs p `shouldReturn` []
      headMay <$> getEncounterDeck `shouldReturn` Just next

  it "Bring Them In searches and deals a matching card without revealing it" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      (deck,_) <- setEncounterDeck [M.hydraMercenary,C.cw56106,M.hydraSoldier] []
      card <- encounterCard C.cw56107
      resolve [RevealEncounterCard you card]
      playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` [deck !! 1]
      identityMatches (IdentityWithTrait T.Unregistered) you `shouldReturn` False
      map toCardCode <$> getEncounterDeck >>= (`shouldMatchList` map toCardCode [testHead deck,last deck])

  it "Bring Them In searches the discard if every Unregistered identity is already exhausted" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      obligation <- encounterCard C.cw56106
      resolve [RevealEncounterCard you obligation]
      exhausted <- L.runQueueT $ L.capture $ L.exhaust you
      resolve exhausted
      (_,discard) <- setEncounterDeck [M.hydraMercenary,M.hydraSoldier] [C.cw56114]
      card <- encounterCard C.cw56107
      resolve [RevealEncounterCard you card]
      playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` discard
      map toCardCode <$> getEncounterDiscard `shouldReturn` ["56107"]

  it "Bring Them In does not search after partially exhausting its targets" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      for_ [p,q] $ \you -> do
        obligation <- encounterCard C.cw56106
        resolve [RevealEncounterCard you obligation]
      exhausted <- L.runQueueT $ L.capture $ L.exhaust p
      resolve exhausted
      _ <- setEncounterDeck [C.cw56114,M.hydraMercenary] []
      card <- encounterCard C.cw56107
      resolve [RevealEncounterCard p card]
      for_ [p,q] $ \you -> do
        playerIdentityExhausted <$> playerAttrs you `shouldReturn` True
        playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` []

  it "the opposing team chooses the Unregistered Super on the receiving team's board" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      own <- villainIdNow
      other <- addSecondBoard tg
      super <- encounterCard C.cw56106
      foreignCard <- encounterCard C.cw56114
      filler <- encounterCard M.hydraMercenary
      setup <- L.runQueueT $ L.capture $ do
        L.createDeck (L.VillainDeckKey own) [super,filler]
        L.createDeck (L.VillainDeckKey other) [foreignCard]
      resolve setup
      card <- encounterCard C.cw56107
      start [RevealEncounterCard p card]
      fmap fst <$> asked `shouldReturn` Just q
      choose "opponent chooses Unregistered Super" (const True)
      pass
      playerIdentityEncounterCards <$> playerAttrs p `shouldReturn` [super]
      asYou q getEncounterDeck `shouldReturn` [foreignCard]

  it "Bring Them In does not exhaust Unregistered opponents" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      _ <- addSecondBoard tg
      obligation <- encounterCard C.cw56106
      resolve [RevealEncounterCard q obligation]
      _ <- setEncounterDeck [M.hydraMercenary,M.hydraSoldier] []
      card <- encounterCard C.cw56107
      resolve [RevealEncounterCard p card]
      playerIdentityExhausted <$> playerAttrs q `shouldReturn` False

  it "Arrest Order deals the topmost S.H.I.E.L.D. minion to the defeating player facedown" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let p = player1 tg
          q = player2 tg
      (_,discard) <- setEncounterDeck [M.hydraMercenary,M.hydraSoldier] [M.hydraSoldier,C.cw56109,C.cw56083]
      side <- encounterCard C.cw56108
      resolve [RevealEncounterCard p side]
      let sid = SideSchemeId $ unCardId $ ecCardId side
      resolve [SideSchemeMessage sid $ SideSchemeThwarted (IdentityRef q) 3]
      playerIdentityEncounterCards <$> playerAttrs q `shouldReturn` [discard !! 1]
      playerIdentityEncounterCards <$> playerAttrs p `shouldReturn` []
      map toCardCode <$> getEncounterDiscard `shouldReturn` ["56108",toCardCode M.hydraSoldier,"56083"]

  it "Arrest Order has no extra effect without a matching discarded minion" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- setEncounterDeck [M.hydraMercenary] [M.hydraSoldier]
      side <- encounterCard C.cw56108
      resolve [RevealEncounterCard you side]
      let sid = SideSchemeId $ unCardId $ ecCardId side
      resolve [SideSchemeMessage sid $ SideSchemeThwarted (IdentityRef you) 3]
      playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` []


  it "Cape-Killer does not gain its identity attack bonus when directly attacking an ally" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      obligation <- encounterCard C.cw56106
      resolve [RevealEncounterCard you obligation]
      mid <- addMinion you C.cw56105
      cat <- putInHand you "01002"
      let aid = allyIdOf cat
      setup <- L.runQueueT $ L.capture $ do
        L.putIntoPlay you cat
        L.lastingModifiers GameRef (SourceEntity $ AllyRef aid) [HitPointModifier 3] False
      resolve setup
      resolve [MinionMessage mid $ MinionAttacksAlly you aid]
      allyDamage <$> allyIn "01002" `shouldReturn` 2
      damageOf you `shouldReturn` 0

  it "a stunned Cape-Killer does not start an attack or apply its bonus" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      obligation <- encounterCard C.cw56106
      resolve [RevealEncounterCard you obligation]
      mid <- addMinion you C.cw56105
      changeForm you
      resolve [GiveStatus (MinionRef mid) StunnedStatus,MinionMessage mid $ MinionAttacks you]
      damageOf you `shouldReturn` 0
      resolve [RemoveFromPlay $ ObligationRef $ ObligationId $ unCardId $ ecCardId obligation]
      resolve [MinionMessage mid $ MinionAttacks you]
      damageOf you `shouldReturn` 2

  it "an ally defending later does not undo Cape-Killer's attack bonus" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      obligation <- encounterCard C.cw56106
      resolve [RevealEncounterCard you obligation]
      mid <- addMinion you C.cw56105
      cat <- putInHand you "01002"
      let aid = allyIdOf cat
      setup <- L.runQueueT $ L.capture $ do
        L.putIntoPlay you cat
        L.lastingModifiers GameRef (SourceEntity $ AllyRef aid) [HitPointModifier 3] False
      resolve setup
      changeForm you
      start [MinionMessage mid $ MinionAttacks you]
      whenM (offers $ isAbilityOf mid) $ choose "resolve Cape-Killer" (isAbilityOf mid)
      let decline = whenM (offers isNoResponse) $ choose "decline a response" isNoResponse >> decline
      decline
      choose "Black Cat defends" (\case AllyDefend target _ -> target == aid; _ -> False)
      pass
      allyDamage <$> allyIn "01002" `shouldReturn` 4
      damageOf you `shouldReturn` 0
