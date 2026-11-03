module Marvel.CivilWar.ChoosingSidesSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Card
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.CivilWar.LeaderSpec (leaderHp)
import Marvel.Damage
import Marvel.Message.Lifted qualified as L
import Marvel.Matchers
import Marvel.Minion.Cards qualified as M
import Marvel.Modifier
import Marvel.Obligation.Types qualified as O
import TestImport

isCode :: CardCode -> Choice -> Bool
isCode code = \case
  CardLabel other _ -> other == code
  _ -> False

spec :: Spec
spec = describe "Civil War Choosing Sides" do
  for_ [C.cw56128a,C.cw56206a,C.cw57039a,C.cw57081a] $ \def ->
    it ("starts " <> show (toCardCode def) <> " at four threat per player") do
      tg <- testGame ["spider-man","captain-marvel"]
      inGame tg do
        card <- encounterCard def
        resolve [RevealEncounterCard (player1 tg) card]
        sideSchemeThreat (sideSchemeOf card) `shouldReturn` 8

  it "caps each attack on its own enemy leader while allowing nonattack damage" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      other <- addSecondBoard tg
      vid <- villainIdNow
      card <- encounterCard C.cw56128a
      resolve [RevealEncounterCard you card]
      before <- leaderHp vid
      otherBefore <- leaderHp other
      resolve [VillainMessage vid $ VillainDamaged (IdentityRef you) (toDamage 5 $ FromPlayerAttack you)]
      leaderHp vid `shouldReturn` (before - 2)
      resolve [VillainMessage vid $ VillainDamaged (IdentityRef you) (toDamage 3 FromAbility)]
      leaderHp vid `shouldReturn` (before - 5)
      resolve [VillainMessage other $ VillainDamaged (IdentityRef you) (toDamage 4 FromAttack)]
      leaderHp other `shouldReturn` (otherBefore - 4)

  it "keeps the permanent card in play after partial threat removal and external discard" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      card <- encounterCard C.cw56128a
      resolve [RevealEncounterCard you card]
      let sid = sideSchemeOf card
      resolve [SideSchemeMessage sid $ SideSchemeThreatRemoved (IdentityRef you) 2,RemoveFromPlay $ SideSchemeRef sid]
      sideSchemeThreat sid `shouldReturn` 2
      getsGame (HashMap.member sid . gameSideSchemes) `shouldReturn` True
      getsGame (HashMap.null . gameObligations) `shouldReturn` True

  for_ [(C.cw56128a,"56128b"),(C.cw56206a,"56206b"),(C.cw57039a,"57039b"),(C.cw57081a,"57081b")] $ \(def,back) ->
    it ("lets the opponent deal from the original top five and flips " <> show (toCardCode def) <> " without defeating it") do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let you = player1 tg
            opponent = player2 tg
        _ <- addSecondBoard tg
        [first,second,_,_,_,sixth] <- stackEncounterDeck [M.hydraMercenary,M.shocker,M.weaponsRunner,M.hydraSoldier,M.armoredGuard,M.whiplash]
        card <- encounterCard def
        resolve [RevealEncounterCard you card]
        let sid = sideSchemeOf card
            oid = ObligationId $ unSideSchemeId sid
        start [SideSchemeMessage sid $ SideSchemeThreatRemoved (IdentityRef you) 4]
        choose "resolve Choosing Sides" (isAbilityOf sid)
        fmap fst <$> openQuestion `shouldReturn` Just opponent
        offers (isCode $ toCardCode sixth) `shouldReturn` False
        saved <- getsGame toJSON
        restored <- case fromJSON saved of
          Success value -> pure value
          Error err -> error err
        toJSON (restored :: Game) `shouldBe` saved
        withGame_ $ const restored
        choose "choose Shocker from the searched cards" (isCode $ toCardCode second)
        pass
        playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` [second]
        getsGame (HashMap.member sid . gameSideSchemes) `shouldReturn` False
        state <- getsGame (fmap toAttrs . HashMap.lookup oid . gameObligations)
        toCardCode <$> state `shouldBe` Just back
        O.obligationOwner <$> state `shouldBe` Just (Just you)
        getEncounterDiscard >>= (`shouldSatisfy` notElem card)
        getEncounterDeck >>= (`shouldSatisfy` elem first)

  it "lets each teammate choose two physical cards from the shared reward pool" do
    tg <- competitive ["spider-man","captain-marvel","black-panther","she-hulk"]
    inGame tg do
      let you = player1 tg
          teammate = player2 tg
      _ <- addSecondBoard tg
      clearHand you
      clearHand teammate
      rewards <- traverse (putInHand you) ["01088","01089","01090","01055"]
      L.runQueueT (L.capture $ L.setAside $ map PlayerCard rewards) >>= resolve
      start [CardSpecific GameRef "cwLeaderRewards" $ toJSON (you,["01088","01089","01090","01055"] :: [CardCode],[you,teammate],2 :: Natural)]
      choose "first reward" (isCode "01088")
      offers (isCode "01088") `shouldReturn` False
      choose "second reward" (isCode "01090")
      fmap fst <$> openQuestion `shouldReturn` Just teammate
      choose "teammate first reward" (isCode "01089")
      choose "teammate second reward" (isCode "01055")
      pass
      map toCardCode <$> handOf you `shouldReturn` ["01090","01088"]
      map toCardCode <$> handOf teammate `shouldReturn` ["01055","01089"]
      for_ [you,teammate] $ \owner -> handOf owner >>= \cards -> do
        map pcOwner cards `shouldBe` replicate 2 (Just owner)
        map pcController cards `shouldBe` replicate 2 (Just owner)
      getSetAsidePlayerCards >>= (`shouldSatisfy` all ((`notElem` ["01088","01089","01090","01055"]) . toCardCode))

  it "cannot be blanked by an outside effect" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      card <- encounterCard C.cw56128a
      resolve [RevealEncounterCard you card]
      let sid = sideSchemeOf card
      L.runQueueT (L.capture $ L.lastingModifiers GameRef (SourceEntity $ SideSchemeRef sid) [Blank] False) >>= resolve
      isBlanked sid `shouldReturn` False
      getModifiers sid >>= (`shouldSatisfy` elem CannotLeavePlay)

  it "deals one card to each teammate from a single five-card search" do
    tg <- competitive ["spider-man","captain-marvel","black-panther","she-hulk"]
    inGame tg do
      let you = player1 tg
          teammate = player2 tg
          chooser = seats tg !! 2
      _ <- addSecondBoard tg
      [first,second,_,_,_,sixth] <- stackEncounterDeck [M.hydraMercenary,M.shocker,M.weaponsRunner,M.hydraSoldier,M.armoredGuard,M.whiplash]
      card <- encounterCard C.cw56128a
      resolve [RevealEncounterCard you card]
      sideSchemeThreat (sideSchemeOf card) `shouldReturn` 8
      start [SideSchemeMessage (sideSchemeOf card) $ SideSchemeThreatRemoved (IdentityRef you) 8]
      choose "resolve Choosing Sides" (isAbilityOf $ sideSchemeOf card)
      fmap fst <$> openQuestion `shouldReturn` Just chooser
      choose "deal the mercenary to the first teammate" (isCode $ toCardCode first)
      fmap fst <$> openQuestion `shouldReturn` Just chooser
      offers (isCode $ toCardCode first) `shouldReturn` False
      offers (isCode $ toCardCode sixth) `shouldReturn` False
      choose "deal Shocker to the other teammate" (isCode $ toCardCode second)
      pass
      playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` [first]
      playerIdentityEncounterCards <$> playerAttrs teammate `shouldReturn` [second]
      playerIdentityEncounterCards <$> playerAttrs chooser `shouldReturn` []

  it "flips even if the encounter deck has no cards to deal" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- addSecondBoard tg
      _ <- stackEncounterDeck []
      card <- encounterCard C.cw56128a
      resolve [RevealEncounterCard you card]
      resolve [SideSchemeMessage (sideSchemeOf card) $ SideSchemeThreatRemoved (IdentityRef you) 4]
      getsGame (HashMap.member (sideSchemeOf card) . gameSideSchemes) `shouldReturn` False
      getsGame (HashMap.member (ObligationId $ unCardId $ ecCardId card) . gameObligations) `shouldReturn` True

  for_ [C.cw56128b,C.cw56206b,C.cw57039b,C.cw57081b] $ \def ->
    it ("removes " <> show (toCardCode def) <> " as an action even with no rewards remaining") do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let you = player1 tg
        _ <- addSecondBoard tg
        card <- encounterCard def
        resolve [RevealEncounterCard you card]
        let oid = ObligationId $ unCardId $ ecCardId card
        getsGame (HashMap.member oid . gameObligations) `shouldReturn` True
        choose "use the team reward action" (isAbilityOf $ ObligationRef oid)
        pass
        getsGame (HashMap.member oid . gameObligations) `shouldReturn` False
        getEncounterDiscard >>= (`shouldSatisfy` notElem card)

  it "permits blanking from the same encounter set" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      card <- encounterCard C.cw56128a
      source <- encounterCard C.cw56128b
      resolve [RevealEncounterCard you card,RevealEncounterCard you source]
      let sid = sideSchemeOf card
          oid = ObligationId $ unCardId $ ecCardId source
      L.runQueueT (L.capture $ L.lastingModifiers (ObligationRef oid) (SourceEntity $ SideSchemeRef sid) [Blank] False) >>= resolve
      isBlanked sid `shouldReturn` True

  it "discards attached upgrades on the type-changing flip without triggering defeat responses" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- addSecondBoard tg
      card <- encounterCard C.cw56128a
      resolve [RevealEncounterCard you card]
      let sid = sideSchemeOf card
      followed <- putInHand you "03032"
      payment <- putInHand you "01088"
      playOn followed sid [payment]
      [chosen,_] <- stackEncounterDeck [M.hydraMercenary,M.shocker]
      start [SideSchemeMessage sid $ SideSchemeThreatRemoved (IdentityRef you) 4]
      choose "resolve Choosing Sides" (isAbilityOf sid)
      choose "deal the mercenary" (isCode $ toCardCode chosen)
      pass
      upgradesWith "03032" `shouldReturn` []
      discardOf you >>= (`shouldSatisfy` any ((== pcCardId followed) . pcCardId))
      villainDamageNow `shouldReturn` 0

  it "lets the first player choose which teammate selects from the shared pool first" do
    tg <- competitive ["spider-man","captain-marvel","black-panther","she-hulk"]
    inGame tg do
      let first = player1 tg
          teammate = player2 tg
      _ <- addSecondBoard tg
      clearHand first
      clearHand teammate
      rewards <- traverse (putInHand first) ["01088","01089","01090","01055"]
      L.runQueueT (L.capture $ L.setAside $ map PlayerCard rewards) >>= resolve
      start [CardSpecific GameRef "cwChooseRewardPlayer" $ toJSON (first,["01088","01089","01090","01055"] :: [CardCode],[first,teammate])]
      fmap fst <$> openQuestion `shouldReturn` Just first
      choose "teammate selects first" (isTargetChoice teammate)
      fmap fst <$> openQuestion `shouldReturn` Just teammate
      choose "teammate takes Energy" (isCode "01088")
      choose "teammate takes Strength" (isCode "01090")
      fmap fst <$> openQuestion `shouldReturn` Just first
      choose "first player takes Genius" (isCode "01089")
      choose "first player takes The Power of Aggression" (isCode "01055")
      pass
      map toCardCode <$> handOf teammate `shouldReturn` ["01090","01088"]
      map toCardCode <$> handOf first `shouldReturn` ["01055","01089"]
