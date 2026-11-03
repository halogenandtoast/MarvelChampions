module Marvel.CivilWar.RewardEventSpec (spec) where

import Marvel.Area (DeckKey (..),Pile (..))
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.CivilWar.LeaderSpec (withLeader,leaderHp,attachmentIdOf)
import Marvel.CivilWar.Cards qualified as C
import Marvel.Message.Lifted qualified as L
import Marvel.Matchers (EntityMatcher (SourceEntity))
import Marvel.Modifier (Modifier (CannotBeDiscarded))
import Marvel.Minion.Cards qualified as M
import Marvel.MainScheme.Types qualified as Main
import Marvel.Villain.Types qualified as V
import Marvel.Window qualified as W
import TestImport

leaders :: TestGame -> CardCode -> CardCode -> GameAppT (VillainId,VillainId)
leaders tg ownCode enemyCode = do
  enemy <- withLeader enemyCode
  own <- addSecondBoard tg
  resolve [ReplaceVillain own ownCode Swapped,VillainMessage own SetVillainHp,SetActivePlayer $ player1 tg]
  let (_,_,_,stage) = C.printedLeaderStats ownCode
  withGame_ $ entitiesL . villainsL . ix own %~ (\(V.Villain leader) -> V.Villain $ V.overVillainAttrs (\a -> a {V.villainStage = stage}) leader)
  pure (own,enemy)

threatOn :: MainSchemeId -> GameAppT Natural
threatOn sid = getsGame $ maybe 0 (Main.mainSchemeThreat . toAttrs) . lookup sid . gameMainSchemes

spec :: Spec
spec = describe "Civil War leader reward events" do
  it "The Futurist lets the friendly Iron Man scheme without drawing a boost card" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,_) <- leaders tg "56059" "56137"
      [next,_] <- stackEncounterDeck [M.hydraMercenary,M.shocker]
      [scheme] <- getMainSchemesIn =<< getAreaOfTarget own
      before <- threatOn scheme
      card <- putInHand you "56129"
      payment <- putInHand you "01089"
      playWith card [payment]
      pass
      threatOn scheme `shouldReturn` (before + 1)
      headMay <$> getEncounterDeck `shouldReturn` Just next
      discardOf you >>= (`shouldSatisfy` any ((== pcCardId card) . pcCardId))

  it "does not offer The Futurist with the wrong friendly leader or only an enemy Iron Man" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- leaders tg "56137" "56059"
      card <- putInHand you "56129"
      _ <- putInHand you "01089"
      offers (isPlay card) `shouldReturn` False

  for_ [("56134","56092","56137","01088",ConfusedStatus)
       ,("56208","56137","56059","01090",StunnedStatus)
       ,("56212","56168","56059","01089",ConfusedStatus)] $ \(code,ownCode,enemyCode,resource,status) ->
    it (show code <> " makes the friendly leader attack and applies its paid-resource bonus") do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let you = player1 tg
        (own,enemy) <- leaders tg ownCode enemyCode
        _ <- stackEncounterDeck [M.hydraMercenary,M.shocker]
        before <- leaderHp enemy
        card <- putInHand you code
        payment <- putInHand you resource
        playWith card [payment]
        whenM (offers $ isAbilityOf own) $ choose "resolve the friendly leader interrupt" (isAbilityOf own)
        pass
        leaderHp enemy >>= (`shouldSatisfy` (< before))
        getCharacterStatusCount (VillainCharacter enemy) status `shouldReturn` 1
        damageOf you `shouldReturn` 0

  it "Double Agent removes threat locally and places threat on the friendly leader's scheme" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,_) <- leaders tg "56168" "56059"
      [local] <- getMainSchemesIn =<< getAreaOfTarget you
      [other] <- getMainSchemesIn =<< getAreaOfTarget own
      L.runQueueT (L.capture $ L.placeThreatOn GameRef (SchemeMainSchemeId local) 3) >>= resolve
      before <- threatOn other
      card <- putInHand you "56211"
      payment <- putInHand you "01089"
      playWith card [payment]
      pass
      threatOn local `shouldReturn` 1
      threatOn other `shouldReturn` (before + 2)

  it "Recruited by Cap moves a chosen engaged minion to an opponent's board" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
          opponent = player2 tg
      _ <- leaders tg "56137" "56059"
      mid <- addMinion you M.hydraMercenary
      card <- putInHand you "56209"
      payment <- traverse (putInHand you) ["01088","01089"]
      playWith card payment
      choose "recruit the mercenary" (isTargetChoice mid)
      choose "send it to the opponent" (isTargetChoice opponent)
      pass
      getsGame (fmap (minionEngagedIdentity . toAttrs) . lookup mid . gameMinions) `shouldReturn` Just opponent
      otherArea <- getAreaOfTarget opponent
      getAreaOfTarget mid `shouldReturn` otherArea

  it "Target Lock discards a Hero Response attachment after its leader attack, even in alter-ego form" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (_,enemy) <- leaders tg "56059" "56137"
      helmet <- encounterCard C.cw56144
      shield <- encounterCard C.cw56143
      resolve [RevealEncounterCard you helmet,RevealEncounterCard you shield]
      _ <- stackEncounterDeck [M.hydraMercenary,M.shocker]
      card <- putInHand you "56130"
      payment <- putInHand you "01089"
      playWith card [payment]
      offers (isTargetChoice $ attachmentIdOf shield) `shouldReturn` False
      choose "discard Cap's Helmet" (isTargetChoice $ attachmentIdOf helmet)
      pass
      getAttachedCharacter (attachmentIdOf helmet) `shouldReturn` Nothing
      getCharacterStatusCount (VillainCharacter enemy) ToughStatus `shouldReturn` 0

  for_ [("56134","56092","56137","01089",ConfusedStatus)
       ,("56208","56137","56059","01088",StunnedStatus)
       ,("56212","56168","56059","01088",ConfusedStatus)] $ \(code,ownCode,enemyCode,resource,status) ->
    it (show code <> " still attacks without its resource bonus when the wrong resource was paid") do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let you = player1 tg
        (own,enemy) <- leaders tg ownCode enemyCode
        _ <- stackEncounterDeck [M.hydraMercenary,M.shocker]
        before <- leaderHp enemy
        card <- putInHand you code
        payment <- putInHand you resource
        playWith card [payment]
        whenM (offers $ isAbilityOf own) $ choose "resolve the friendly leader interrupt" (isAbilityOf own)
        pass
        leaderHp enemy >>= (`shouldSatisfy` (< before))
        getCharacterStatusCount (VillainCharacter enemy) status `shouldReturn` 0

  it "waits for an opposing player's boost decision before applying the resource bonus" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
          opponent = player2 tg
      (_,enemy) <- leaders tg "56092" "56137"
      ally <- putIntoPlay opponent "01050"
      _ <- stackEncounterDeck [C.cw56156,M.hydraMercenary]
      card <- putInHand you "56134"
      payment <- putInHand you "01088"
      playWith card [payment]
      fmap fst <$> openQuestion `shouldReturn` Just opponent
      getCharacterStatusCount (VillainCharacter enemy) ConfusedStatus `shouldReturn` 0
      choose "opponent resolves Spectrum by exhausting Hulk" (isTargetChoice $ AllyCharacter $ allyIdOf ally)
      pass
      getCharacterStatusCount (VillainCharacter enemy) ConfusedStatus `shouldReturn` 1

  it "still places Double Agent's threat when the local scheme has no threat to remove" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,_) <- leaders tg "56168" "56059"
      [local] <- getMainSchemesIn =<< getAreaOfTarget you
      [other] <- getMainSchemesIn =<< getAreaOfTarget own
      before <- threatOn other
      card <- putInHand you "56211"
      payment <- putInHand you "01089"
      playWith card [payment]
      pass
      threatOn local `shouldReturn` 0
      threatOn other `shouldReturn` (before + 2)

  it "does not offer leader rewards in cooperative play" do
    tg <- testGame ["spider-man"]
    inGame tg do
      let you = player1 tg
      _ <- withLeader "56059"
      for_ ["56129","56130","56134","56208","56209","56211","56212"] $ \code -> do
        card <- putInHand you code
        _ <- putInHand you "01089"
        offers (isPlay card) `shouldReturn` False

  it "does not spend the acting player's stun because the leader, rather than that player, attacks" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (_,enemy) <- leaders tg "56092" "56137"
      resolve [GiveStatus (IdentityRef you) StunnedStatus]
      _ <- stackEncounterDeck [M.hydraMercenary,M.shocker]
      before <- leaderHp enemy
      card <- putInHand you "56134"
      payment <- putInHand you "01088"
      playWith card [payment]
      pass
      leaderHp enemy >>= (`shouldSatisfy` (< before))
      getCharacterStatusCount (IdentityCharacter you) StunnedStatus `shouldReturn` 1

  it "Recruited by Cap preserves a minion's damage and status and later discards it to its original encounter deck" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
          opponent = player2 tg
      (own,enemy) <- leaders tg "56137" "56059"
      minion <- encounterCard M.hydraMercenary
      filler <- encounterCard M.shocker
      otherFiller <- encounterCard M.weaponsRunner
      let original = VillainDeckKey enemy
          destination = VillainDeckKey own
          mid = MinionId $ unCardId $ ecCardId minion
      resolve [CreateDeck original [minion,filler],CreateDeck destination [otherFiller],RevealEncounterCard you minion
        ,MinionMessage mid $ MinionDamaged GameRef $ toDamage 1 FromAbility,GiveStatus (MinionRef mid) ToughStatus]
      card <- putInHand you "56209"
      payment <- traverse (putInHand you) ["01088","01089"]
      playWith card payment
      choose "recruit the damaged mercenary" (isTargetChoice mid)
      choose "send it to the opponent" (isTargetChoice opponent)
      pass
      minionDamageOf mid `shouldReturn` Just 1
      getCharacterStatusCount (MinionCharacter mid) ToughStatus `shouldReturn` 1
      resolve [MinionMessage mid MinionDefeated]
      pileDiscard <$> getPile original >>= (`shouldSatisfy` elem minion)
      pileDiscard <$> getPile destination >>= (`shouldSatisfy` notElem minion)

  it "Now It's Personal gives the player actual native leader events from their own leader's pool" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- leaders tg "56059" "56137"
      clearHand you
      rewards <- traverse (putInHand you) ["56129","56130","56208"]
      L.runQueueT (L.capture $ L.setAside $ map (PlayerCard . (\card -> card {pcOwner = Nothing,pcController = Nothing})) rewards) >>= resolve
      personal <- encounterCard C.cw56128b
      resolve [RevealEncounterCard you personal]
      let oid = ObligationId $ unCardId $ ecCardId personal
      choose "use Now It's Personal" (isAbilityOf $ ObligationRef oid)
      getsGame (isNothing . lookup oid . gameObligations) `shouldReturn` True
      offers (\case CardLabel code _ -> code == "56208"; _ -> False) `shouldReturn` False
      choose "take The Futurist" (\case CardLabel code _ -> code == "56129"; _ -> False)
      choose "take Target Lock" (\case CardLabel code _ -> code == "56130"; _ -> False)
      pass
      map toCardCode <$> handOf you `shouldReturn` ["56130","56129"]
      handOf you >>= \cards -> map pcOwner cards `shouldBe` replicate 2 (Just you)
      map toCardCode <$> getSetAsidePlayerCards >>= (`shouldSatisfy` elem "56208")

  it "Target Lock does not offer an attachment that cannot be discarded" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (_,enemy) <- leaders tg "56059" "56137"
      helmet <- encounterCard C.cw56144
      resolve [RevealEncounterCard you helmet]
      let aid = attachmentIdOf helmet
      L.runQueueT (L.capture $ L.lastingModifiers GameRef (SourceEntity $ AttachmentRef aid) [CannotBeDiscarded] False) >>= resolve
      _ <- stackEncounterDeck [M.hydraMercenary,M.shocker]
      card <- putInHand you "56130"
      payment <- putInHand you "01089"
      playWith card [payment]
      offers (isTargetChoice aid) `shouldReturn` False
      pass
      getAttachedCharacter aid `shouldReturn` Just (VillainCharacter enemy)

  for_ [("01090",True),("01088",False)] $ \(resource,physical) ->
    it ("She-Hulk Smash attacks and checks the resource actually spent: physical " <> show physical) do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let you = player1 tg
        (own,enemy) <- leaders tg "57001" "56137"
        _ <- stackEncounterDeck [M.hydraMercenary,M.hydraSoldier]
        before <- leaderHp enemy
        card <- putInHand you "57033"
        payment <- putInHand you resource
        playWith card [payment]
        whenM (offers $ isAbilityOf own) $ choose "resolve She-Hulk's interrupt" (isAbilityOf own)
        pass
        leaderHp enemy >>= (`shouldSatisfy` (< before))
        getCharacterStatusCount (VillainCharacter enemy) StunnedStatus `shouldReturn` if physical then 1 else 0
        damageOf you `shouldReturn` 0

  it "She-Hulk Smash is not playable with a different friendly leader" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- leaders tg "56137" "57001"
      card <- putInHand you "57033"
      _ <- putInHand you "01090"
      offers (isPlay card) `shouldReturn` False

  for_ [W.ThreatFromAbility,W.ThreatFromVillain,W.ThreatFromAcceleration] $ \source ->
    for_ [2,5] $ \amount ->
      it ("You're Out of Order prevents up to three main-scheme threat: " <> show (source,amount)) do
        tg <- testGame ["spider-man"]
        inGame tg do
          let you = player1 tg
          card <- putInHand you "57032"
          payment <- putInHand you "01088"
          scheme <- mainSchemeIdNow
          before <- mainSchemeThreat
          messages <- L.runQueueT $ L.capture $ L.placeThreatFrom source (SchemeMainSchemeId scheme) amount
          start messages
          playWith card [payment]
          pass
          mainSchemeThreat `shouldReturn` (before + subtractNatural 3 amount)
          discardOf you >>= (`shouldSatisfy` elem card)

  for_ [2,6] $ \size ->
    it ("Calculated Risk searches only the friendly leader's top five and deals to the enemy first player: size " <> show size) do
      tg <- competitive ["spider-man","captain-marvel"]
      inGame tg do
        let you = player1 tg
            opponent = player2 tg
        (own,enemy) <- leaders tg "57040" "56137"
        cards <- traverse encounterCard $ take size [M.hydraMercenary,M.hydraSoldier,M.shocker,M.weaponsRunner,C.cw57013,C.cw57014]
        filler <- encounterCard M.hydraMercenary
        resolve [CreateDeck (VillainDeckKey own) cards,CreateDeck (VillainDeckKey enemy) [filler]]
        ordered <- pileDeck <$> getPile (VillainDeckKey own)
        card <- putInHand you "57074"
        payment <- putInHand you "01088"
        playWith card [payment]
        fmap fst <$> openQuestion `shouldReturn` Just you
        for_ (take 5 ordered) $ \visible ->
          offers (\case CardLabel code _ -> code == toCardCode visible; _ -> False) `shouldReturn` True
        for_ (drop 5 ordered) $ \hidden ->
          offers (\case CardLabel code _ -> code == toCardCode hidden; _ -> False) `shouldReturn` False
        let selected = ordered !! min 4 (size - 1)
        choose "select a searched card" (\case CardLabel code _ -> code == toCardCode selected; _ -> False)
        pass
        dealtTo opponent `shouldReturn` [selected]
        dealtTo you `shouldReturn` []
        pileDeck <$> getPile (VillainDeckKey enemy) `shouldReturn` [filler]
        remaining <- pileDeck <$> getPile (VillainDeckKey own)
        length remaining `shouldBe` (size - 1)
        remaining `shouldSatisfy` notElem selected

  for_ [(C.cw57046a,True),(C.cw57046b,False)] $ \(formDef,dense) ->
    for_ [False,True] $ \tough ->
      it ("Synthezoid Strike uses the starting mass form for its bonuses: dense " <> show dense <> ", tough " <> show tough) do
        tg <- competitive ["spider-man","captain-marvel"]
        inGame tg do
          let you = player1 tg
          (own,enemy) <- leaders tg "57040" "56059"
          form <- encounterCard formDef
          resolve [RevealEncounterCard (player2 tg) form]
          when tough $ resolve [GiveStatus (VillainRef enemy) ToughStatus]
          ownBoost <- encounterCard C.cw57036
          filler <- encounterCard C.cw57036
          resolve [CreateDeck (VillainDeckKey own) [ownBoost,filler]]
          before <- leaderHp enemy
          card <- putInHand you "57075"
          payment <- putInHand you "01088"
          playWith card [payment]
          pass
          leaderHp enemy `shouldReturn` (before - if tough && not dense then 0 else if dense then 3 else 2)
          getCharacterStatusCount (VillainCharacter enemy) ToughStatus `shouldReturn` 0
          getsGame (fmap (toCardCode . getCardDef) . lookup (attachmentIdOf form) . gameAttachments) `shouldReturn` Just "57046a"
          getAttachedCharacter (attachmentIdOf form) `shouldReturn` Just (VillainCharacter own)

  it "Synthezoid Strike's bonus and piercing do not remain for Vision's next attack" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,enemy) <- leaders tg "57040" "56059"
      form <- encounterCard C.cw57046a
      resolve [RevealEncounterCard (player2 tg) form]
      boosts <- traverse encounterCard $ replicate 4 C.cw57036
      resolve [CreateDeck (VillainDeckKey own) boosts]
      card <- putInHand you "57075"
      payment <- putInHand you "01088"
      playWith card [payment]
      pass
      before <- leaderHp enemy
      resolve [GiveStatus (VillainRef enemy) ToughStatus]
      L.runQueueT (L.capture $ L.villainAttacksCharacter own (player2 tg) (VillainCharacter enemy)) >>= resolve
      leaderHp enemy `shouldReturn` before
      getCharacterStatusCount (VillainCharacter enemy) ToughStatus `shouldReturn` 0
      L.runQueueT (L.capture $ L.villainAttacksCharacter own (player2 tg) (VillainCharacter enemy)) >>= resolve
      leaderHp enemy `shouldReturn` (before - 2)

  it "Synthezoid Strike does not leave its bonus behind when stun replaces its attack" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      (own,enemy) <- leaders tg "57040" "56059"
      form <- encounterCard C.cw57046a
      resolve [RevealEncounterCard (player2 tg) form,GiveStatus (VillainRef own) StunnedStatus]
      boosts <- traverse encounterCard $ replicate 3 C.cw57036
      resolve [CreateDeck (VillainDeckKey own) boosts]
      before <- leaderHp enemy
      card <- putInHand you "57075"
      payment <- putInHand you "01088"
      playWith card [payment]
      pass
      leaderHp enemy `shouldReturn` before
      getCharacterStatusCount (VillainCharacter own) StunnedStatus `shouldReturn` 0
      L.runQueueT (L.capture $ L.villainAttacksCharacter own (player2 tg) (VillainCharacter enemy)) >>= resolve
      leaderHp enemy `shouldReturn` (before - 2)
