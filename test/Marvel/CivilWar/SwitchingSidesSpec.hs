module Marvel.CivilWar.SwitchingSidesSpec (spec) where

import Marvel.Ally.Types qualified as A
import Marvel.CivilWar.Cards qualified as C
import Marvel.CivilWar.TeamSpec (competitive,addSecondBoard)
import Marvel.Damage
import Marvel.Matchers
import Marvel.Message.Lifted qualified as L
import Marvel.Minion.Cards qualified as M
import Marvel.Modifier
import TestImport

isClaim :: Choice -> Bool
isClaim = \case
  CardLabel _ choice -> isClaim choice
  choice -> isLabelStarting "$label.cwSwitchingSidesPay" choice || isLabel "$label.cwSwitchingSidesFree" choice

hasPhysical :: PlayerCard -> [PlayerCard] -> Bool
hasPhysical card = any ((== pcCardId card) . pcCardId)

spec :: Spec
spec = describe "Civil War Switching Sides" do
  it "discards a chosen controlled ally in cooperative play without surging" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      first <- putIntoPlay you "01050"
      kept <- putIntoPlay you "01002"
      teammate <- putIntoPlay (player2 tg) "01076"
      [next,_] <- stackEncounterDeck [M.hydraMercenary,M.shocker]
      card <- encounterCard C.cw56158
      start [RevealEncounterCard you card]
      offers (isTargetChoice $ allyIdOf teammate) `shouldReturn` False
      choose "discard Hulk" (isTargetChoice $ allyIdOf first)
      pass
      discardOf you >>= (`shouldSatisfy` hasPhysical first)
      getAllyControllerId (allyIdOf kept) `shouldReturn` Just you
      playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` []
      headMay <$> getEncounterDeck `shouldReturn` Just next

  it "surges when only another player's ally is in play" do
    tg <- testGame ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- putIntoPlay (player2 tg) "01050"
      [next,_] <- stackEncounterDeck [M.hydraMercenary,M.shocker]
      card <- encounterCard C.cw56158
      resolve [RevealEncounterCard you card]
      playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` [next]

  for_ [CannotBeDiscarded,CannotLeavePlay,CannotLeavePlayByCardAbilities] $ \protection ->
    it ("surges instead of offering an ally protected by " <> show protection) do
      tg <- testGame ["spider-man"]
      inGame tg do
        let you = player1 tg
        ally <- putIntoPlay you "01050"
        L.runQueueT (L.capture $ L.lastingModifiers GameRef (SourceEntity $ AllyRef $ allyIdOf ally) [protection] False) >>= resolve
        [next,_] <- stackEncounterDeck [M.hydraMercenary,M.shocker]
        card <- encounterCard C.cw56158
        resolve [RevealEncounterCard you card]
        getAllyControllerId (allyIdOf ally) `shouldReturn` Just you
        playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` [next]

  it "lets an opponent pay the printed cost, preserving owner and physical identity while entering fresh" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
          opponent = player2 tg
      _ <- addSecondBoard tg
      clearHand opponent
      energy <- putInHand opponent "01088"
      ally <- putIntoPlay you "01050"
      let aid = allyIdOf ally
      L.runQueueT (L.capture $ do
        L.damageCharacter aid you (toDamage 2 FromAbility)
        L.exhaust aid
        L.giveStatus aid StunnedStatus
        L.giveStatus aid ToughStatus) >>= resolve
      card <- encounterCard C.cw56158
      start [RevealEncounterCard you card]
      choose "discard Hulk" (isTargetChoice aid)
      fmap fst <$> openQuestion `shouldReturn` Just opponent
      choose "pay for Hulk" isClaim
      payWith [energy]
      pass
      getAllyControllerId aid `shouldReturn` Just opponent
      state <- getsGame (fmap toAttrs . lookup aid . gameAllies)
      A.allyOwner <$> state `shouldBe` Just you
      A.allyDamage <$> state `shouldBe` Just 0
      A.allyExhausted <$> state `shouldBe` Just False
      getCharacterStatusCount (AllyCharacter aid) StunnedStatus `shouldReturn` 0
      getCharacterStatusCount (AllyCharacter aid) ToughStatus `shouldReturn` 0
      discardOf you >>= (`shouldSatisfy` not . hasPhysical ally)
      discardOf opponent >>= (`shouldSatisfy` hasPhysical energy)
      opponentArea <- getAreaOfTarget opponent
      getAreaOfTarget aid `shouldReturn` opponentArea
      L.runQueueT (L.capture $ L.removeFromPlay aid) >>= resolve
      discardOf you >>= (`shouldSatisfy` hasPhysical ally)
      discardOf opponent >>= (`shouldSatisfy` not . hasPhysical ally)

  it "lets an opponent decline without surging or spending their resources" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
          opponent = player2 tg
      _ <- addSecondBoard tg
      clearHand opponent
      energy <- putInHand opponent "01088"
      ally <- putIntoPlay you "01050"
      card <- encounterCard C.cw56158
      start [RevealEncounterCard you card]
      choose "discard Hulk" (isTargetChoice $ allyIdOf ally)
      choose "pass on Hulk" (isLabel "$label.pass")
      pass
      getAllyControllerId (allyIdOf ally) `shouldReturn` Nothing
      discardOf you >>= (`shouldSatisfy` hasPhysical ally)
      handOf opponent >>= (`shouldSatisfy` hasPhysical energy)
      playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` []

  it "skips unaffordable opponents while still discarding the ally without surge" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
          opponent = player2 tg
      _ <- addSecondBoard tg
      clearHand opponent
      ally <- putIntoPlay you "01050"
      card <- encounterCard C.cw56158
      resolve [RevealEncounterCard you card]
      getAllyControllerId (allyIdOf ally) `shouldReturn` Nothing
      discardOf you >>= (`shouldSatisfy` hasPhysical ally)
      playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` []

  it "does not offer payment when the opponent already controls that unique ally" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
          opponent = player2 tg
      _ <- addSecondBoard tg
      clearHand opponent
      energy <- putInHand opponent "01088"
      original <- putIntoPlay you "01050"
      own <- putIntoPlay opponent "01050"
      card <- encounterCard C.cw56158
      resolve [RevealEncounterCard you card]
      discardOf you >>= (`shouldSatisfy` hasPhysical original)
      getAllyControllerId (allyIdOf own) `shouldReturn` Just opponent
      handOf opponent >>= (`shouldSatisfy` hasPhysical energy)

  it "offers a zero-cost ally without requiring resource cards" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
          opponent = player2 tg
      _ <- addSecondBoard tg
      clearHand opponent
      ally <- putIntoPlay you "18011"
      card <- encounterCard C.cw56158
      start [RevealEncounterCard you card]
      choose "discard Angela" (isTargetChoice $ allyIdOf ally)
      choose "claim Angela for free" isClaim
      pass
      getAllyControllerId (allyIdOf ally) `shouldReturn` Just opponent
      discardOf you >>= (`shouldSatisfy` not . hasPhysical ally)

  it "offers each eligible opposing teammate their own decision until someone pays" do
    tg <- competitive ["spider-man","captain-marvel","black-panther","she-hulk"]
    inGame tg do
      let you = player1 tg
          first = seats tg !! 2
          second = seats tg !! 3
      _ <- addSecondBoard tg
      clearHand first
      clearHand second
      _ <- putInHand first "01088"
      payment <- putInHand second "01088"
      ally <- putIntoPlay you "01050"
      card <- encounterCard C.cw56158
      start [RevealEncounterCard you card]
      choose "discard Hulk" (isTargetChoice $ allyIdOf ally)
      fmap fst <$> openQuestion `shouldReturn` Just first
      choose "first teammate passes" (isLabel "$label.pass")
      fmap fst <$> openQuestion `shouldReturn` Just second
      choose "second teammate claims Hulk" isClaim
      payWith [payment]
      pass
      getAllyControllerId (allyIdOf ally) `shouldReturn` Just second

  it "discards a controlled encounter minion as an ally without offering its nonexistent cost" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
      _ <- addSecondBoard tg
      mid <- addMinion you M.hydraMercenary
      L.runQueueT (L.capture $ L.takeControlOfMinionWithConsequential you mid 0) >>= resolve
      let aid = AllyId $ unMinionId mid
      card <- encounterCard C.cw56158
      start [RevealEncounterCard you card]
      choose "discard the controlled mercenary" (isTargetChoice aid)
      pass
      getAllyControllerId aid `shouldReturn` Nothing
      encounterDiscardCodes >>= (`shouldSatisfy` elem (toCardCode M.hydraMercenary))
      playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` []

  it "does not double conditional aspect resources when spending an amount equal to the ally cost" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
          opponent = player2 tg
      _ <- addSecondBoard tg
      clearHand opponent
      resource <- putInHand opponent "01055"
      ally <- putIntoPlay you "01050"
      card <- encounterCard C.cw56158
      resolve [RevealEncounterCard you card]
      discardOf you >>= (`shouldSatisfy` hasPhysical ally)
      handOf opponent >>= (`shouldSatisfy` hasPhysical resource)
      getAllyControllerId (allyIdOf ally) `shouldReturn` Nothing

  it "discards attachments before the ally enters under the opponent's control" do
    tg <- competitive ["spider-man","captain-marvel"]
    inGame tg do
      let you = player1 tg
          opponent = player2 tg
      _ <- addSecondBoard tg
      clearHand opponent
      payment <- putInHand opponent "01088"
      ally <- putIntoPlay you "01050"
      inspired <- putInHand you "01074"
      resource <- putInHand you "01089"
      playOn inspired (allyIdOf ally) [resource]
      card <- encounterCard C.cw56158
      start [RevealEncounterCard you card]
      choose "discard the upgraded Hulk" (isTargetChoice $ allyIdOf ally)
      choose "claim Hulk" isClaim
      payWith [payment]
      pass
      getAllyControllerId (allyIdOf ally) `shouldReturn` Just opponent
      upgradesWith "01074" `shouldReturn` []
      discardOf you >>= (`shouldSatisfy` hasPhysical inspired)
