module Marvel.Encounter.MysterioSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Choice (choiceMessages)
import Marvel.Attachment.Cards qualified as Attachments
import Marvel.EncounterSet (EncounterSet (WhispersOfParanoia))
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

mysterio :: [Text] -> IO TestGame
mysterio = testGameAgainst "27084"

expertMysterio :: IO TestGame
expertMysterio = do
  (app, ran, logged) <- dealtGameLogged (ScenarioSetup "27084" [WhispersOfParanoia] Expert) [starter "spider-man-miles-morales"] 1
  seats <- runGameApp app (getsGame gameSeats)
  let tg = TestGame app ran logged seats
  inGame tg toFirstTurn
  pure tg

playerDeck :: IdentityId -> GameAppT [PlayerCard]
playerDeck p = unDeck . playerIdentityDeck <$> playerAttrs p

setPlayerDeck :: IdentityId -> [PlayerCard] -> GameAppT ()
setPlayerDeck p cards = withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity attrs) -> PlayerIdentity attrs {playerIdentityDeck = Deck cards}

_clearApparitions :: GameAppT ()
_clearApparitions = do
  minions <- getsGame (HashMap.keys . gameMinions)
  resolve [RemoveFromGame (toRef mid) | mid <- minions]

spec :: Spec
spec = describe "Mysterio" do
  it "starts with a Shifting Apparition engaged with every player" do
    tg <- mysterio ["spider-man-miles-morales", "ghost-spider"]
    inGame tg do
      minions <- getsGame (map toAttrs . HashMap.elems . gameMinions)
      map (toCardCode . minionCardDef) minions `shouldMatchList` ["27091", "27091"]
      map minionEngagedIdentity minions `shouldMatchList` tg.seats

  it "advances Maze of Mirrors to Edge of Reality, shuffles two encounter cards per player, and loses if Edge is completed" do
    tg <- mysterio ["spider-man-miles-morales", "ghost-spider"]
    inGame tg do
      _ <- setEncounterDeck (replicate 6 Attachments.smHumongousHallucination) []
      old <- mainSchemeIdNow
      resolve [AdvanceMainScheme old]
      codes <- getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameMainSchemes)
      codes `shouldBe` ["27088"]
      for_ tg.seats \p -> do
        cards <- playerDeck p
        length (mapMaybe playerZoneEncounterCard cards) `shouldBe` 2
      current <- mainSchemeIdNow
      resolve [AdvanceMainScheme current]
      getsGame gameState `shouldReturn` Finished Lost

  it "advances Mysterio I to II and shuffles the top encounter card into each player's deck" do
    tg <- mysterio ["spider-man-miles-morales", "ghost-spider"]
    inGame tg do
      _ <- setEncounterDeck (replicate 4 Attachments.smHumongousHallucination) []
      vid <- villainIdNow
      resolve [VillainMessage vid VillainDefeated]
      toCardCode . getCardDef <$> getVillain `shouldReturn` "27085"
      for_ tg.seats \p -> length . mapMaybe playerZoneEncounterCard <$> playerDeck p `shouldReturn` 1

  it "advances expert Mysterio II to III and discards five cards from each player's deck" do
    tg <- expertMysterio
    let p = player1 tg
    inGame tg do
      normal <- stackDeck p (replicate 8 "27031")
      setPlayerDeck p normal
      before <- length <$> discardOf p
      vid <- villainIdNow
      resolve [VillainMessage vid VillainDefeated]
      toCardCode . getCardDef <$> getVillain `shouldReturn` "27086"
      length <$> discardOf p `shouldReturn` before + 5

  describe "Illusion boosts" do
    for_ [1 :: Int,2,3] \stage ->
      it ("places a resolved Illusion boost in the correct player zone at stage " <> show stage <> " without also discarding it") do
        tg <- if stage == 3 then expertMysterio else mysterio ["spider-man-miles-morales"]
        let p = player1 tg
        inGame tg do
          when (stage > 1) do
            vid <- villainIdNow
            resolve [VillainMessage vid VillainDefeated]
          clearHand p
          baseline <- stackDeck p ["27031", "27032", "27033"]
          setPlayerDeck p baseline
          ([illusion,_],_) <- setEncounterDeck [Attachments.smHumongousHallucination, Minions.hydraSoldier] []
          vid <- villainIdNow
          resolve [VillainMessage vid (VillainSchemesAgainst p)]
          let proxy = encounterCardInPlayerDeck p illusion
          case stage of
            1 -> discardOf p >>= (`shouldSatisfy` elem proxy)
            2 -> lastMay <$> playerDeck p `shouldReturn` Just proxy
            _ -> headMay <$> playerDeck p `shouldReturn` Just proxy
          encounterDiscardNow >>= (`shouldSatisfy` notElem illusion)
          pcOwner proxy `shouldBe` Nothing
          pcCardId proxy `shouldBe` ecCardId illusion

    it "leaves non-Illusion boosts in the encounter discard pile" do
      tg <- mysterio ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        ([boost,_],_) <- setEncounterDeck [Minions.hydraSoldier, Minions.hydraSoldier] []
        vid <- villainIdNow
        resolve [VillainMessage vid (VillainSchemesAgainst p)]
        encounterDiscardNow >>= (`shouldSatisfy` elem boost)
        playerDeck p >>= (`shouldSatisfy` all ((/= ecCardId boost) . pcCardId))

    it "resolves Masterful Mirage's additional boost before routing both Illusions" do
      tg <- mysterio ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        ([mirage,illusion,_],_) <- setEncounterDeck [Attachments.smMasterfulMirage, Attachments.smHumongousHallucination, Minions.hydraSoldier] []
        -- Keep the full activation below Maze of Mirrors' threshold so its
        -- advancement cannot replace the scheme whose threat is asserted.
        main <- mainSchemeIdNow
        resolve [MainSchemeMessage main (MainSchemeThreatRemoved GameRef 2)]
        before <- mainSchemeThreatNow
        vid <- villainIdNow
        resolve [VillainMessage vid (VillainSchemesAgainst p)]
        mainSchemeThreatNow `shouldReturn` before + 6
        discarded <- mapMaybe playerZoneEncounterCard <$> discardOf p
        discarded `shouldSatisfy` (\cards -> mirage `elem` cards && illusion `elem` cards)

  describe "encounter cards in player decks" do
    it "deals a drawn encounter card facedown and draws a replacement, preserving the physical card" do
      tg <- mysterio ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        encounter <- encounterCard Attachments.smHumongousHallucination
        [first,second,third] <- stackDeck p ["27031","27032","27033"]
        setPlayerDeck p [encounterCardInPlayerDeck p encounter,first,second,third]
        runNow [IdentityMessage p (DrawCards FromDeck 2)]
        pass
        handOf p >>= (`shouldMatchList` [first,second])
        dealtTo p `shouldReturn` [encounter]
        playerDeck p `shouldReturn` [third]

    it "also replaces an encounter card discarded from the deck with a facedown deal and a draw" do
      tg <- mysterio ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        encounter <- encounterCard Attachments.smHumongousHallucination
        [first,second,third] <- stackDeck p ["27031","27032","27033"]
        setPlayerDeck p [encounterCardInPlayerDeck p encounter,first,second,third]
        runNow [IdentityMessage p (DiscardFrom FromDeck 2 Nothing)]
        pass
        handOf p `shouldReturn` [second]
        discardOf p >>= (`shouldSatisfy` elem first)
        dealtTo p `shouldReturn` [encounter]
        playerDeck p `shouldReturn` [third]

  describe "Humongous Hallucination" do
    it "pays one resource and moves two encounter cards as its cost, then discards itself" do
      tg <- mysterio ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        changeForm p
        pass
        clearHand p
        attachment <- revealNow p Attachments.smHumongousHallucination
        pass
        ([first,second,_],_) <- setEncounterDeck (replicate 3 Attachments.smMasterfulMirage) []
        resource <- putInHand p "27031"
        choose "pay to discard Hallucination" (isAbilityOf $ AttachmentId $ unCardId $ ecCardId attachment)
        payWith [resource]
        pass
        cards <- mapMaybe playerZoneEncounterCard <$> playerDeck p
        cards `shouldSatisfy` (\xs -> first `elem` xs && second `elem` xs)
        encounterDiscardCodes >>= (`shouldSatisfy` elem "27089")

  describe "Masterful Mirage" do
    it "replaces player ability damage with discarding four cards from the dealing player's deck" do
      tg <- mysterio ["spider-man-miles-morales", "ghost-spider"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        mirage <- revealNow p Attachments.smMasterfulMirage
        pass
        cards <- stackDeck other (replicate 5 "27002")
        setPlayerDeck other cards
        before <- length <$> discardOf other
        vid <- villainIdNow
        damage <- choiceMessages other (DamageEnemy (VillainRef vid) (IdentityRef other) $ toDamage 3 FromAbility)
        runNow damage
        choose "resolve Mirage's forced replacement" (isAbilityOf $ AttachmentId $ unCardId $ ecCardId mirage)
        pass
        villainDamageNow `shouldReturn` 0
        length <$> discardOf other `shouldReturn` before + 4
        encounterDiscardCodes >>= (`shouldSatisfy` elem "27090")

    it "does not replace an ally's damage" do
      tg <- mysterio ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Attachments.smMasterfulMirage
        pass
        ally <- putIntoPlay p "01068"
        vid <- villainIdNow
        resolve [VillainMessage vid (VillainDamaged (AllyRef $ allyIdOf ally) $ toDamage 2 $ FromAllyAttack $ allyIdOf ally)]
        villainDamageNow `shouldReturn` 2

  describe "Shifting Apparition" do
    for_ [("ms-marvel", False), ("spider-man-miles-morales", True)] \(hero, excess) ->
      it ("shuffles a top encounter card only when defeated with excess damage: " <> show excess) do
        tg <- mysterio [hero]
        let p = player1 tg
        inGame tg do
          changeForm p
          pass
          clearHand p
          [apparition] <- getsGame (HashMap.keys . gameMinions)
          ([top,_],_) <- setEncounterDeck [Attachments.smHumongousHallucination, Minions.hydraSoldier] []
          choose "basic attack" (isHeroAttack p)
          vid <- villainIdNow
          offers (isTargetChoice vid) `shouldReturn` False
          choose "attack the guard" (isTargetChoice apparition)
          choose "resolve Apparition's When Defeated" (isAbilityOf apparition)
          pass
          cards <- mapMaybe playerZoneEncounterCard <$> playerDeck p
          (top `elem` cards) `shouldBe` excess

  describe "Deja Vu and Fearmonger" do
    it "takes the chosen damage then shuffles Deja Vu into any chosen player's deck" do
      tg <- mysterio ["spider-man-miles-morales", "ghost-spider"]
      let p = player1 tg
          other = player2 tg
      inGame tg do
        card <- revealNow p Treacheries.smDejaVu
        choose "take damage" (isLabel "$label.smDejaVuDamage")
        choose "shuffle into the other player's deck" (isTargetChoice other)
        pass
        damageOf p `shouldReturn` 1
        playerDeck other >>= (`shouldSatisfy` any ((== ecCardId card) . pcCardId))
        encounterDiscardNow >>= (`shouldSatisfy` notElem card)

    it "offers the alternative threat choice" do
      tg <- mysterio ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        before <- mainSchemeThreatNow
        _ <- revealNow p Treacheries.smDejaVu
        choose "place threat" (isLabel "$label.smDejaVuThreat")
        choose "your deck" (isTargetChoice p)
        pass
        mainSchemeThreatNow `shouldReturn` before + 1
        damageOf p `shouldReturn` 0

    it "Fearmonger discards the hand, draws to the modified hand size, and surges" do
      tg <- mysterio ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        old <- handOf p
        _ <- stackEncounterDeck [Minions.hydraSoldier]
        _ <- revealNow p Treacheries.smFearmonger
        pass
        length <$> handOf p `shouldReturn` 6
        discarded <- discardOf p
        all (`elem` discarded) old `shouldBe` True
        dealt <- map toCardCode <$> dealtTo p
        dealt `shouldSatisfy` elem (toCardCode Minions.hydraSoldier)

    it "Fearmonger's boost deals that exact card facedown without also discarding it" do
      tg <- mysterio ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        ([boost,_],_) <- setEncounterDeck [Treacheries.smFearmonger, Minions.hydraSoldier] []
        vid <- villainIdNow
        resolve [VillainMessage vid (VillainSchemesAgainst p)]
        dealtTo p `shouldReturn` [boost]
        encounterDiscardNow >>= (`shouldSatisfy` notElem boost)

    it "can pay Fearmonger's two mental resources instead of being dealt the boost card" do
      tg <- mysterio ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        mental <- putInHand p "01089"
        ([boost,_],_) <- setEncounterDeck [Treacheries.smFearmonger, Minions.hydraSoldier] []
        vid <- villainIdNow
        start [VillainMessage vid (VillainSchemesAgainst p)]
        choose "pay two mental" (isLabel "$label.smSpendTwoMentalResources")
        payWith [mental]
        pass
        dealtTo p `shouldReturn` []
        encounterDiscardNow >>= (`shouldSatisfy` elem boost)
        discardOf p >>= (`shouldSatisfy` elem mental)

    it "reveals a Deja Vu boost and moves that physical card to a player deck exactly once" do
      tg <- mysterio ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        ([boost,_],_) <- setEncounterDeck [Treacheries.smDejaVu, Minions.hydraSoldier] []
        vid <- villainIdNow
        start [VillainMessage vid (VillainSchemesAgainst p)]
        choose "take one damage" (isLabel "$label.smDejaVuDamage")
        choose "your deck" (isTargetChoice p)
        pass
        cards <- playerDeck p
        length (filter ((== ecCardId boost) . pcCardId) cards) `shouldBe` 1
        encounterDiscardNow >>= (`shouldSatisfy` notElem boost)
