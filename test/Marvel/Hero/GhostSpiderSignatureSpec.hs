module Marvel.Hero.GhostSpiderSignatureSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Hero.GhostSpiderSpec (ghostSpider, asGhost, attackVillain, isReflexes)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Obligation.Types qualified as Obligation
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

clearGeorges :: IdentityId -> GameAppT ()
clearGeorges p = withGame_ $ entitiesL . playersL . ix p %~ \(PlayerIdentity a) ->
  let keep = filter ((/= "27007") . toCardCode)
   in PlayerIdentity a {playerIdentityHand = Hand $ keep $ unHand $ playerIdentityHand a,
      playerIdentityDeck = Deck $ keep $ unDeck $ playerIdentityDeck a,
      playerIdentityDiscard = Discard $ keep $ unDiscard $ playerIdentityDiscard a}

spec :: Spec
spec = describe "Ghost-Spider signature cards" do
  describe "Ghost Kick" do
    it "attacks for six after a basic attack" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        event <- putInHand p "27002"
        energy <- putInHand p "01088"
        offers (isPlay event) `shouldReturn` False
        attackVillain p
        playWith event [energy]
        chooseVillain
        villainDamageNow `shouldReturn` 8

    it "can answer a basic thwart" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        placeMainThreat 3
        event <- putInHand p "27002"
        energy <- putInHand p "01088"
        choose "basic thwart" (isHeroThwart p)
        chooseMainScheme
        playWith event [energy]
        chooseVillain
        villainDamageNow `shouldReturn` 6

    it "can answer a basic defense after the attack finishes" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        event <- putInHand p "27002"
        energy <- putInHand p "01088"
        _ <- stackEncounterDeck [Treacheries.imTough]
        vid <- villainIdNow
        runNow [VillainMessage vid $ VillainAttacks p]
        choose "defend" isDefend
        playWith event [energy]
        chooseVillain
        villainDamageNow `shouldReturn` 6

    it "does not answer a stunned basic attack" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        event <- putInHand p "27002"
        _ <- putInHand p "01088"
        resolve [IdentityMessage p IdentityStunned]
        choose "attempt basic attack" (isHeroAttack p)
        offers (isPlay event) `shouldReturn` False

  describe "Phantom Flip" do
    it "removes five threat after a basic attack" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        placeMainThreat 6
        threat <- mainSchemeThreatNow
        event <- putInHand p "27004"
        genius <- putInHand p "01089"
        offers (isPlay event) `shouldReturn` False
        attackVillain p
        playWith event [genius]
        chooseMainScheme
        mainSchemeThreatNow `shouldReturn` (threat - 5)

    it "can answer a basic thwart, but not a confused attempt" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        placeMainThreat 6
        event <- putInHand p "27004"
        genius <- putInHand p "01089"
        choose "basic thwart" (isHeroThwart p)
        chooseMainScheme
        playWith event [genius]
        chooseMainScheme
        mainSchemeThreatNow `shouldReturn` 0
        choose "Dizzying Reflexes" isReflexes
        pass
        placeMainThreat 3
        second <- putInHand p "27004"
        _ <- putInHand p "01089"
        resolve [IdentityMessage p IdentityConfused]
        choose "attempt basic thwart" (isHeroThwart p)
        offers (isPlay second) `shouldReturn` False

  describe "Parental Guidance" do
    it "searches deck and discard for George when he is absent" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        clearHand p
        clearGeorges p
        [george] <- stackDiscard p ["27007"]
        event <- putInHand p "27003"
        playWith event []
        choose "find George" (isTargetChoice george)
        handOf p >>= (`shouldSatisfy` elem george)
        discardOf p >>= (`shouldSatisfy` notElem george)

    it "attaches an event from discard to George in play" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        clearHand p
        george <- putIntoPlay p "27007"
        [stored] <- stackDiscard p ["27002"]
        event <- putInHand p "27003"
        playWith event []
        choose "attach Ghost Kick" (isTargetChoice stored)
        getSupportHeldCardsOn (supportIdOf george) `shouldReturn` [stored]
        discardOf p >>= (`shouldSatisfy` notElem stored)

    it "can attach from hand and respects George's global three-card maximum" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        clearHand p
        george <- putIntoPlay p "27007"
        stored <- putInHand p "27002"
        event <- putInHand p "27003"
        playWith event []
        choose "attach from hand" (isTargetChoice stored)
        for_ ["09021", "27004"] $ \code -> do
          c <- putInHand p code
          resolve [SupportMessage (supportIdOf george) ReadiedSupport]
          choose "attach event to George" (isAbilityOfCard george)
          choose "store event" (isTargetChoice c)
        extra <- putInHand p "27002"
        guidance <- putInHand p "27003"
        offers (isPlay guidance) `shouldReturn` False
        length <$> getSupportHeldCardsOn (supportIdOf george) `shouldReturn` 3
        handOf p >>= (`shouldSatisfy` elem extra)

  describe "Pirouette and Punch" do
    it "deals boost icons plus one and cancels a treachery's reveal" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        event <- putInHand p "27005"
        genius <- putInHand p "01089"
        card <- revealNow p Treacheries.caughtOffGuard
        playWith event [genius]
        pass
        villainDamageNow `shouldReturn` 2
        encounterDiscard >>= (`shouldSatisfy` elem card)
        dealtTo p `shouldReturn` []

    it "cancels a minion's When Revealed while allowing it to enter play" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        event <- putInHand p "27005"
        genius <- putInHand p "01089"
        card <- revealNow p Minions.shocker
        playWith event [genius]
        pass
        damageOf p `shouldReturn` 0
        villainDamageNow `shouldReturn` 3
        getsGame (HashMap.member (MinionId $ unCardId $ ecCardId card) . gameMinions) `shouldReturn` True

  describe "Web Binding" do
    it "cancels a villain attack without defending or damaging the villain" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        event <- putInHand p "27006"
        genius <- putInHand p "01089"
        vid <- villainIdNow
        runNow [VillainMessage vid $ VillainAttacks p]
        playWith event [genius]
        pass
        damageOf p `shouldReturn` 0
        exhaustedNow p `shouldReturn` False
        villainDamageNow `shouldReturn` 0

    it "cancels a villain scheme as well" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        event <- putInHand p "27006"
        genius <- putInHand p "01089"
        before <- mainSchemeThreatNow
        vid <- villainIdNow
        runNow [VillainMessage vid VillainSchemes]
        playWith event [genius]
        pass
        mainSchemeThreatNow `shouldReturn` before

    it "deals four damage to the minion whose activation it cancels" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        mid <- addMinion p Minions.hydraMercenary
        event <- putInHand p "27006"
        genius <- putInHand p "01089"
        runNow [MinionMessage mid $ MinionAttacks p]
        playWith event [genius]
        pass
        damageOf p `shouldReturn` 0
        minionDamageOf mid `shouldReturn` Nothing

    it "requires a mental resource and is not offered for a stunned enemy" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        asGhost p
        event <- putInHand p "27006"
        _ <- putInHand p "01088"
        vid <- villainIdNow
        runNow [VillainMessage vid $ VillainAttacks p]
        offers (isPlay event) `shouldReturn` False
        pass
        _ <- putInHand p "01089"
        resolve [VillainMessage vid $ VillainStunned GameRef]
        runNow [VillainMessage vid $ VillainAttacks p]
        offers (isPlay event) `shouldReturn` False

  describe "Worried Father" do
    for_ ["hand", "deck", "discard"] $ \zone ->
      it ("attaches George facedown from the " <> zone <> " and returns him by exhausting Gwen") do
        tg <- ghostSpider
        let p = player1 tg
        inGame tg do
          clearHand p
          clearGeorges p
          george <- case zone of
            "hand" -> putInHand p "27007"
            "deck" -> head <$> stackDeck p ["27007"]
            _ -> head <$> stackDiscard p ["27007"]
          card <- revealNow p Obligations.worriedFather
          pass
          let oid = ObligationId $ unCardId $ ecCardId card
          held <- getsGame (maybe [] (Obligation.obligationHeldCards . toAttrs) . HashMap.lookup oid . gameObligations)
          held `shouldBe` [george]
          handOf p >>= (`shouldSatisfy` notElem george)
          choose "exhaust Gwen and remove the obligation" (isAbilityOf $ ObligationRef oid)
          exhaustedNow p `shouldReturn` True
          handOf p >>= (`shouldSatisfy` elem george)
          obligationsInPlay `shouldReturn` 0
          encounterDiscard >>= (`shouldSatisfy` notElem card)

    it "takes George out of play, discards his events, and cannot be removed in hero form" do
      tg <- ghostSpider
      let p = player1 tg
      inGame tg do
        clearHand p
        george <- putIntoPlay p "27007"
        event <- putInHand p "09021"
        choose "store event" (isAbilityOfCard george)
        choose "attach Warning" (isTargetChoice event)
        changeForm p
        card <- revealNow p Obligations.worriedFather
        pass
        let oid = ObligationId $ unCardId $ ecCardId card
        supportsWith "27007" `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` elem event)
        offers (isAbilityOf $ ObligationRef oid) `shouldReturn` False
