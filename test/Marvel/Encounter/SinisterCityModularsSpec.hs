module Marvel.Encounter.SinisterCityModularsSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Matchers hiding (ExhaustedIdentity, ExhaustedAlly)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Minion.Runner qualified as Minion
import Marvel.Keyword (Keyword (Overkill, Piercing, Steady))
import Marvel.Query
import Marvel.Trait (Trait (Location))
import Marvel.Modifier
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

spec :: Spec
spec = describe "City in Chaos and Down to Earth" do
  describe "Panic in the Streets" do
    it "blanks Location and Persona supports across all players but leaves other supports intact" do
      tg <- testGame ["spider-man-miles-morales", "ghost-spider"]
      inGame tg do
        location <- putIntoPlay (player1 tg) "01091"
        persona <- putIntoPlay (player2 tg) "27007"
        shield <- putIntoPlay (player1 tg) "27055"
        scheme <- revealNow (player1 tg) Schemes.smPanicInTheStreets
        pass
        getModifiers (supportIdOf location) >>= (`shouldSatisfy` elem Blank)
        getModifiers (supportIdOf persona) >>= (`shouldSatisfy` elem Blank)
        getModifiers (supportIdOf shield) >>= (`shouldSatisfy` notElem Blank)
        selectList (SupportWithTrait Location) >>= (`shouldSatisfy` elem (supportIdOf location))
        sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 5
        resolve [SideSchemeMessage (sideSchemeOf scheme) (SideSchemeThwarted GameRef 5)]
        getModifiers (supportIdOf persona) >>= (`shouldSatisfy` notElem Blank)

  describe "Rhino and Calling in Favors" do
    it "gives the Rhino minion overkill and piercing in addition to printed Steady" do
      tg <- testGameAgainst "01113" ["spider-man-miles-morales"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.smRhino
        attrs <- getsGame (toAttrs . fromMaybe (error "Rhino missing") . HashMap.lookup mid . gameMinions)
        keywords <- Minion.getModifiedKeywords attrs
        keywords `shouldSatisfy` elem Overkill
        keywords `shouldSatisfy` elem Piercing
        keywords `shouldSatisfy` elem Steady

    it "makes an existing Rhino scheme with +2 SCH for that scheme alone" do
      tg <- testGameAgainst "01113" ["spider-man-miles-morales"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.smRhino
        before <- mainSchemeThreatNow
        _ <- revealNow (player1 tg) Treacheries.smCallingInFavors
        pass
        mainSchemeThreatNow `shouldReturn` before + 2
        resolve [MinionMessage mid MinionSchemes]
        mainSchemeThreatNow `shouldReturn` before + 2

    it "searches the discard pile for Rhino when absent and engages him with the revealing player" do
      tg <- testGameAgainst "01113" ["spider-man-miles-morales", "ghost-spider"]
      inGame tg do
        (_, [rhino]) <- setEncounterDeck [Minions.hydraSoldier] [Minions.smRhino]
        _ <- revealNow (player2 tg) Treacheries.smCallingInFavors
        pass
        minions <- getsGame (map toAttrs . HashMap.elems . gameMinions)
        map minionEngagedIdentity (filter ((== ecCardId rhino) . CardId . unMinionId . minionId) minions) `shouldBe` [player2 tg]
        encounterDiscard >>= (`shouldSatisfy` notElem rhino)

    it "does not leak its scheme bonus when two confuse cards cancel Rhino's activation" do
      tg <- testGameAgainst "01113" ["spider-man-miles-morales"]
      inGame tg do
        mid <- addMinion (player1 tg) Minions.smRhino
        resolve [MinionMessage mid (MinionConfused GameRef), MinionMessage mid (MinionConfused GameRef)]
        before <- mainSchemeThreatNow
        _ <- revealNow (player1 tg) Treacheries.smCallingInFavors
        pass
        resolve [MinionMessage mid MinionSchemes]
        mainSchemeThreatNow `shouldReturn` before

  describe "Now or Never" do
    it "always offers the acceleration choice and omits an unaffordable exhaust-and-resource cost" do
      tg <- testGame ["spider-man-miles-morales"]
      inGame tg do
        clearHand (player1 tg)
        before <- accelerationTokensNow
        _ <- revealNow (player1 tg) Treacheries.smNowOrNever
        pass
        accelerationTokensNow `shouldReturn` before + 1

    it "pays both the controlled character exhaustion and one resource instead of accelerating" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        resource <- putInHand p "27042"
        before <- accelerationTokensNow
        _ <- revealNow p Treacheries.smNowOrNever
        choose "exhaust and spend" (isLabel "$label.smNowOrNeverExhaust")
        choose "exhaust own identity" (isTargetChoice p)
        payWith [resource]
        pass
        isExhaustedNow p `shouldReturn` True
        discardOf p >>= (`shouldSatisfy` elem resource)
        accelerationTokensNow `shouldReturn` before

  describe "Common Criminal" do
    it "can be defeated by spending a physical resource in alter-ego to draw a card" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        mid <- addMinion p Minions.smCommonCriminal
        pass
        clearHand p
        physical <- putInHand p "27039"
        choose "pay the criminal" (isAbilityOf mid)
        payWith [physical]
        choose "draw reward" (isLabel "$label.smCriminalDraw")
        pass
        minionDamageOf mid `shouldReturn` Nothing
        length <$> handOf p `shouldReturn` 1

    it "can remove threat from a side scheme, including one that cannot be thwarted" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        work <- revealNow p Schemes.smVolunteerWork
        pass
        mid <- addMinion p Minions.smCommonCriminal
        pass
        clearHand p
        physical <- putInHand p "27039"
        choose "pay the criminal" (isAbilityOf mid)
        payWith [physical]
        choose "remove threat reward" (isLabel "$label.smCriminalRemoveThreat")
        choose "remove volunteer threat" (isTargetChoice $ sideSchemeOf work)
        pass
        sideSchemeThreat (sideSchemeOf work) `shouldReturn` 0

    it "offers no criminal action in hero form" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        mid <- addMinion p Minions.smCommonCriminal
        changeForm p
        pass
        offers (isAbilityOf mid) `shouldReturn` False

  describe "Friends and Family" do
    it "taxes only its owner's events and only while that identity is a hero" do
      tg <- testGame ["spider-man-miles-morales", "ghost-spider"]
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Obligations.smFriendsAndFamily
        pass
        getModifiers p >>= (`shouldSatisfy` notElem (AdditionalResourceCost (CardWithType EventType) 1))
        changeForm p
        pass
        getModifiers p >>= (`shouldSatisfy` elem (AdditionalResourceCost (CardWithType EventType) 1))
        getModifiers (player2 tg) >>= (`shouldSatisfy` notElem (AdditionalResourceCost (CardWithType EventType) 1))

    it "discards a signature card from hand as its alter-ego removal action" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        signature <- putInHand p "27031"
        aspect <- putInHand p "27042"
        obligation <- revealNow p Obligations.smFriendsAndFamily
        pass
        choose "remove friends and family" (isAbilityOf $ ObligationRef $ ObligationId $ unCardId $ ecCardId obligation)
        offers (isTargetChoice $ CardIdRef $ pcCardId aspect) `shouldReturn` False
        choose "discard signature" (isTargetChoice $ CardIdRef $ pcCardId signature)
        pass
        discardOf p >>= (`shouldSatisfy` elem signature)
        handOf p >>= (`shouldSatisfy` elem aspect)
        encounterDiscard >>= (`shouldSatisfy` elem obligation)

  describe "Volunteer Work" do
    it "cannot be thwarted, but spending two resources in alter-ego removes modified REC threat and draws for Civilian" do
      tg <- testGame ["spider-man-miles-morales", "ghost-spider"]
      let p = player1 tg
      inGame tg do
        work <- revealNow p Schemes.smVolunteerWork
        pass
        let sid = sideSchemeOf work
        schemeMatches SchemeWithoutThwartPrevention (SchemeSideSchemeId sid) `shouldReturn` False
        _ <- putIntoPlay p "05033"
        clearHand p
        energy <- putInHand p "01088"
        choose "volunteer" (isAbilityOf sid)
        payWith [energy]
        pass
        sideSchemeThreat sid `shouldReturn` 0
        length <$> handOf p `shouldReturn` 1

    it "does not draw a card for an alter-ego without the Civilian trait" do
      tg <- testGame ["she-hulk", "ghost-spider"]
      let p = player1 tg
      inGame tg do
        work <- revealNow p Schemes.smVolunteerWork
        pass
        clearHand p
        energy <- putInHand p "01088"
        choose "volunteer" (isAbilityOf $ sideSchemeOf work)
        payWith [energy]
        pass
        sideSchemeThreat (sideSchemeOf work) `shouldReturn` 1
        handOf p `shouldReturn` []

    it "has no action in hero form" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        work <- revealNow p Schemes.smVolunteerWork
        pass
        changeForm p
        pass
        offers (isAbilityOf $ sideSchemeOf work) `shouldReturn` False

  describe "Threat or Menace" do
    it "places two threat when the player declines to leave hero form" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        changeForm p
        pass
        before <- mainSchemeThreatNow
        _ <- revealNow p Treacheries.smThreatOrMenace
        choose "keep hero form" (isLabel "$label.doNotChangeForm")
        pass
        mainSchemeThreatNow `shouldReturn` before + 2

    it "delays an alter-ego form lock until the player's next turn and removes it at that turn's end" do
      tg <- testGame ["spider-man-miles-morales", "ghost-spider"]
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Treacheries.smThreatOrMenace
        choose "keep alter-ego form" (isLabel "$label.doNotChangeForm")
        pass
        getModifiers p >>= (`shouldSatisfy` notElem CannotChangeForm)
        runNow [IdentityEndedTurn p]
        getModifiers p >>= (`shouldSatisfy` notElem CannotChangeForm)
        runNow [IdentityMessage p BeginTurn]
        getModifiers p >>= (`shouldSatisfy` elem CannotChangeForm)
        getModifiers (player2 tg) >>= (`shouldSatisfy` notElem CannotChangeForm)
        runNow [IdentityEndedTurn p]
        getModifiers p >>= (`shouldSatisfy` notElem CannotChangeForm)

  describe "Loose Ends" do
    it "reveals the player's own obligation from discard and discards one random hand card only after changing to alter-ego during that reveal" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        changeForm p
        pass
        clearHand p
        _ <- putInHand p "27042"
        _ <- putInHand p "27043"
        _ <- setEncounterDeck [Minions.hydraSoldier] [Obligations.keepingSecrets]
        _ <- revealNow p Treacheries.smLooseEnds
        choose "change to Miles" (isLabel "$label.changeToB")
        pass
        choose "exhaust Miles to remove obligation" (isLabel "$label.exhaustMilesMoralesRemoveKeepingSecrets")
        pass
        length <$> handOf p `shouldReturn` 1
        identityMatches AlterEgoIdentity p `shouldReturn` True

    it "does not discard a hand card when the player was already in alter-ego" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putInHand p "27042"
        _ <- setEncounterDeck [Minions.hydraSoldier] [Obligations.keepingSecrets]
        _ <- revealNow p Treacheries.smLooseEnds
        choose "remove own obligation" (isLabel "$label.exhaustMilesMoralesRemoveKeepingSecrets")
        pass
        handOf p >>= (`shouldSatisfy` elem card)

    it "can recover an obligation previously removed from the game without making its earlier form change count again" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        obligation <- revealNow p Obligations.keepingSecrets
        choose "remove own obligation" (isLabel "$label.exhaustMilesMoralesRemoveKeepingSecrets")
        pass
        getRemovedFromGameEncounterCards >>= (`shouldSatisfy` elem obligation)
        clearHand p
        card <- putInHand p "27042"
        _ <- setEncounterDeck [Minions.hydraSoldier, Minions.hydraSoldier] []
        _ <- revealNow p Treacheries.smLooseEnds
        pass
        handOf p >>= (`shouldSatisfy` elem card)

    it "surges when no copy of the player's own obligation can be revealed" do
      tg <- testGame ["spider-man-miles-morales"]
      let p = player1 tg
      inGame tg do
        _ <- setEncounterDeck [Schemes.bombScare, Minions.hydraSoldier] []
        _ <- revealNow p Treacheries.smLooseEnds
        pass
        dealt <- map toCardCode <$> dealtTo p
        dealt `shouldSatisfy` elem "01109"
