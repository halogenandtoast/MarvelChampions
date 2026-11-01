module Marvel.Aspect.CyclopsAlliesSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Identity (getModifiedAllyLimit)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Window qualified as W
import TestImport

cardAbility :: PlayerCard -> Natural -> Choice -> Bool
cardAbility card index = \case
  UseAbility a -> abilitySource a == toSource (allyIdOf card) && abilityIndex a == index
  _ -> False

supportAbility :: PlayerCard -> Choice -> Bool
supportAbility card = \case
  UseAbility a -> abilitySource a == toSource (supportIdOf card) && abilityIndex a == 1
  _ -> False

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

playPaid :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
playPaid you code payment = do
  card <- putInHand you code
  money <- traverse (putInHand you) payment
  playWith card money
  pure card

allyDamageNow :: PlayerCard -> GameAppT Natural
allyDamageNow card = getsGame $ maybe 0 (allyDamage . toAttrs) . HashMap.lookup (allyIdOf card) . gameAllies

supportExhaustedNow :: PlayerCard -> GameAppT Bool
supportExhaustedNow card = getsGame $ maybe False (supportExhausted . toAttrs) . HashMap.lookup (supportIdOf card) . gameSupports

spec :: Spec
spec = describe "Cyclops allies and locations" do
  describe "Beast (33011)" do
    it "searches his controller's deck for a resource card after entering play" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        clearHand p
        [resource, tactic] <- stackDeck p ["01089", "33016"]
        beast <- playPaid p "33011" ["01088", "01090"]
        choose "Beast response" (cardAbility beast 1)
        offers (isTargetChoice tactic) `shouldReturn` False
        choose "resource from deck" (isTargetChoice resource)
        pass
        handOf p >>= (`shouldSatisfy` elem resource)
        unDeck . playerIdentityDeck <$> playerAttrs p >>= (`shouldSatisfy` notElem resource)

    it "also finds resource cards in his controller's discard pile" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        clearHand p
        resource <- putInDiscard p "01089"
        beast <- playPaid p "33011" ["01088", "01090"]
        choose "Beast response" (cardAbility beast 1)
        choose "discarded resource" (isTargetChoice resource)
        pass
        handOf p >>= (`shouldSatisfy` elem resource)
        discardOf p >>= (`shouldSatisfy` notElem resource)

    it "may decline the search when put into play" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        clearHand p
        beast <- putInHand p "33011"
        start [PutCardIntoPlay p beast NoPayment Nothing False]
        offers (cardAbility beast 1) `shouldReturn` True
        choose "decline Beast" (isLabel "$label.noResponse")
        pass
        handOf p `shouldReturn` []

  describe "Dust (33012)" do
    it "attacks every minion across both players and pays two consequential damage once" do
      tg <- testGame ["cyclops", "shadowcat"]
      inGame tg do
        let p = player1 tg
            other = player2 tg
        clearHand p
        dust <- putIntoPlay p "33012"
        first <- addMinion p Minions.hydraSoldier
        second <- addMinion other Minions.hydraSoldier
        attrs <- allyIn "33012"
        choose "Dust attacks" (isAllyAttack attrs)
        choose "initial minion" (isTargetChoice first)
        choose "Dust interrupts" (cardAbility dust 1)
        choose "first minion landing" (isTargetChoice first)
        choose "second minion landing" (isTargetChoice second)
        pass
        minionDamageOf first `shouldReturn` Just 1
        minionDamageOf second `shouldReturn` Just 1
        allyDamageNow dust `shouldReturn` 2

    it "can decline the interrupt and attack just the chosen minion for normal consequence" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        clearHand p
        dust <- putIntoPlay p "33012"
        first <- addMinion p Minions.hydraSoldier
        second <- addMinion p Minions.hydraSoldier
        attrs <- allyIn "33012"
        choose "Dust attacks" (isAllyAttack attrs)
        choose "first minion" (isTargetChoice first)
        offers (cardAbility dust 1) `shouldReturn` True
        pass
        minionDamageOf first `shouldReturn` Just 1
        minionDamageOf second `shouldReturn` Just 0
        allyDamageNow dust `shouldReturn` 1

    it "does not offer her multi-minion interrupt against the villain" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        clearHand p
        dust <- putIntoPlay p "33012"
        attrs <- allyIn "33012"
        choose "Dust attacks" (isAllyAttack attrs)
        chooseVillain
        offers (cardAbility dust 1) `shouldReturn` False
        pass
        villainDamageNow `shouldReturn` 1
        allyDamageNow dust `shouldReturn` 1

    it "does not carry the extra consequential damage into her next attack" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        clearHand p
        dust <- putIntoPlay p "33012"
        target <- addMinion p Minions.weaponsRunner
        attrs <- allyIn "33012"
        choose "Dust attacks" (isAllyAttack attrs)
        choose "minion" (isTargetChoice target)
        choose "Dust interrupts" (cardAbility dust 1)
        choose "landing" (isTargetChoice target)
        pass
        allyDamageNow dust `shouldReturn` 2
        resolve [AllyMessage (allyIdOf dust) (AllySetDamage 0), AllyMessage (allyIdOf dust) ReadiedAlly]
        attrs' <- allyIn "33012"
        choose "Dust attacks again" (isAllyAttack attrs')
        choose "same minion" (isTargetChoice target)
        pass
        allyDamageNow dust `shouldReturn` 1

  describe "Dust attack interactions" do
    for_ [False, True] $ \befuddleInitial ->
      it ("uses Befuddle's numeric THW for every target when the upgrade is on " <> if befuddleInitial then "the initial target" else "an additional target") do
        tg <- testGame ["cyclops"]
        inGame tg do
          let p = player1 tg
          clearHand p
          dust <- putIntoPlay p "33012"
          attackTraining <- putInHand p "32043"
          money <- putInHand p "33016"
          playOn attackTraining (allyIdOf dust) [money]
          first <- addMinion p Minions.hydraSoldier
          second <- addMinion p Minions.hydraSoldier
          befuddle <- putInHand p "33033"
          playOn befuddle (if befuddleInitial then first else second) []
          attrs <- allyIn "33012"
          choose "Dust attacks" (isAllyAttack attrs)
          choose "initial target" (isTargetChoice first)
          when befuddleInitial $ choose "Befuddle interrupt" (isAbilityOfCard befuddle)
          choose "Dust attacks all minions" (cardAbility dust 1)
          unless befuddleInitial $ choose "Befuddle on additional target" (isAbilityOfCard befuddle)
          choose "first landing" (isTargetChoice first)
          choose "second landing" (isTargetChoice second)
          pass
          minionDamageOf first `shouldReturn` Just 1
          minionDamageOf second `shouldReturn` Just 1
          allyDamageNow dust `shouldReturn` 2

    it "Coordinated Attack on an additional target reduces this single attack's consequence" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        clearHand p
        dust <- putIntoPlay p "33012"
        first <- addMinion p Minions.hydraSoldier
        second <- addMinion p Minions.hydraSoldier
        coordinated <- putInHand p "33016"
        playOn coordinated second []
        attrs <- allyIn "33012"
        choose "Dust attacks" (isAllyAttack attrs)
        choose "initial target" (isTargetChoice first)
        choose "Dust attacks all" (cardAbility dust 1)
        chooseIf (isAbilityOfCard coordinated)
        choose "first landing" (isTargetChoice first)
        choose "second landing" (isTargetChoice second)
        pass
        minionDamageOf first `shouldReturn` Just 1
        minionDamageOf second `shouldReturn` Just 1
        allyDamageNow dust `shouldReturn` 1

    it "Coordinated Attacks on separate targets stack without multiplying consequential payments" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        clearHand p
        dust <- putIntoPlay p "33012"
        first <- addMinion p Minions.hydraSoldier
        second <- addMinion p Minions.hydraSoldier
        coordinateFirst <- putInHand p "33016"
        playOn coordinateFirst first []
        coordinateSecond <- putInHand p "33016"
        playOn coordinateSecond second []
        attrs <- allyIn "33012"
        choose "Dust attacks" (isAllyAttack attrs)
        choose "initial target" (isTargetChoice first)
        chooseIf (isAbilityOfCard coordinateFirst)
        choose "Dust attacks all" (cardAbility dust 1)
        chooseIf (isAbilityOfCard coordinateSecond)
        choose "first landing" (isTargetChoice first)
        choose "second landing" (isTargetChoice second)
        pass
        minionDamageOf first `shouldReturn` Just 1
        minionDamageOf second `shouldReturn` Just 1
        allyDamageNow dust `shouldReturn` 0

  describe "Rockslide (33013)" do
    it "deals three attack damage and takes two consequential damage" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        clearHand p
        rockslide <- putIntoPlay p "33013"
        attrs <- allyIn "33013"
        choose "Rockslide attacks" (isAllyAttack attrs)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 3
        allyDamageNow rockslide `shouldReturn` 2

    it "thwarts for one and takes only one consequential damage" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        clearHand p
        rockslide <- putIntoPlay p "33013"
        placeMainThreat 3
        before <- mainSchemeThreatNow
        attrs <- allyIn "33013"
        choose "Rockslide thwarts" (isAllyThwart attrs)
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` before - 1
        allyDamageNow rockslide `shouldReturn` 1

    it "retaliates for one after surviving a defended enemy attack" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        asHero p
        rockslide <- putIntoPlay p "33013"
        vid <- villainIdNow
        start [VillainMessage vid (VillainAttacksWithoutBoost p)]
        choose "Rockslide defends" (\case AllyDefend aid _ -> aid == allyIdOf rockslide; _ -> False)
        pass
        allyDamageNow rockslide `shouldReturn` 2
        villainDamageNow `shouldReturn` 1

  describe "Blindfold (33014)" do
    it "discards one of the top five encounter cards and preserves the others' order" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        clearHand p
        [a,b,c,d,e] <- stackEncounterDeck [Treacheries.advance, Minions.hydraSoldier, Treacheries.assault, Minions.weaponsRunner, Treacheries.shadowOfThePast]
        blindfold <- playPaid p "33014" ["01088", "33016"]
        choose "Blindfold response" (cardAbility blindfold 1)
        choose "discard middle card" (isTargetChoice c)
        pass
        take 4 <$> scenarioEncounterDeckNow `shouldReturn` [a,b,d,e]
        encounterDiscardNow >>= (`shouldSatisfy` elem c)

    it "can decline the encounter-deck look" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        clearHand p
        blindfold <- playPaid p "33014" ["01088", "33016"]
        before <- scenarioEncounterDeckNow
        offers (cardAbility blindfold 1) `shouldReturn` True
        pass
        scenarioEncounterDeckNow `shouldReturn` before

    it "uses the printed zero attack consequential damage even after her attack is increased" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        asHero p
        blindfold <- putIntoPlay p "33014"
        training <- putInHand p "33015"
        money <- putInHand p "33016"
        playOn training (allyIdOf blindfold) [money]
        attrs <- allyIn "33014"
        choose "Blindfold attacks" (isAllyAttack attrs)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 1
        allyDamageNow blindfold `shouldReturn` 0

    it "takes one consequential damage after thwarting for two" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        clearHand p
        blindfold <- putIntoPlay p "33014"
        placeMainThreat 3
        before <- mainSchemeThreatNow
        attrs <- allyIn "33014"
        choose "Blindfold thwarts" (isAllyThwart attrs)
        chooseMainScheme
        pass
        mainSchemeThreatNow `shouldReturn` before - 2
        allyDamageNow blindfold `shouldReturn` 1

  describe "Angel (33019)" do
    for_ [False, True] $ \heroForm ->
      it ("costs two for Cyclops in " <> if heroForm then "hero form" else "alter-ego form") do
        tg <- testGame ["cyclops"]
        inGame tg do
          let p = player1 tg
          when heroForm $ asHero p
          clearHand p
          angel <- playPaid p "33019" ["01090"]
          pass
          allyController <$> allyIn "33019" `shouldReturn` p
          handOf p `shouldReturn` []
          allyDamageNow angel `shouldReturn` 0

    it "costs the full three for an identity with neither Mutant nor X-Men" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        let p = player1 tg
        clearHand p
        angel <- putInHand p "33019"
        two <- putInHand p "01090"
        offers (isPlay angel) `shouldReturn` False
        one <- putInHand p "33016"
        playWith angel [two,one]
        pass
        handOf p `shouldReturn` []
        allyController <$> allyIn "33019" `shouldReturn` p

  describe "Utopia (33020)" do
    it "increases only its controller's ally limit while all that player's allies are X-Men" do
      tg <- testGame ["cyclops", "captain-marvel"]
      inGame tg do
        let p = player1 tg
            other = player2 tg
        clearHand p
        _ <- putIntoPlay p "33020"
        (getModifiedAllyLimit =<< getPlayer p) `shouldReturn` 4
        (getModifiedAllyLimit =<< getPlayer other) `shouldReturn` 3
        _ <- putIntoPlay p "33019"
        (getModifiedAllyLimit =<< getPlayer p) `shouldReturn` 4
        nonXmen <- putIntoPlay p "01084"
        (getModifiedAllyLimit =<< getPlayer p) `shouldReturn` 3
        resolve [RemoveFromPlay (AllyRef $ allyIdOf nonXmen)]
        (getModifiedAllyLimit =<< getPlayer p) `shouldReturn` 4

    it "readies an exhausted X-Men hero after an X-Men ally enters play" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        asHero p
        utopia <- putIntoPlay p "33020"
        choose "Cyclops attacks" (isHeroAttack p)
        chooseVillain
        pass
        _ <- playPaid p "33019" ["01090"]
        choose "Utopia responds" (supportAbility utopia)
        choose "ready Cyclops" (isTargetChoice p)
        pass
        exhaustedNow p `shouldReturn` False
        supportExhaustedNow utopia `shouldReturn` True

    it "can ready another player's X-Men ally and responds to put-into-play entry" do
      tg <- testGame ["cyclops", "shadowcat"]
      inGame tg do
        let p = player1 tg
            other = player2 tg
        clearHand p
        utopia <- putIntoPlay p "33020"
        ally <- putIntoPlay other "33013"
        resolve [AllyMessage (allyIdOf ally) ExhaustedAlly]
        entrant <- putInHand p "33019"
        start [PutCardIntoPlay p entrant NoPayment Nothing False]
        choose "Utopia response to put into play" (supportAbility utopia)
        choose "other player's ally" (isTargetChoice $ allyIdOf ally)
        pass
        allyExhausted <$> allyIn "33013" `shouldReturn` False
        supportExhaustedNow utopia `shouldReturn` True

    it "does not respond to a non-X-Men ally entering play" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        clearHand p
        utopia <- putIntoPlay p "33020"
        ally <- putIntoPlay p "33019"
        resolve [AllyMessage (allyIdOf ally) ExhaustedAlly]
        nick <- putInHand p "01084"
        start [PutCardIntoPlay p nick NoPayment Nothing False]
        offers (supportAbility utopia) `shouldReturn` False
        pass
        allyExhausted <$> allyIn "33019" `shouldReturn` True

    it "cannot trigger again while exhausted" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        clearHand p
        utopia <- putIntoPlay p "33020"
        ally <- putIntoPlay p "33013"
        resolve [AllyMessage (allyIdOf ally) ExhaustedAlly]
        angel <- putInHand p "33019"
        start [PutCardIntoPlay p angel NoPayment Nothing False]
        choose "Utopia response" (supportAbility utopia)
        choose "ready Rockslide" (isTargetChoice $ allyIdOf ally)
        pass
        resolve [AllyMessage (allyIdOf ally) ExhaustedAlly]
        beast <- putInHand p "33011"
        start [PutCardIntoPlay p beast NoPayment Nothing False]
        offers (supportAbility utopia) `shouldReturn` False
        pass
        allyExhausted <$> allyIn "33013" `shouldReturn` True

  describe "Danger Room (33021)" do
    it "searches its controller's deck and attaches a legal Training to the entering ally for free" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        clearHand p
        room <- putIntoPlay p "33021"
        [training] <- stackDeck p ["33015"]
        angel <- playPaid p "33019" ["01090"]
        choose "Danger Room response" (supportAbility room)
        choose "Training from deck" (isTargetChoice training)
        pass
        upgrades <- upgradesWith "33015"
        map upgradeAttachedAlly upgrades `shouldBe` [Just $ allyIdOf angel]
        map upgradeController upgrades `shouldBe` [p]
        handOf p `shouldReturn` []
        supportExhaustedNow room `shouldReturn` True

    it "lets another Mutant alter-ego search their own discard and attach to someone else's ally" do
      tg <- testGame ["cyclops", "shadowcat"]
      inGame tg do
        let p = player1 tg
            other = player2 tg
        asHero p
        clearHand other
        room <- putIntoPlay p "33021"
        training <- putInDiscard other "32013"
        wrong <- putInDiscard p "33015"
        angel <- playPaid p "33019" ["01090"]
        fmap fst <$> asked `shouldReturn` Just other
        choose "other Mutant uses Danger Room" (supportAbility room)
        offers (isTargetChoice wrong) `shouldReturn` False
        choose "other player's Training" (isTargetChoice training)
        pass
        upgrades <- upgradesWith "32013"
        map upgradeAttachedAlly upgrades `shouldBe` [Just $ allyIdOf angel]
        -- RR v1.8 Upgrade: the host's controller controls attached upgrades,
        -- while the searching player's ownership is retained.
        map upgradeController upgrades `shouldBe` [p]
        map upgradeOwner upgrades `shouldBe` [other]
        discardOf other >>= (`shouldSatisfy` notElem training)
        supportExhaustedNow room `shouldReturn` True

    it "lets its non-Mutant controller use the base alter-ego response" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        let p = player1 tg
        clearHand p
        room <- putIntoPlay p "33021"
        training <- putInDiscard p "33015"
        angel <- playPaid p "33019" ["01090", "33016"]
        choose "Carol uses her Danger Room" (supportAbility room)
        choose "Training" (isTargetChoice training)
        pass
        upgrades <- upgradesWith "33015"
        map upgradeAttachedAlly upgrades `shouldBe` [Just $ allyIdOf angel]

    it "does not let a non-Mutant noncontroller trigger the response" do
      tg <- testGame ["cyclops", "captain-marvel"]
      inGame tg do
        let p = player1 tg
            other = player2 tg
        asHero p
        clearHand other
        room <- putIntoPlay p "33021"
        _ <- putInDiscard other "33015"
        _ <- playPaid p "33019" ["01090"]
        offers (supportAbility room) `shouldReturn` False
        pass
        upgradesWith "33015" `shouldReturn` []
        supportExhaustedNow room `shouldReturn` False

    it "does not let an X-Men hero use its alter-ego response" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        asHero p
        room <- putIntoPlay p "33021"
        _ <- putInDiscard p "33015"
        _ <- playPaid p "33019" ["01090"]
        offers (supportAbility room) `shouldReturn` False
        supportExhaustedNow room `shouldReturn` False

    it "does not trigger for a non-X-Men entrant" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        clearHand p
        room <- putIntoPlay p "33021"
        _ <- putInDiscard p "33015"
        entrant <- putInHand p "01084"
        start [PutCardIntoPlay p entrant NoPayment Nothing False]
        offers (supportAbility room) `shouldReturn` False
        pass
        supportExhaustedNow room `shouldReturn` False

    it "rejects a second Training on an entrant that already has one" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        clearHand p
        ally <- putIntoPlay p "33019"
        first <- putInHand p "33015"
        money <- putInHand p "33016"
        playOn first (allyIdOf ally) [money]
        room <- putIntoPlay p "33021"
        second <- putInDiscard p "32013"
        start [CheckWindows [W.Window W.After $ W.AllyEnteredPlay $ allyIdOf ally]]
        choose "Danger Room searches" (supportAbility room)
        offers (isTargetChoice second) `shouldReturn` False
        choose "no legal Training" (isLabel "$label.noMatchingCardsFound")
        pass
        upgradesWith "32013" `shouldReturn` []
        length <$> upgradesWith "33015" `shouldReturn` 1
        discardOf p >>= (`shouldSatisfy` elem second)

    it "responds when the X-Men ally is put into play rather than played" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        clearHand p
        room <- putIntoPlay p "33021"
        training <- putInDiscard p "32013"
        entrant <- putInHand p "33019"
        start [PutCardIntoPlay p entrant NoPayment Nothing False]
        choose "Danger Room responds to entry" (supportAbility room)
        choose "Training from discard" (isTargetChoice training)
        pass
        upgrades <- upgradesWith "32013"
        map upgradeAttachedAlly upgrades `shouldBe` [Just $ allyIdOf entrant]
        handOf p `shouldReturn` []

    it "cannot respond a second time while exhausted" do
      tg <- testGame ["cyclops"]
      inGame tg do
        let p = player1 tg
        clearHand p
        room <- putIntoPlay p "33021"
        training <- putInDiscard p "32013"
        _ <- playPaid p "33019" ["01090"]
        choose "Danger Room response" (supportAbility room)
        choose "Training" (isTargetChoice training)
        pass
        _ <- putInDiscard p "33015"
        entrant <- putInHand p "33013"
        start [PutCardIntoPlay p entrant NoPayment Nothing False]
        offers (supportAbility room) `shouldReturn` False
        pass
        upgradesWith "33015" `shouldReturn` []
