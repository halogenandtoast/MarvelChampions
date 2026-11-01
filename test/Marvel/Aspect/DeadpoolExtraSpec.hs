module Marvel.Aspect.DeadpoolExtraSpec (spec) where

import Control.Monad (replicateM)
import Marvel.Ally.Types qualified as Ally
import Marvel.Minion.Cards qualified as Minions
import Marvel.Support.Types qualified as Support
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Villain.Types (villainIsTough)
import Marvel.Window qualified as W
import TestImport

becomeHero :: TestGame -> GameAppT ()
becomeHero tg = changeForm (player1 tg) >> pass >> clearHand (player1 tg)

payCards :: IdentityId -> Int -> GameAppT [PlayerCard]
payCards you n = replicateM n (putInHand you "01002")

heldByPlot :: GameAppT [PlayerCard]
heldByPlot = do
  supports <- supportsWith "44050"
  pure $ concatMap Support.supportHeldCards supports

playBob :: IdentityId -> GameAppT PlayerCard
playBob you = do
  card <- putInHand you "44043"
  payment <- payCards you 2
  playWith card payment
  choose "Bob response" (isAbilityOfCard card)
  pure card

prepareFront :: IdentityId -> GameAppT PlayerCard
prepareFront you = do
  card <- putInHand you "44047"
  _ <- putInHand you "01002"
  _ <- setEncounterDeck [Treacheries.imTough, Treacheries.imTough, Treacheries.imTough] []
  pure card

playFront :: IdentityId -> PlayerCard -> GameAppT ()
playFront you card = do
  _ <- revealNow you Treacheries.imTough
  choose "Get in Front of Me" (isPlay card)
  choose "pay" (\case PayWithCard _ -> True; _ -> False)
  chooseIf isFinishPayment

spec :: Spec
spec = describe "DeadpoolExtra" do
  describe "Bob, Agent of Hydra" do
    it "can deal two damage after entering play and has no consequential damage" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        _ <- playBob you
        choose "deal damage" (isLabel $ countLabel "dealDamageToAnEnemy" 2)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 2
        attrs <- allyIn "44043"
        choose "Bob attacks" (isAllyAttack attrs)
        chooseVillain
        pass
        allyDamage <$> allyIn "44043" `shouldReturn` 0
        getAccelerationCount `shouldReturn` 1

    it "can remove one threat instead of dealing damage" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        let you = player1 tg
        clearHand you
        placeMainThreat 3
        before <- mainSchemeThreatNow
        _ <- playBob you
        choose "remove threat" (isLabel $ countLabel "removeThreatFromAScheme" 1)
        main <- mainSchemeIdNow
        choose "main scheme" (isTargetChoice main)
        pass
        mainSchemeThreatNow `shouldReturn` before - 1
        villainDamageNow `shouldReturn` 0

    it "does not offer its response again after an attack" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        card <- putIntoPlay (player1 tg) "44043"
        attrs <- allyIn "44043"
        choose "attack" (isAllyAttack attrs)
        chooseVillain
        offers (isAbilityOfCard card) `shouldReturn` False

  describe "Negasonic Teenage Warhead" do
    it "takes two damage to cancel a revealed treachery's When Revealed effects" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        let you = player1 tg
        clearHand you
        card <- putIntoPlay you "44044"
        _ <- revealNow you Treacheries.imTough
        choose "Negasonic interrupt" (isAbilityOfCard card)
        pass
        villainIsTough <$> getVillain `shouldReturn` False
        allyDamage <$> allyIn "44044" `shouldReturn` 2
        getHazardCount `shouldReturn` 1

    it "can pay by dealing damage into her tough status" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        let you = player1 tg
        clearHand you
        card <- putIntoPlay you "44044"
        resolve [GiveStatus (AllyRef $ allyIdOf card) ToughStatus]
        _ <- revealNow you Treacheries.imTough
        choose "Negasonic interrupt" (isAbilityOfCard card)
        pass
        villainIsTough <$> getVillain `shouldReturn` False
        attrs <- allyIn "44044"
        Ally.allyTough attrs `shouldBe` False
        Ally.allyDamage attrs `shouldBe` 0

    it "still cancels the treachery if paying the damage cost defeats her" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        let you = player1 tg
        clearHand you
        card <- putIntoPlay you "44044"
        resolve [AllyMessage (allyIdOf card) $ AllySetDamage 2]
        _ <- revealNow you Treacheries.imTough
        choose "Negasonic's last interrupt" (isAbilityOfCard card)
        pass
        villainIsTough <$> getVillain `shouldReturn` False
        alliesWith "44044" `shouldReturn` []

    it "also answers a treachery revealed from the discard pile" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        let you = player1 tg
        clearHand you
        ally <- putIntoPlay you "44044"
        card <- encounterCard Treacheries.imTough
        runNow [RevealEncounterCardFrom W.RevealedFromDiscardPile you card]
        choose "Negasonic interrupt" (isAbilityOfCard ally)
        pass
        villainIsTough <$> getVillain `shouldReturn` False

    it "does not answer a revealed minion" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        let you = player1 tg
        clearHand you
        ally <- putIntoPlay you "44044"
        _ <- revealNow you Minions.hydraSoldier
        offers (\case UseAbility ab -> isAbilityOfCard ally (UseAbility ab) && abilityIndex ab == 1; _ -> False) `shouldReturn` False

  describe "Pandapool" do
    it "enters with tough, a hazard icon, three ATK and no consequential damage" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        card <- putInHand you "44045"
        payment <- payCards you 4
        playWith card payment
        attrs <- allyIn "44045"
        Ally.allyTough attrs `shouldBe` True
        getHazardCount `shouldReturn` 1
        choose "Pandapool attacks" (isAllyAttack attrs)
        chooseVillain
        pass
        villainDamageNow `shouldReturn` 3
        allyDamage <$> allyIn "44045" `shouldReturn` 0

  describe "Break Time" do
    it "costs three per player and heals every identity by the reported whole minutes" do
      tg <- testGame ["captain-marvel", "spider-man"]
      inGame tg do
        let you = player1 tg
            other = player2 tg
        clearHand you
        runNow [IdentityMessage other $ ChangedToForm A]
        setDamage you 5
        setDamage other 8
        card <- putInHand you "44046"
        payment <- payCards you 6
        playWith card payment
        choose "five minutes" (isLabel "$label.breakTimeFiveMinutes")
        choose "resume" (isLabel "$label.breakTimeResume")
        pass
        damageOf you `shouldReturn` 0
        damageOf other `shouldReturn` 3

    it "allows repeated hour increments without imposing a fixed minute cap" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        let you = player1 tg
        clearHand you
        setDamage you 7
        card <- putInHand you "44046"
        payment <- payCards you 3
        playWith card payment
        choose "one hour" (isLabel "$label.breakTimeSixtyMinutes")
        choose "another hour" (isLabel "$label.breakTimeSixtyMinutes")
        choose "another minute" (isLabel "$label.breakTimeOneMinute")
        choose "another fifteen minutes" (isLabel "$label.breakTimeFifteenMinutes")
        choose "resume after 136 minutes" (isLabel "$label.breakTimeResume")
        damageOf you `shouldReturn` 0

    it "cannot be played in hero form" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        card <- putInHand (player1 tg) "44046"
        _ <- payCards (player1 tg) 3
        offers (isPlay card) `shouldReturn` False

  describe "Get in Front of Me" do
    it "cancels the treachery, induces an attack, and draws when an ally defends that attack" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        defender <- putIntoPlay you "44045"
        card <- prepareFront you
        playFront you card
        choose "Pandapool defends" (\case AllyDefend aid _ -> aid == allyIdOf defender; _ -> False)
        pass
        villainIsTough <$> getVillain `shouldReturn` False
        damageOf you `shouldReturn` 0
        length <$> handOf you `shouldReturn` 1

    it "draws when another hero defends the induced attack" do
      tg <- testGame ["captain-marvel", "spider-man"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
            other = player2 tg
        runNow [IdentityMessage other $ ChangedToForm A]
        card <- prepareFront you
        playFront you card
        choose "ask Spider-Man to defend" (isTargetChoice other)
        choose "Spider-Man accepts" (isTargetChoice other)
        pass
        length <$> handOf you `shouldReturn` 1
        damageOf you `shouldReturn` 0

    it "does not draw for its own hero's defense or a later unrelated defended attack" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        card <- prepareFront you
        playFront you card
        choose "defend yourself" (\case Defend _ -> True; _ -> False)
        pass
        length <$> handOf you `shouldReturn` 0
        ally <- putIntoPlay you "44045"
        vid <- villainIdNow
        runNow [VillainMessage vid $ VillainAttacks you]
        choose "Pandapool defends the later attack" (\case AllyDefend aid _ -> aid == allyIdOf ally; _ -> False)
        pass
        length <$> handOf you `shouldReturn` 0

    it "cannot cancel a treachery revealed from the discard pile" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        event <- prepareFront you
        card <- encounterCard Treacheries.imTough
        runNow [RevealEncounterCardFrom W.RevealedFromDiscardPile you card]
        offers (isPlay event) `shouldReturn` False
        pass
        villainIsTough <$> getVillain `shouldReturn` True

  describe "Mulligan" do
    it "discards the remaining hand and draws to current hand size" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        _ <- putIntoPlay you "44024"
        card <- putInHand you "44048"
        discarded <- putInHand you "44045"
        payment <- payCards you 3
        playWith card payment
        pass
        length <$> handOf you `shouldReturn` 7
        discard <- unDiscard . playerIdentityDiscard <$> playerAttrs you
        (pcCardId discarded `elem` map pcCardId discard) `shouldBe` True

    it "cannot be played after another card in this phase" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        becomeHero tg
        let you = player1 tg
        _ <- putIntoPlay you "44015"
        first <- putInHand you "44020"
        playWith first []
        attrs <- allyIn "44015"
        choose "boost Kidpool" (isTargetChoice $ allyId attrs)
        card <- putInHand you "44048"
        _ <- payCards you 3
        offers (isPlay card) `shouldReturn` False

  describe "Deadpool Corps Ship" do
    it "exhausts and deals an encounter card to put a Pool ally from hand into play" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        let you = player1 tg
        clearHand you
        ship <- putIntoPlay you "44049"
        pool <- putInHand you "44045"
        other <- putInHand you "01083"
        before <- length . playerIdentityEncounterCards <$> playerAttrs you
        choose "use the Ship" (isAbilityOfCard ship)
        offers (isTargetChoice other) `shouldReturn` False
        choose "put Pandapool into play" (isTargetChoice pool)
        pass
        length . playerIdentityEncounterCards <$> playerAttrs you `shouldReturn` before + 1
        Ally.allyTough <$> allyIn "44045" `shouldReturn` True
        [attrs] <- supportsWith "44049"
        Support.supportExhausted attrs `shouldBe` True

    it "is not offered with only a duplicate unique ally or a non-Pool ally in hand" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        let you = player1 tg
        clearHand you
        ship <- putIntoPlay you "44049"
        _ <- putIntoPlay you "44045"
        _ <- putInHand you "44045"
        _ <- putInHand you "01083"
        offers (isAbilityOfCard ship) `shouldReturn` False

  describe "Plot Convenience" do
    it "stores only aspect cards facedown, up to three, and can return a stored card" do
      tg <- testGame ["captain-marvel"]
      inGame tg do
        let you = player1 tg
        clearHand you
        plot <- putIntoPlay you "44050"
        basic <- putInHand you "01083"
        signature <- putInHand you "01015"
        for_ ["44018", "01050", "44017"] $ \code -> do
          card <- putInHand you code
          choose "Plot Convenience" (isAbilityOfCard plot)
          choose "store" (isLabel "$label.plotConvenienceStore")
          offers (isTargetChoice basic) `shouldReturn` False
          offers (isTargetChoice signature) `shouldReturn` False
          choose "aspect card" (isTargetChoice card)
          resolve [SupportMessage (supportIdOf plot) ReadiedSupport]
        length <$> heldByPlot `shouldReturn` 3
        _ <- putInHand you "44019"
        choose "Plot Convenience full" (isAbilityOfCard plot)
        offers (isLabel "$label.plotConvenienceStore") `shouldReturn` False
        choose "retrieve" (isLabel "$label.plotConvenienceRetrieve")
        held <- heldByPlot
        let returned = fromMaybe (error "stored card") $ headMay held
        choose "take stored card" (isTargetChoice returned)
        length <$> heldByPlot `shouldReturn` 2
        hand <- handOf you
        (pcCardId returned `elem` map pcCardId hand) `shouldBe` True

    it "lets another player store from and retrieve into their own hand" do
      tg <- testGame ["captain-marvel", "spider-man"]
      inGame tg do
        let owner = player1 tg
            actor = player2 tg
        clearHand owner
        clearHand actor
        plot <- putIntoPlay owner "44050"
        card <- putInHand actor "44018"
        choose "owner ends their turn" (\case EndTurn -> True; _ -> False)
        keepCards
        pass
        choose "other player uses Plot Convenience" (isAbilityOfCard plot)
        choose "store" (isLabel "$label.plotConvenienceStore")
        choose "their aspect card" (isTargetChoice card)
        length <$> heldByPlot `shouldReturn` 1
        resolve [SupportMessage (supportIdOf plot) ReadiedSupport]
        choose "other player uses Plot Convenience again" (isAbilityOfCard plot)
        choose "retrieve" (isLabel "$label.plotConvenienceRetrieve")
        choose "stored card" (isTargetChoice card)
        hand <- handOf actor
        (pcCardId card `elem` map pcCardId hand) `shouldBe` True
        ownerHand <- handOf owner
        (pcCardId card `elem` map pcCardId ownerHand) `shouldBe` False
