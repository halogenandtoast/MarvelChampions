module Marvel.Aspect.CaptainAmericaPackSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Identity (getModifiedAllyLimit)
import Marvel.Window (Window (..), WindowTiming (..), WindowType (..))
import TestImport

-- | A card from hand paid for with the cards given, put into play.
inPlay :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
inPlay p code payment = do
  card <- putInHand p code
  cards <- traverse (putInHand p) payment
  playWith card cards
  pure card

flipTo :: IdentityId -> Side -> GameAppT ()
flipTo p side = resolve [IdentityMessage p (ChangedToForm side)]

-- | An ally put into play as if played, its response offered.
enters :: IdentityId -> CardCode -> GameAppT PlayerCard
enters p code = do
  card <- putInHand p code
  runNow [PutCardIntoPlay p card NoPayment Nothing]
  pure card

spec :: Spec
spec = describe "the Captain America pack's Leadership and basic cards" do
  describe "Falcon (03011)" do
    it "looks at the top 3 encounter cards, removing 1 threat for each treachery" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        placeMainThreat 5
        before <- mainSchemeThreat
        _ <- stackEncounterDeck [Treacheries.hailHydra, Minions.vulture, Treacheries.imTough, Treacheries.imTough]
        deck <- scenarioEncounterDeckNow
        falcon <- enters p "03011"
        choose "use Falcon" (isAbilityOfCard falcon)
        choose "done looking" (isLabel "$label.done")
        mainSchemeThreat `shouldReturn` (before - 2)
        -- the cards stay where they were
        scenarioEncounterDeckNow `shouldReturn` deck

  it "Squirrel Girl deals 1 damage to each enemy as she enters play" do
    tg <- testGame ["captain-america"]
    let p = player1 tg
    inGame tg do
      minion <- addMinion p Minions.madameHydra
      squirrel <- enters p "03013"
      choose "use Squirrel Girl" (isAbilityOfCard squirrel)
      villainDamageNow `shouldReturn` 1
      minionDamageOf minion `shouldReturn` Just 1

  describe "Wonder Man (03014)" do
    it "discards a card from hand to attack" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "03014"
        wonder <- allyIn "03014"
        offers (isAllyAttack wonder) `shouldReturn` False
        card <- putInHand p "03003"
        vid <- villainIdNow
        choose "attack" (isAllyAttack wonder)
        choose "discard" (isTargetChoice card)
        choose "attack Rhino" (isTargetChoice vid)
        pass
        villainDamageNow `shouldReturn` 3
        discardOf p `shouldReturn` [card]
        (.allyDamage) <$> allyIn "03014" `shouldReturn` 1

  describe "Avengers Assemble! (03015)" do
    it "readies the player's Avengers, and gives each Avenger in play +1 THW and +1 ATK for the phase" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        flipTo p A
        _ <- putIntoPlay p "03014"
        _ <- putIntoPlay p "03002"
        wonder <- allyIn "03014"
        agent <- allyIn "03002"
        resolve [IdentityMessage p ExhaustedIdentity, AllyMessage (allyId wonder) ExhaustedAlly, AllyMessage (allyId agent) ExhaustedAlly]
        _ <- inPlay p "03015" ["01088", "01089"]
        isExhaustedNow p `shouldReturn` False
        (.allyExhausted) <$> allyIn "03014" `shouldReturn` False
        -- Agent 13 is no Avenger
        (.allyExhausted) <$> allyIn "03002" `shouldReturn` True
        card <- putInHand p "03003"
        vid <- villainIdNow
        choose "attack" (isAllyAttack wonder)
        choose "discard" (isTargetChoice card)
        choose "attack Rhino" (isTargetChoice vid)
        pass
        villainDamageNow `shouldReturn` 4

    it "is played once a round" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        flipTo p A
        _ <- inPlay p "03015" ["01088", "01089"]
        card <- putInHand p "03015"
        _ <- traverse (putInHand p) ["01088", "01089"]
        offers (isPlay card) `shouldReturn` False

  it "Strength in Numbers exhausts any number of the player's allies to draw a card for each" do
    tg <- testGame ["captain-america"]
    let p = player1 tg
    inGame tg do
      clearHand p
      _ <- putIntoPlay p "03014"
      _ <- putIntoPlay p "03002"
      wonder <- allyIn "03014"
      agent <- allyIn "03002"
      _ <- inPlay p "03017" []
      choose "exhaust Wonder Man" (isTargetChoice (allyId wonder))
      -- with no ally left to exhaust, the cost is paid
      choose "exhaust Agent 13" (isTargetChoice (allyId agent))
      length <$> handOf p `shouldReturn` 2
      (.allyExhausted) <$> allyIn "03014" `shouldReturn` True

  describe "Quinjet (03019)" do
    it "puts an Avenger ally from hand with printed cost up to its time counters into play, then is discarded" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        quinjet <- putIntoPlay p "03019"
        squirrel <- putInHand p "03013"
        agent <- putInHand p "03002"
        offers (isAbilityOfCard quinjet) `shouldReturn` False
        runNow [CheckWindows [Window After (TurnBegan p)]]
        choose "add a time counter" (isAbilityOfCard quinjet)
        runNow [CheckWindows [Window After (TurnBegan p)]]
        choose "add a time counter" (isAbilityOfCard quinjet)
        choose "use the Quinjet" (isAbilityOfCard quinjet)
        -- Agent 13 is no Avenger
        offers (isTargetChoice agent) `shouldReturn` False
        choose "Squirrel Girl" (isTargetChoice squirrel)
        alliesWith "03013" >>= (`shouldSatisfy` ((== 1) . length))
        supportsWith "03019" `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` elem quinjet)

  describe "Avengers Tower (03024)" do
    it "raises the ally limit while each of the player's allies is an Avenger" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        _ <- putIntoPlay p "03024"
        (.playerIdentityAllyLimit) <$> playerAttrs p `shouldReturn` 3
        _ <- putIntoPlay p "03014"
        limit <- getModifiedAllyLimit =<< getPlayer p
        limit `shouldBe` 4
        _ <- putIntoPlay p "03002"
        limit' <- getModifiedAllyLimit =<< getPlayer p
        limit' `shouldBe` 3

    it "makes the next Avenger ally played this phase cost 1 less" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        flipTo p A
        tower <- putIntoPlay p "03024"
        choose "use the tower" (isAbilityOfCard tower)
        -- Squirrel Girl costs 2
        squirrel <- putInHand p "03013"
        payment <- putInHand p "03003"
        playWith squirrel [payment]
        pass
        alliesWith "03013" >>= (`shouldSatisfy` ((== 1) . length))
        wonder <- putInHand p "03014"
        _ <- putInHand p "03003"
        offers (isPlay wonder) `shouldReturn` False

  describe "Honorary Avenger (03025)" do
    it "gives an ally +1 hit point and the Avenger trait" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        flipTo p A
        _ <- putIntoPlay p "03002"
        agent <- allyIn "03002"
        honorary <- putInHand p "03025"
        playOn honorary (allyId agent) []
        resolve [AllyMessage (allyId agent) (AllyDamaged GameRef (toDamage 3 FromAbility))]
        -- 3 damage does not defeat her
        alliesWith "03002" >>= (`shouldSatisfy` ((== 1) . length))
        upgradeCodes `shouldReturn` ["03025"]

    it "an ally whose damage it held off is defeated as it leaves play" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        flipTo p A
        _ <- putIntoPlay p "03002"
        agent <- allyIn "03002"
        honorary <- putInHand p "03025"
        playOn honorary (allyId agent) []
        resolve [AllyMessage (allyId agent) (AllyDamaged GameRef (toDamage 3 FromAbility))]
        resolve [UpgradeMessage (upgradeIdOf honorary) DiscardUpgrade]
        alliesWith "03002" `shouldReturn` []

    it "an identity left with no hit points as it leaves play would be defeated" do
      tg <- testGame ["captain-america", "spider-man"]
      let p = player1 tg
      inGame tg do
        clearHand p
        flipTo p A
        helmet <- putIntoPlay p "03008"
        honorary <- putInHand p "03025"
        playOn honorary p []
        hitPointsOf p `shouldReturn` 12
        setDamage p 11
        runNow [UpgradeMessage (upgradeIdOf honorary) DiscardUpgrade]
        -- the helmet answers the defeat
        choose "use the helmet" (isAbilityOfCard helmet)
        damageOf p `shouldReturn` 10
        getsGame (HashMap.member p . gamePlayers) `shouldReturn` True

    it "is played only by an Avenger identity, once a character" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putInHand p "03025"
        _ <- putInHand p "03003"
        -- Steve Rogers is no Avenger
        offers (isPlay card) `shouldReturn` False
        flipTo p A
        offers (isPlay card) `shouldReturn` True
        -- Captain America is the one character to play it on
        _ <- inPlay p "03025" []
        hitPointsOf p `shouldReturn` 12
        -- no character is left without one
        offers (isPlay card) `shouldReturn` False

  describe "Enraged (03031)" do
    it "gives an ally +2 ATK and 1 more consequential damage after it attacks" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "03002"
        agent <- allyIn "03002"
        -- Agent 13 is the one ally to play it on
        _ <- inPlay p "03031" ["03003"]
        vid <- villainIdNow
        choose "attack" (isAllyAttack agent)
        choose "attack Rhino" (isTargetChoice vid)
        pass
        villainDamageNow `shouldReturn` 3
        (.allyDamage) <$> allyIn "03002" `shouldReturn` 2

    it "is discarded with the ally" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- putIntoPlay p "03002"
        agent <- allyIn "03002"
        enraged <- inPlay p "03031" ["03003"]
        resolve [AllyMessage (allyId agent) AllyDefeated]
        upgradeCodes `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` elem enraged)

  describe "Followed (03032)" do
    it "deals 4 damage to an enemy as the side scheme it is attached to is defeated" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        scheme <- revealNow p SideSchemes.crowdControl
        pass
        followed <- inPlay p "03032" ["03003"]
        runNow [SideSchemeMessage (sideSchemeOf scheme) (SideSchemeThwarted GameRef 20)]
        choose "use Followed" (isAbilityOfCard followed)
        vid <- villainIdNow
        chooseIf (isTargetChoice (EnemyVillainId vid))
        villainDamageNow `shouldReturn` 4
        upgradeCodes `shouldReturn` []
        discardOf p >>= (`shouldSatisfy` elem followed)

    it "needs a side scheme without one" do
      tg <- testGame ["captain-america"]
      let p = player1 tg
      inGame tg do
        clearHand p
        card <- putInHand p "03032"
        _ <- putInHand p "03003"
        offers (isPlay card) `shouldReturn` False

  it "Expert Defense gives the defending hero +3 DEF for the attack" do
    tg <- testGame ["captain-america"]
    let p = player1 tg
    inGame tg do
      clearHand p
      flipTo p A
      defense <- putInHand p "03033"
      -- a boost of 2: Rhino attacks for 4
      _ <- stackEncounterDeck [Minions.vulture]
      vid <- villainIdNow
      runNow [VillainMessage vid (VillainAttacks p)]
      choose "defend" isDefend
      choose "play Expert Defense" (isPlay defense)
      pass
      -- 4 ATK against 2 + 3 DEF
      damageOf p `shouldReturn` 0

  it "Enhanced Awareness pays 3 mental resources as the hero, then is discarded" do
    tg <- testGame ["captain-america"]
    let p = player1 tg
    inGame tg do
      clearHand p
      awareness <- putIntoPlay p "03034"
      helmet <- putInHand p "03008"
      offers (isPlay helmet) `shouldReturn` False
      flipTo p A
      choose "play the helmet" (isPlay helmet)
      choose "pay with awareness" (isAbilityOfCard awareness)
      chooseIf isFinishPayment
      upgradesWith "03034" >>= \case
        [u] -> u.upgradeUses `shouldBe` 2
        _ -> error "no Enhanced Awareness"
