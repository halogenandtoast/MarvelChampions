module Marvel.Hero.IronheartSignatureSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Marvel.Game.Public (publicGameFor)
import Marvel.Identity (ironheartVersion)
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as Schemes
import TestImport

ironheartGame :: IO TestGame
ironheartGame = testGame ["ironheart"]

asHero :: IdentityId -> GameAppT ()
asHero p = changeForm p >> pass >> clearHand p

countersOf :: IdentityId -> GameAppT Natural
countersOf p = (.playerIdentityCounters) <$> playerAttrs p

levelTo :: IdentityId -> Natural -> GameAppT ()
levelTo p n = do
  runNow [PlaceCounters (toTarget p) (6 * (n - 1))]
  for_ [1 .. n - 1] $ \_ -> choose "level up" (isNamedAbility "Level Up!")

playPaying :: IdentityId -> CardCode -> [CardCode] -> GameAppT PlayerCard
playPaying p code resources = do
  card <- putInHand p code
  payment <- traverse (putInHand p) resources
  playWith card payment
  pure card

setMainThreat :: Natural -> GameAppT ()
setMainThreat n = do
  current <- mainSchemeThreatNow
  mid <- mainSchemeIdNow
  if n >= current
    then runNow [MainSchemeMessage mid (MainSchemePlaceThreat (n - current))]
    else runNow [MainSchemeMessage mid (MainSchemeThreatRemoved GameRef (current - n))]

spec :: Spec
spec = describe "Ironheart signature" do
  describe "Brawn (29004)" do
    it "cannot generate a resource while ready" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        asHero p
        brawn <- putIntoPlay p "29004"
        card <- putInHand p "29010"
        _ <- putInHand p "01090"
        choose "play" (isPlay card)
        offers (isAbilityOfCard brawn) `shouldReturn` False

    it "generates mental while exhausted, once per phase" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        asHero p
        brawn <- putIntoPlay p "29004"
        runNow [AllyMessage (allyIdOf brawn) ExhaustedAlly]
        card <- putInHand p "29010"
        choose "play" (isPlay card)
        choose "Brawn resource" (isAbilityOfCard brawn)
        chooseIf isFinishPayment
        another <- putInHand p "29011"
        _ <- putInHand p "01090"
        choose "play" (isPlay another)
        offers (isAbilityOfCard brawn) `shouldReturn` False

  describe "Fly Over (29005)" do
    it "removes three threat and gains one progress if threat remains" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        asHero p
        setMainThreat 5
        _ <- playPaying p "29005" ["01089"]
        chooseMainScheme
        mainSchemeThreatNow `shouldReturn` 2
        countersOf p `shouldReturn` 1

    it "gains two progress when removing the main scheme's last threat" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        asHero p
        setMainThreat 2
        _ <- playPaying p "29005" ["01089"]
        chooseMainScheme
        countersOf p `shouldReturn` 2

    it "gains two progress when its thwart defeats a side scheme" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        asHero p
        encounter <- revealNow p Schemes.bombScare
        pass
        let scheme = sideSchemeOf encounter
        _ <- playPaying p "29005" ["01089"]
        choose "side scheme" (isTargetChoice scheme)
        countersOf p `shouldReturn` 2

    it "confused cancels both the thwart and counter placement" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        asHero p
        setMainThreat 3
        runNow [IdentityMessage p IdentityConfused]
        _ <- playPaying p "29005" ["01089"]
        countersOf p `shouldReturn` 0
        mainSchemeThreatNow `shouldReturn` 3

  describe "Photon Beam (29006)" do
    it "deals four damage and gains one progress against a surviving villain" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        asHero p
        _ <- playPaying p "29006" ["01088"]
        chooseVillain
        villainDamageNow `shouldReturn` 4
        countersOf p `shouldReturn` 1

    it "gains two progress when its attack defeats a minion" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        asHero p
        minion <- addMinion p Minions.hydraBomber
        _ <- playPaying p "29006" ["01088"]
        choose "minion" (isTargetChoice minion)
        countersOf p `shouldReturn` 2

    it "stunned cancels both its attack and progress" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        asHero p
        runNow [IdentityMessage p IdentityStunned]
        _ <- playPaying p "29006" ["01088"]
        countersOf p `shouldReturn` 0
        villainDamageNow `shouldReturn` 0

  describe "New and Improved (29007)" do
    it "version one chooses one option" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        asHero p
        _ <- playPaying p "29007" ["01088", "01092"]
        choose "tough" (isLabel "$label.newAndImprovedTough")
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` True
        offers (isLabel "$label.newAndImprovedSearch") `shouldReturn` False

    it "version three chooses three different options" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        asHero p
        levelTo p 3
        runNow [RemoveStatus (toTarget p) ToughStatus, IdentityMessage p ExhaustedIdentity]
        _ <- playPaying p "29007" ["01088", "01092"]
        choose "tough" (isLabel "$label.newAndImprovedTough")
        offers (isLabel "$label.newAndImprovedTough") `shouldReturn` False
        choose "ready" (isLabel "$label.newAndImprovedReady")
        known <- stackDeck p ["29005"]
        choose "search" (isLabel "$label.newAndImprovedSearch")
        choose "Ironheart card" (isTargetChoice $ testHead known)
        exhaustedNow p `shouldReturn` False
        (.playerIdentityTough) <$> playerAttrs p `shouldReturn` True

  describe "Sector Scan (29008)" do
    it "costs two resources in version one" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        asHero p
        _ <- playPaying p "29008" ["01088"]
        view <- liftIO . publicGameFor (== p) =<< getGame
        case view of
          Object o -> KeyMap.lookup "encounterDeckTop" o `shouldSatisfy` maybe False (/= Null)
          _ -> error "expected public game object"

    it "costs zero in version three" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        asHero p
        levelTo p 3
        card <- putInHand p "29008"
        playWith card []
        ironheartVersion p `shouldReturn` 3

  describe "Stroke of Genius (29009)" do
    it "offers progress and a card when spent in alter-ego form" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        clearHand p
        ronnie <- putInHand p "29010"
        genius <- putInHand p "29009"
        choose "play" (isPlay ronnie)
        choose "pay Stroke" (isPayWith genius)
        choose "Stroke response" (isLabel "$label.strokeOfGenius")
        chooseIf isFinishPayment
        countersOf p `shouldReturn` 1
        length <$> handOf p `shouldReturn` 1

    it "can decline its response" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        clearHand p
        ronnie <- putInHand p "29010"
        genius <- putInHand p "29009"
        choose "play" (isPlay ronnie)
        choose "pay Stroke" (isPayWith genius)
        choose "decline" (isLabel "$label.noResponse")
        chooseIf isFinishPayment
        countersOf p `shouldReturn` 0

  describe "Ronnie Williams (29010)" do
    it "exhausts in alter-ego form to place progress" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        clearHand p
        ronnie <- putIntoPlay p "29010"
        choose "Ronnie" (isAbilityOfCard ronnie)
        choose "progress" (isLabel "$label.ronnieProgress")
        countersOf p `shouldReturn` 1
        offers (isAbilityOfCard ronnie) `shouldReturn` False

    it "can heal two damage instead and has no hero action" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        clearHand p
        ronnie <- putIntoPlay p "29010"
        setDamage p 3
        choose "Ronnie" (isAbilityOfCard ronnie)
        choose "heal" (isLabel "$label.ronnieHeal")
        damageOf p `shouldReturn` 1
        countersOf p `shouldReturn` 0

  describe "Tony Stark A.I. (29011)" do
    it "adds one of the top two to hand and discards the other" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        clearHand p
        ai <- putIntoPlay p "29011"
        cards <- stackDeck p ["01088", "01089"]
        choose "Tony Stark A.I." (isAbilityOfCard ai)
        choose "first card" (isTargetChoice $ testHead cards)
        map toCardCode <$> handOf p `shouldReturn` ["01088"]
        discard <- map toCardCode <$> discardOf p
        discard `shouldSatisfy` elem "01089"

  describe "Photon Blasters (29012)" do
    it "grants two hit points and deals the current version's non-attack damage" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        asHero p
        blasters <- putIntoPlay p "29012"
        hitPointsOf p `shouldReturn` 12
        levelTo p 3
        runNow [IdentityMessage p IdentityStunned]
        choose "blasters" (isAbilityOfCard blasters)
        chooseVillain
        villainDamageNow `shouldReturn` 3
        (.playerIdentityStunned) <$> playerAttrs p `shouldReturn` True

  describe "Propulsion Jets (29013)" do
    it "grants two hit points and removes version-scaled threat without thwarting" do
      tg <- ironheartGame
      inGame tg do
        let p = player1 tg
        asHero p
        jets <- putIntoPlay p "29013"
        hitPointsOf p `shouldReturn` 12
        levelTo p 2
        setMainThreat 5
        runNow [IdentityMessage p IdentityConfused]
        choose "jets" (isAbilityOfCard jets)
        chooseMainScheme
        mainSchemeThreatNow `shouldReturn` 3
        (.playerIdentityConfused) <$> playerAttrs p `shouldReturn` True
