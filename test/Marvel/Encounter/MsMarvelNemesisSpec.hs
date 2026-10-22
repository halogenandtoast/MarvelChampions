module Marvel.Encounter.MsMarvelNemesisSpec (spec) where

import Data.HashMap.Strict qualified as HashMap
import Data.List (sort)
import Marvel.Minion.Cards qualified as Minions
import Marvel.Phase (Phase (..))
import Marvel.SideScheme.Cards qualified as SideSchemes
import Marvel.Treachery.Cards qualified as Treacheries
import TestImport

msMarvel :: IO TestGame
msMarvel = testGame ["ms-marvel"]

damageMinion :: IdentityId -> MinionId -> Natural -> GameAppT ()
damageMinion p m n = resolve [MinionMessage m (MinionDamaged (toSource p) (toDamage n FromAbility))]

deckSizeOf :: IdentityId -> GameAppT Int
deckSizeOf p = length . unDeck . (.playerIdentityDeck) <$> playerAttrs p

spec :: Spec
spec = describe "Ms. Marvel nemesis" do
  describe "Shadow of the Past" do
    it "puts Thomas Edison, her nemesis minion, into play and shuffles the robot in" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Treacheries.shadowOfThePast
        pass
        getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameMinions) `shouldReturn` ["05027"]
        getsGame (map (toCardCode . getCardDef) . HashMap.elems . gameSideSchemes) `shouldReturn` ["05026"]
        sort . filter (`elem` ["05028", "05029"]) . map toCardCode <$> scenarioEncounterDeckNow
          `shouldReturn` ["05028", "05029", "05029"]

  describe "Generation Why? (05026)" do
    it "discards the top card of each player's deck for each ally and Persona support in play" do
      tg <- testGame ["ms-marvel", "black-panther"]
      inGame tg do
        [p1, p2] <- players
        _ <- putIntoPlay p1 "05002"
        _ <- putIntoPlay p1 "05006"
        -- not a Persona support
        _ <- putIntoPlay p2 "01091"
        before1 <- deckSizeOf p1
        before2 <- deckSizeOf p2
        scheme <- revealNow p1 SideSchemes.generationWhy
        pass
        deckSizeOf p1 `shouldReturn` (before1 - 2)
        deckSizeOf p2 `shouldReturn` (before2 - 2)
        -- 2 per player
        sideSchemeThreat (sideSchemeOf scheme) `shouldReturn` 4
        getHazardCount `shouldReturn` 1

  describe "Thomas Edison (05027)" do
    it "cannot take damage while you are engaged with another minion" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        edison <- addMinion p Minions.thomasEdison
        pass
        damageMinion p edison 1
        minionDamageOf edison `shouldReturn` Just 1
        _ <- addMinion p Minions.hydraMercenary
        pass
        damageMinion p edison 1
        minionDamageOf edison `shouldReturn` Just 1

  describe "Edison's Giant Robot (05028)" do
    it "cannot take damage, until a hero spends a mental resource to blank it for the phase" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        changeForm p
        clearHand p
        robot <- addMinion p Minions.edisonsGiantRobot
        pass
        damageMinion p robot 3
        minionDamageOf robot `shouldReturn` Just 0
        genius <- putInHand p "01089"
        choose "blank the robot" (isAbilityOf robot)
        choose "pay" (isPayWith genius)
        chooseIf isFinishPayment
        damageMinion p robot 3
        minionDamageOf robot `shouldReturn` Just 3
        offers (isAbilityOf robot) `shouldReturn` False
        resolve [EndPhase PlayerPhase]
        damageMinion p robot 3
        minionDamageOf robot `shouldReturn` Just 3

    it "is blanked by a hero only" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        robot <- addMinion p Minions.edisonsGiantRobot
        pass
        _ <- putInHand p "01089"
        offers (isAbilityOf robot) `shouldReturn` False

  describe "Harvest (05029)" do
    it "exhausts each ready Persona support, and the villain heals 1 for each" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        aamir <- putIntoPlay p "05006"
        bruno <- putIntoPlay p "05007"
        nakia <- putIntoPlay p "05008"
        resolve [SupportMessage (supportIdOf nakia) ExhaustedSupport]
        vid <- villainIdNow
        resolve [VillainMessage vid (VillainDamaged (toSource p) (toDamage 5 FromAbility))]
        _ <- revealNow p Treacheries.harvest
        pass
        villainDamageNow `shouldReturn` 3
        map (.supportExhausted) <$> traverse (fmap (fromMaybe (error "gone") . headMay) . supportsWith . toCardCode) [aamir, bruno]
          `shouldReturn` [True, True]

    it "surges when no Persona support is exhausted" do
      tg <- msMarvel
      let p = player1 tg
      inGame tg do
        [next] <- stackEncounterDeck [Minions.hydraMercenary]
        _ <- revealNow p Treacheries.harvest
        pass
        dealtTo p >>= (`shouldSatisfy` (ecCardId next `elem`) . map ecCardId)
