module Marvel.Encounter.VisionNemesisSpec (spec) where

import Marvel.Card.Id (CardId (..))
import Marvel.Hero.VisionSpec (changeMass, massForm)
import Marvel.Hero.Runner qualified as Hero
import Marvel.Identity (currentIdentity, PlayerIdentitySide (..))
import Marvel.Obligation.Cards qualified as Obligations
import Marvel.Environment.Cards qualified as Environments
import Marvel.Minion.Cards qualified as Minions
import Marvel.SideScheme.Cards qualified as Schemes
import Marvel.Treachery.Cards qualified as Treacheries
import Marvel.Matchers hiding (ExhaustedAlly, ExhaustedIdentity)
import Marvel.Query
import Marvel.Trait (Trait (Drone))
import TestImport

obligationOf :: EncounterCard -> ObligationId
obligationOf = ObligationId . unCardId . ecCardId

spec :: Spec
spec = describe "Vision nemesis" do
  describe "Corrupted Programming" do
    it "blanks Intangible's restrictions without changing mass form or its permanent keyword" do
      tg <- testGame ["vision"]
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Obligations.corruptedProgramming
        pass
        toCardCode <$> massForm `shouldReturn` "26002"
        handSizeOf p `shouldReturn` 6
        changeForm p
        identityMatches IdentityCanAttack p `shouldReturn` True
        identityMatches IdentityCanDefend p `shouldReturn` True

    it "permits changing mass while blank but suppresses Dense stats and response" do
      tg <- testGame ["vision"]
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Obligations.corruptedProgramming
        pass
        changeForm p
        clearHand p
        changeMass p
        u <- massForm
        toCardCode u `shouldBe` "26002b"
        offers (isAbilityOf u) `shouldReturn` False
        length <$> handOf p `shouldReturn` 0
        currentIdentity <$> getPlayer p >>= \case
          HeroSide h -> Hero.getModifiedAttack (toAttrs h) `shouldReturn` 0
          _ -> error "expected Vision hero"

    it "exhausts the alter-ego and is removed from the game" do
      tg <- testGame ["vision"]
      let p = player1 tg
      inGame tg do
        card <- revealNow p Obligations.corruptedProgramming
        pass
        choose "remove Corrupted Programming" (isAbilityOf (ObligationRef $ obligationOf card))
        isExhaustedNow p `shouldReturn` True
        obligationsInPlay `shouldReturn` 0

  describe "Ultron Drones" do
    it "allows a facedown player card to become a 1 SCH/ATK/HP Drone" do
      tg <- testGame ["vision"]
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Environments.ultronDronesVision
        pass
        before <- selectList (MinionWithTrait Drone)
        resolve [PutTopOfDeckIntoPlayAsDrone p]
        after <- selectList (MinionWithTrait Drone)
        length after `shouldBe` length before + 1

  describe "Relentless Android" do
    it "discards two random cards when Ultron Drones is absent" do
      tg <- testGame ["vision"]
      let p = player1 tg
      inGame tg do
        clearHand p
        _ <- traverse (putInHand p) ["01088", "01089", "01090"]
        _ <- revealNow p Treacheries.relentlessAndroid
        pass
        length <$> handOf p `shouldReturn` 1

    it "puts two deck cards into play as Drones when the environment exists" do
      tg <- testGame ["vision"]
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Environments.ultronDronesVision
        pass
        before <- length <$> selectList (MinionWithTrait Drone)
        _ <- revealNow p Treacheries.relentlessAndroid
        pass
        length <$> selectList (MinionWithTrait Drone) `shouldReturn` before + 2

  describe "Ultron Unleashed" do
    it "finds the set-aside environment and gives each player one Drone" do
      tg <- testGame ["vision", "spider-man"]
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Schemes.ultronUnleashed
        pass
        selectAny (EnvironmentWithTitle "Ultron Drones") `shouldReturn` True
        for_ [p, player2 tg] \who ->
          length <$> selectList (MinionWithTrait Drone <> MinionEngagedWith (IdentityWithId who)) `shouldReturn` 1

  describe "Ultron" do
    it "makes a Drone when he attacks while Ultron Drones is in play" do
      tg <- testGame ["vision"]
      let p = player1 tg
      inGame tg do
        changeForm p
        _ <- revealNow p Environments.ultronDronesVision
        pass
        ultron <- revealNow p Minions.ultronNemesis
        pass
        before <- length <$> selectList (MinionWithTrait Drone)
        runNow [MinionMessage (MinionId $ unCardId $ ecCardId ultron) (MinionAttacks p)]
        choose "Ultron forced interrupt" (isAbilityOf $ MinionRef $ MinionId $ unCardId $ ecCardId ultron)
        pass
        length <$> selectList (MinionWithTrait Drone) `shouldReturn` before + 1

    it "does not make a Drone without the environment" do
      tg <- testGame ["vision"]
      let p = player1 tg
      inGame tg do
        changeForm p
        ultron <- revealNow p Minions.ultronNemesis
        pass
        runNow [MinionMessage (MinionId $ unCardId $ ecCardId ultron) (MinionAttacks p)]
        pass
        selectList (MinionWithTrait Drone) `shouldReturn` []

  describe "defeated facedown Drone" do
    it "returns its physical player card to its owner's discard pile" do
      tg <- testGame ["vision"]
      let p = player1 tg
      inGame tg do
        _ <- revealNow p Environments.ultronDronesVision
        pass
        top <- head . unDeck . playerIdentityDeck <$> playerAttrs p
        before <- selectList (MinionWithTrait Drone)
        resolve [PutTopOfDeckIntoPlayAsDrone p]
        drones <- filter (`notElem` before) <$> selectList (MinionWithTrait Drone)
        case drones of
          [mid] -> do
            resolve [MinionMessage mid (MinionDamaged GameRef $ toDamage 1 FromAbility)]
            discardOf p >>= (`shouldSatisfy` any ((== pcCardId top) . pcCardId))
          _ -> error "expected one newly created Drone"
